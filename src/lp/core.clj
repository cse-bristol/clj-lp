(ns lp.core
  (:require [clojure.set :refer [map-invert]]
            [com.rpl.specter :as s]
            [clojure.math.combinatorics :refer [cartesian-product]]
            [clojure.spec.alpha :as spec]
            [lp.dsl]))

(def ^:dynamic *lp-vars* {})
(def ^:dynamic *raw-vars* {})

(defrecord Product [factors]) ;; {:x 2 :y 3} => x² × y³
(defrecord Sum [terms]) ;; {a 1 b 2} => a + 2b
(defrecord Program [sense objective vars constraints])
(defrecord Constraint [body lower upper]) ;; lower <= body <= upper, allowing nil for l and u
(defrecord Conjunction [body]) ;; AND x y z
(defrecord Abnormal [fn args]) ;; something which could not be normalized

(def ^:private ^:constant common-operators
  {+ :+ * :* - :- / :/ < :< > :> = := <= :<= >= :>=})

(defn expression-type [expr]
  (cond
    (number? expr)    :number
    (*lp-vars* expr)  :variable
    (nil? expr)       :boolean
    (boolean? expr)   :boolean

    (instance? Product expr)  :product
    (instance? Abnormal expr) :abnormal

    (or (instance? Sum expr)
        (instance? Constraint expr)
        (instance? Conjunction expr)) :identity

    (vector? expr)
    ;; a grim hack to make using operators more convenient
    ;; 
    (get common-operators (first expr) (first expr))

    :else (throw (ex-info "Unknown expression-type" {:expr expr ::c (class expr)}))))

(def ZERO (Sum. {::c 0.0}))
(def ONE  (Sum. {::c 1.0}))

(defn is-constant?
  ([x] (or (number? x)
           (and (instance? Sum x)
                (#{#{} #{::c}} (set (keys (:terms x)))))))
  
  ([x y]
   (or (and (number? x) (== y x))
       (and (instance? Sum x)
            (when-let [xv (-> x :terms (get ::c))]
              (== y xv))
            (= 1 (count (:terms x)))))))

(defn is-one?  [x] (is-constant? x 1))
(defn is-zero? [x] (is-constant? x 0))

(defn simplify-product [p]
  (if (= ::c p)
    p
    (let [factors (remove (comp is-zero? second) (:factors p))]
      (if (empty? factors)
        ::c
        (Product. (into {} factors))))))

(defn is-logical? [x]
  (or (instance? Constraint x)
      (instance? Conjunction x)))

(defn constant-value [x]
  (or
   (cond
     (number? x) x

     (instance? Sum x)
     (-> x :terms (get ::c 0)))
   0))

(defmulti norm-expr expression-type)

(defn norm-exprs [exprs]
  (doall
   (reduce
    (fn [a e]
      (if (and (seq? e) (not (vector? e)))
        (concat a (norm-exprs e))
        (conj a (norm-expr e))))
    [] exprs)))

(defmethod norm-expr :number   [x] (Sum. {::c (double x)}))
(defmethod norm-expr :boolean  [x] (norm-expr (if x 1 0)))
(defmethod norm-expr :variable [x]
  (let [v (get *lp-vars* x)]
    (if (or (:fixed v)
            (and (:lower v)
                 (:upper v)
                 (== (:lower v) (:upper v))))
      ;; Constant
      (norm-expr (:value v (:lower v)))
      ;; v^1
      (Sum. {(Product. {x 1}) 1}))))
(defmethod norm-expr :product  [x] (Sum. {x 1})) 
(defmethod norm-expr :identity [x] x)
(defmethod norm-expr :abnormal [x] (Sum. {(Product. {x 1}) 1})) ;; x^1 × 1

(defmethod norm-expr :*        [[_ & args]]
  (doall
   (reduce
    (fn [acc val]
      (cond
        (or (is-zero? val) (is-zero? acc)) ZERO
        (is-one? val)  acc
        (is-one? acc)  val
        
        (instance? Sum val)
        (->> (for [[ka wa] (:terms acc)
                   [kb wb] (:terms val)
                   :let [wc (* wa wb)]
                   :when (not (zero? wc))]
               [(cond
                  (= ::c ka) kb
                  (= ::c kb) ka

                  (and (instance? Product ka)
                       (instance? Product kb))
                  (Product. (merge-with + (:factors ka) (:factors kb)))

                  :else (throw (ex-info "Can't multiply these" {:a ka :b kb})))
                wc])
             ;; now we have the cross product, we need to
             ;; sum up any duplicates
             (reduce
              (fn [acc [term mul]]
                (if (zero? mul)
                  acc
                  (let [term (if (instance? Product term)
                               (simplify-product term) ;; in case it's now actually just 1
                               term)]
                    (assoc acc term (+ mul (get acc term 0))))))
              {})
             (Sum.))
        :else
        (throw (ex-info "Unable to mul" {:val val}))))
    ONE
    (norm-exprs args))))

(defmethod norm-expr :+        [[_ & args]]
  (doall
   (reduce
    (fn [acc val]
      (cond
        (is-zero? val) acc
        (is-zero? acc) val

        (instance? Sum val)
        (Sum. (->> (merge-with + (:terms acc) (:terms val))
                   (filter (comp not zero? second))
                   (into {})))

        :else (throw (ex-info "Unable to add" {:val val}))))
    ZERO
    (norm-exprs args))))

(defmethod norm-expr :-        [[_ & args]]
  (let [[f & r] (norm-exprs args)]
    (cond
      (seq r)
      (norm-expr [:+ f [:- `[:+ ~@r]]])

      (and f (instance? Sum f))
      (->> (doall
            (for [[k w] (:terms f)
                  :when (not (zero? w))]
              [k (- w)]))
           (into {})
           (Sum.))

      (and (not f) (not r))
      ZERO

      :else (throw (ex-info "Unable to negate" {:f f :r r}))
      )))

(defmethod norm-expr :/        [[_ n d & args]]
  (when-not (empty? args) (throw (ex-info "/ only works with 2 arguments")))
  (let [n (norm-expr n)
        d (norm-expr d)]
    (cond
      (and (is-zero? n) (is-zero? d))
      (norm-expr ##NaN)
      
      (is-zero? d)
      (norm-expr ##Inf)
      
      (= n d) (norm-expr 1)
      
      (is-constant? d)
      (norm-expr [:* n (/ 1.0 (constant-value d))])

      ;; here we could do x/x => 1
      
      :else (throw (ex-info "Nonlinear division not implemented, sorry"))))
  
  ;; / doesn't distribute over +, so we can't do much here
  ;; special cases we can handle are
  ;; constant / constant
  ;; sumexpr / constant
  ;; constant / product
  ;; product / product
  ;; anything else has to become Abnormal
  )

(defmethod norm-expr :>=       [[_ & args]]
  (norm-expr `[:<= ~@(reverse args)]))

(defmethod norm-expr :<=       [[_ & args]]
  (let [args (norm-exprs args)]
    (cond
      (= 2 (count args))
      ;; so here we have a <= b
      ;; so a - b <= 0
      ;; blah + k <= 0
      ;; blah <= -k
      ;; blah >= k
      ;; TODO this could be nicer.
      (let [delta (norm-expr `[:- ~@args])
            k (constant-value delta)
            delta (norm-expr `[:- ~delta ~k])
            ]
        (Constraint. delta nil (- k)))
      
      (= 3 (count args))
      (let [[lb mid ub] args]
        (cond
          (and (is-constant? lb)
               (is-constant? ub))
          (let [constant-term (constant-value mid)]
            (Constraint. (norm-expr `[:- ~mid ~constant-term])
                         (- (constant-value lb) constant-term)
                         (- (constant-value ub) constant-term)))
              
          :else
          (norm-expr `[:and
                       [:<= ~lb ~mid]
                       [:<= ~mid ~ub]])))

      :else (throw (ex-info "Constraints can only be < x y or < l x u"))
      )))

(defmethod norm-expr :and      [[_ & args]]
  ;; several constraints which are all true
  (let [args (norm-exprs args)]
    (if (every? is-logical? args)
      (Conjunction.
       (reduce
        (fn [acc arg]
          (cond
            (instance? Constraint arg)
            (conj acc arg)

            ;; fold up other conjunctions
            (instance? Conjunction arg)
            (concat acc (:body arg))))
        nil
        args))
      (throw (ex-info ":and only applicable to constraints or other ands" {:args args})))))

(defmethod norm-expr :=        [[_ & args]]
  ;; for our two expressions to be equal, they must sum to zero
  (let [expr (norm-expr `[:- ~@args])
        k (constant-value expr) ;; extract constant term
        expr (norm-expr `[:- ~expr ~k]) ;; a bit lazy, but take it off.
        ]
    (Constraint. expr (- k) (- k))))

(defmethod norm-expr :default  [e]
  (throw
   (if-let [possible-var (get *raw-vars* (first e))]
     (ex-info "Index outside range for variable" {:variable (first e) :index (rest e)})
     (ex-info "Unsupported operator in expression, or undefined variable" {:expression e}))))

(let [as-fn-n (fn [x]
                (cond
                  (map? x) #(get x (vec %))
                  (fn? x)  #(apply x %) ;; convenience or terrible idea?
                  :else    (constantly x)))
      as-fn-1  (fn [x]
                 (cond
                   (map? x) #(get x %)
                   (fn? x)  #(apply x %)
                   :else    (constantly x)))
      ]
  (defn expand-indices
    "In the normalized form of the program, we want to turn
  :vars {:x {:indexed-by [#{:a :b}] :value some-fn}}
  into
  :vars {[:x :a] {:value (some-fn :a)}
         [:x :b] {:value (some-fn :b)}}

  The :value, :lower, :upper, and :fixed parts can support three
  different forms:

  1. If it's a map, it is taken to be a map from an index to a value
     There's a special case where the index always has one element,
     so you can write:
     {:x {:indexed-by [#{:a :b}] :value {:a 1 :b 2}}}
     and
     {:x {:indexed-by [#{:a :b} #{:c :d}] :value {[:a :c] 1 [:b :c] 2, ...}}}
     point being in the first case we have written :a rather than [:a].
  2. If it's fn?, it's called with the index as its arguments
  3. Otherwise it's taken as a constant and repeated everywhere
  "
    [vars]
    (reduce-kv
     (fn [vars k v]
       (if-let [indices (:indexed-by v)]
         (let [singular-index (= 1 (count indices))
               as-fn (if singular-index as-fn-1 as-fn-n)
               
               v (dissoc v :indexed-by)
               value (as-fn (:value v))
               lower (as-fn (:lower v))
               upper (as-fn (:upper v))
               fixed (as-fn (:fixed v))
               ]
           (->> (for [index (apply cartesian-product indices)]
                  [`[~k ~@index]
                   (-> v
                       (assoc :value (value index)
                              :lower (lower index)
                              :upper (upper index)
                              :fixed (fixed index)))])
                (into {})
                (merge vars)))
         (assoc vars k v)))
     {} vars)))

(defn add-bounds [var]
  (cond-> var
    (= (:type var) :non-negative)
    (update :lower (fn [l] (max (or l 0) 0)))
    
    (= (:type var) :binary)
    (assoc :lower 0 :upper 1)))

(defn normalize
  "Given `lp`, return a normalized form of it, unless already done.

  In the normalized form:
  - Variable names are the same
  - The objective and constraints are, where possible, transformed into a sumproduct
  - Constraints are not fixed up for canonical form
  "
  [lp]
  
  (if (instance? Program lp)
    lp
    (if (spec/valid? :lp/program lp)
      (binding [*raw-vars* (:vars lp)]
        (let [tidy-constraints
              #(if (map? %) %
                   (into {} (map-indexed vector %)))
              
              {obj :objective min :minimize max :maximize
               subject-to :subject-to
               constraints :constraints}
              lp
              lp (cond->
                     lp (not obj)
                     (cond-> 
                         min (-> (assoc :sense :minimize :objective min)
                                 (dissoc :minimize))
                         max (-> (assoc :sense :maximize :objective max)
                                 (dissoc :maximize))
                         constraints (update :constraints tidy-constraints)
                         subject-to  (-> (update :constraints merge
                                                 (tidy-constraints subject-to))
                                         (dissoc :subject-to))
                         ))

              lp (-> lp
                     (->> (s/transform [:vars s/MAP-VALS] add-bounds))
                     (update :vars expand-indices)) ;; simplifies vars which are an xprod
              
              lp (binding [*lp-vars* (:vars lp)]
                   (->> lp
                        (s/transform [:constraints s/MAP-VALS]
                                     (fn [c]
                                       (if (and (seq? c) (not (vector? c)))
                                         (norm-expr [:and c])
                                         (norm-expr c)
                                         )))
                        
                        (s/transform [:objective] norm-expr)))
              ]
          ;; Convert to normalized program record
          (map->Program lp)))
      (throw (ex-info
              "Invalid linear program"
              {:explain-data (spec/explain-data :lp/program lp)
               :explain-message (spec/explain-str :lp/program lp)})))))

(defmulti linear? class)

(defmethod linear? Number [x] true)
(defmethod linear? clojure.lang.Keyword [c] (= c ::c))

(defmethod linear? Sum [x] (every? linear? (keys (:terms x))))
(defmethod linear? Product [x] ;; TODO special case for single variable?
  (let [by-exponent (group-by (comp double second) (:factors x))
        exponents (conj (set (keys by-exponent))
                        1.0 0.0)]

    (and (= exponents #{1.0 0.0})
         (< (count (get by-exponent 1.0)) 2))))

(defmethod linear? Conjunction [x]
  (every? linear? (:body x)))

(defmethod linear? Constraint [x]
  (linear? (:body x)))

(defmethod linear? Program [x]
  (and (linear? (:objective x))
       (every? linear? (vals (:constraints x)))))

(defmethod linear? :default [_] false)

(defn merge-results
  "Add results back onto an lp.
  If the original lp is not normalized it might have vars that are :indexed-by
  The :value for these should be of the form {[index] value ...}
  "
  [lp result]

  (let [indexed-vars (set
                      (filter
                       (comp seq :indexed-by (:vars lp))
                       (keys (:vars lp))))

        result-vars
        (reduce-kv
         (fn [vars k v]
           (if
             (and (vector? k) (indexed-vars (first k)))
             (let [var (first k)
                   index (vec (rest k))]
               (cond->
                   vars
                 (:value v)
                 (update-in [var :value] assoc index (:value v))
                 (:dual-value v)
                 (update-in [var :dual-value] assoc index (:dual-value v))
                 (:status v)
                 (update-in [var :status] assoc index (:status v))))

             (assoc vars k v)))
         {} (:vars result))
        
        ]
    (-> lp
        (update :vars #(merge-with merge %1 %2) result-vars)
        (assoc :solution (:solution result)))))

(defn constraint-bodies
  "Given a normalized LP, get all the constraint bodies, ignoring their names.
  Why do we have names?"
  [lp]

  (apply concat
         (for [[n c] (:constraints lp)]
           (cond
             (instance? Constraint c)
             [c]

             (instance? Conjunction c)

             (:body c)))))

(defn linear-variable [p]
  (if (= p ::c) p
      (first (keys (:factors p)))))

(defmulti linear-coefficients class)
(defmethod linear-coefficients Sum [sum]
  (reduce
   (fn [acc [term weight]]
     (when (linear? term)
       (let [term (simplify-product term)
             x    (linear-variable term)
             ]
         (assoc acc x (+ weight (get acc x 0))))))
   {} (:terms sum)))

(defmethod linear-coefficients Constraint [c]
  (linear-coefficients (:body c)))
