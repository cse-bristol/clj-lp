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

(defn- singleton? [coll] (and (seq coll) (empty? (rest coll))))

(defn expression-type [expr]
  (cond
    (number? expr)    :number
    
    (nil? expr)       :boolean
    (boolean? expr)   :boolean

    (instance? Product expr)  :product
    (instance? Abnormal expr) :abnormal

    (or (instance? Sum expr)
        (instance? Constraint expr)
        (instance? Conjunction expr)) :identity

    (*lp-vars* expr)  :variable
    
    (vector? expr)
    ;; a grim hack to make using operators more convenient
    ;; 
    (get common-operators (first expr) (first expr))

    :else (throw (ex-info "Unknown expression-type" {:expr expr ::c (class expr)}))))

(def ZERO (Sum. {::c 0.0}))
(def ONE  (Sum. {::c 1.0}))

(let [constant-sum?
      (fn [x]
        (let [terms (:terms x)]
          (or (empty? terms)
              (and (= 1 (count terms)) (contains? terms ::c)))))
      
      ]
  (defn is-constant?
    ([x]
     (or (number? x)
         (and (instance? Sum x)
              (constant-sum? x))))
    
    ([x y]
     (or (and (number? x) (== y x))
         (and (instance? Sum x)
              (constant-sum? x)
              (== y (::c (:terms x) 0)))))))

(defn is-one?  [x] (is-constant? x 1))
(defn is-zero? [x] (is-constant? x 0))

(defn simplify-product
  "Given a product, simplify it by removing any x^0 in it"
  [p]
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

(let [norm-exprs!
      (fn norm-exprs! [exprs acc]
        (reduce
         (fn [acc e]
           (if (and (seq? e) (not (vector? e)))
             (norm-exprs! e acc)
             (conj! acc (norm-expr e))))
         acc exprs))]
  
  (defn norm-exprs [exprs]
    (persistent! (norm-exprs! exprs (transient [])))))

(defmethod norm-expr :number   [x] (Sum. {::c (double x)}))
(defmethod norm-expr :boolean  [x] (norm-expr (if x 1.0 0.0)))
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
    (Conjunction.
     (persistent!
      (reduce
       (fn [acc arg]
         (cond
           (instance? Constraint arg)
           (conj! acc arg)

           ;; fold up other conjunctions
           (instance? Conjunction arg)
           (reduce conj! acc (:body arg))

           (nil? arg)
           acc

           :else
           (throw (ex-info "Non-logical argument to :and" {:arg arg}))
           ))
       (transient [])
       args)))))

;; (defmethod norm-expr :and      [[_ & args]]
;;   ;; several constraints which are all true
;;   (p :norm-and
;;      (let [args (norm-exprs args)]
;;        (Conjunction.
;;         (reduce
;;          (fn [acc arg]
;;            (cond
;;              (instance? Constraint arg)
;;              (conj acc arg)

;;              ;; fold up other conjunctions
;;              (instance? Conjunction arg)
;;              (into acc (:body arg))

;;              (nil? arg)
;;              acc

;;              :else
;;              (throw (ex-info "Non-logical argument to :and" {:arg arg}))
;;              ))
;;          []
;;          args)))))


(defmethod norm-expr ::lower [[_ v]]
  (norm-expr (:lower (get *lp-vars* v) (- Double/MAX_VALUE))))

(defmethod norm-expr ::upper [[_ v]]
  (norm-expr (:upper (get *lp-vars* v) Double/MAX_VALUE)))

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
                  (nil? x) nil
                  (map? x) #(get x (vec %))
                  (fn? x)  #(apply x %) ;; convenience or terrible idea?
                  :else    (constantly x)))
      as-fn-1  (fn [x]
                 (cond
                   (nil? x) nil
                   (map? x) #(get x (first %))
                   (fn? x)  #(apply x %)
                   :else    (constantly x)))
      ]
  (defn expand-indices
    "In the normalized form of the program, we want to turn
  :vars {:x {:indexed-by [#{:a :b}] :value some-fn}}
  into
  :vars {[:x :a] {:value (some-fn :a)}
         [:x :b] {:value (some-fn :b)}}

  :indexed-by can take two forms:
  1. A vector of sets:
     In this form, the indexes used will be the cartesian product of the sets
  2. A set of vectors
     In this form, the indexed used will be whatever is in the set.
     Every index should probably have the same length for this to work right.
  
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
       (if-let [indices (:indexed-by v)

                ;; if indices is a vector of sets, we want to multiply them out
                ;; if indices is a set of vectors, this has already been done
                
                ]
         (let [[indices singular-index]
               (cond
                 (and (vector? indices)
                      (every? set? indices))
                 [(apply cartesian-product indices)
                  (singleton? indices)]

                 (and (set? indices)
                      (every? vector? indices))
                 [indices
                  (singleton? (first indices))]
                 
                 :else
                 (throw (ex-info
                         ":indexed-by should be either a vector of sets or a set of vectors" {:var k}
                         ))
                 )
               as-fn (if singular-index as-fn-1 as-fn-n)
               value (as-fn (:value v))
               lower (as-fn (:lower v))
               upper (as-fn (:upper v))
               fixed (as-fn (:fixed v))
               v (dissoc v :indexed-by :value :lower :upper :fixed)
               ]
           (->> (for [index indices]
                  [`[~k ~@index]
                   (cond-> v
                     value (assoc :value (value index))
                     lower (assoc :lower (lower index))
                     upper (assoc :upper (upper index))
                     fixed (assoc :fixed (fixed index)))])
                (into {})
                (merge vars)))
         (assoc vars k v)))
     {} vars)))

(defn add-bounds [var]
  (try (cond-> var
         (= (:type var) :non-negative)
         (update :lower (fn [l] (max (or l 0) 0)))
         
         (= (:type var) :binary)
         (assoc :lower 0 :upper 1))
       (catch Exception e
         (throw (ex-info "Adding bounds to var" {:var var} e))
         )))

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
                     (update :vars expand-indices)
                     (->> (s/transform [:vars s/MAP-VALS] add-bounds)))

              lp (binding [*lp-vars* (:vars lp)]
                   (->> lp
                        (s/setval [:constraints s/MAP-VALS nil?] s/NONE)
                        (s/transform [:constraints s/MAP-VALS]
                                     (fn [c]
                                       (let [result
                                             (if (and (seq? c) (not (vector? c)))
                                               (norm-expr [:and c])
                                               (norm-expr c)
                                               )]
                                         (when-not
                                             (or (instance? Constraint result)
                                                 (instance? Conjunction result))
                                           (throw
                                            (ex-info "Constraint not a constraint"
                                                     {:input c
                                                      :result result})))
                                         result
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

(defn collapse-indices
  "Output vars have had their indices blown up, whereas input vars
  may (a) have index set and (b) be defined by functions

  We need to (a) roll up the indices of output vars, and (b) copy the
  values of any fixed variables in the input.
  "
  [input-vars output-vars]
  (let [indexed-vars    (set (for [[k v] input-vars :when (seq (:indexed-by v))] k))
        ;; TODO this is a bit wasteful, as it's already been done
        ;; once. We could change the API to add results of each pass
        ;; onto the program I guess, so all operations can be chained up.
        expanded-inputs (expand-indices input-vars)

        ;; copy across what we know
        output-vars (merge-with merge expanded-inputs output-vars)
        ]
    (reduce-kv
     (fn [vars k v]
       (if (and (vector? k) (indexed-vars (first k)))
         (let [var   (first k)
               index (rest k)
               index (if (singleton? index)
                       (first index)
                       (vec index))
               ]
           (cond->
               vars
             (:value v)
             (update-in [var :value] assoc index (:value v))
             
             (:dual-value v)
             (update-in [var :dual-value] assoc index (:dual-value v))
             
             (:status v)
             (update-in [var :status] assoc index (:status v))

             (:fixed v)
             (update-in [var :fixed] assoc index (:fixed v))

             (:upper v)
             (update-in [var :upper] assoc index (:upper v))

             (:lower v)
             (update-in [var :lower] assoc index (:lower v))
             ))

         (assoc vars k v)))
     {} output-vars)))

(defn fix-var-types [vars]
  (->> vars
       (s/transform
        [s/MAP-VALS
         (s/selected? [:type #{:binary :integer}])
         (s/collect-one [:type])
         :value]
        (fn fix-value [type value]
          (cond
            (map? value)
            (s/transform [(s/putval type) s/MAP-VALS] fix-value value)
            
            (and (number? value) (= :binary type))
            (if (> value 0.1) true false)
            
            (and (number? value) (= :integer type))
            (int value)

            true value)))))

(defn merge-results
  "Add results back onto an lp.
  If the original lp is not normalized it might have vars that are :indexed-by
  The :value for these should be of the form {[index] value ...}
  "
  [lp result]

  (let [result-vars (collapse-indices (:vars lp) (:vars result))]
    (-> lp
        (update :vars #(fix-var-types
                        (merge-with merge %1 %2)) result-vars)
        (assoc :solution (:solution result)))))

(defn constraint-bodies
  "Given a normalized LP, get all the constraint bodies, ignoring their names.
  Why do we have names?"
  [lp]
  (reduce
   (fn [a c]
     (cond
       (instance? Constraint c)
       (conj a c)

       (instance? Conjunction c)
       (into a (:body c))

       (nil? c)
       a
       
       :else
       (throw (ex-info "Unexpected non-constraint object in constraint list!"
                       {:value c
                        :meta (meta c)
                        }
                       ))
       ))
   []
   (vals (:constraints lp))))

(defn nontrivial-constraint-bodies
  "Like `constraint-bodies`, with trivial constraints removed"
  [lp]
  (remove
   (fn [{:keys [body lower upper]}]
     (or (and (nil? lower) (nil? upper)) (is-constant? body)))
   (constraint-bodies lp)))

(defn linear-variable [p]
  (if (= p ::c) p
      (first (keys (:factors p)))))

(defmulti linear-coefficients class)
(defmethod linear-coefficients Sum [sum]
  (reduce
   (fn [acc [term weight]]
     (if (and (linear? term) (not (zero? weight)))
       (let [term (simplify-product term)
             x    (linear-variable term)]
         (assoc acc x (+ weight (get acc x 0))))))
   {} (:terms sum)))

(defmethod linear-coefficients Constraint [c]
  (linear-coefficients (:body c)))

(defn trivial-checks
  "Given a normalized LP, do some trivial checks to make sure it's OK"
  [lp]

  (let [invalid-vars
        (keep
         (fn [[v {lb :lower ub :upper}]]
           (cond
             (not (and lb ub)) [v "Missing lower or upper bound" lb ub]
             (not (<= lb ub))  [v "Impossible bounds" lb ub]))
         (:vars lp))

        invalid-cons
        (keep
         (fn [{b :body l :lower u :upper}]
           (let [cv (and (is-constant? b) (constant-value b))]
             (when (or (and l u (> l u))
                       (and cv
                            (and l (> l cv))
                            (and u (< u cv))))
               ["A constraint has impossible bounds" l b u])))
         (constraint-bodies lp))

        problems (concat invalid-vars invalid-cons)
        ]
    problems))

