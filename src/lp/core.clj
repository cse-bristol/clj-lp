(ns lp.core
  (:require [clojure.set :refer [map-invert]]
            [com.rpl.specter :as s]))

(def ^:dynamic *lp-vars* #{})

(defrecord Product [factors]) ;; {:x 2 :y 3} => x² × y³
(defrecord Sum [terms]) ;; {a 1 b 2} => a + 2b
(defrecord Program [sense objective vars constraints])
(defrecord Constraint [body lower upper]) ;; lower <= body <= upper, allowing nil for l and u
(defrecord Conjunction [body]) ;; AND x y z
(defrecord Abnormal [fn args]) ;; something which could not be normalized

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
    
    (vector? expr)   (first expr) ;; operator

    :else (throw (ex-info "Unknown expression-type" {:expr expr :c (class expr)
                                                     :vars *lp-vars*
                                                     }))))

(def ZERO (Sum. {:c 0.0}))
(def ONE  (Sum. {:c 1.0}))

(defn is-constant?
  ([x] (or (number? x)
           (and (instance? Sum x)
                (#{#{} #{:c}} (set (keys (:terms x)))))))
  
  ([x y]
   (or (and (number? x) (== y x))
       (and (instance? Sum x)
            (when-let [xv (-> x :terms (get :c))]
              (== y xv))
            (= 1 (count (:terms x)))))))

(defn is-one?  [x] (is-constant? x 1))
(defn is-zero? [x] (is-constant? x 0))

(defn simplify-product [p]
  (let [factors (remove (comp is-zero? second)
                        (:factors p))]
    (if (empty? factors)
      :c (Product. (into {} factors)))))

(defn is-logical? [x]
  (or (instance? Constraint x)
      (instance? Conjunction x)))

(defn constant-value [x]
  (or
   (cond
     (number? x) x

     (instance? Sum x)
     (-> x :terms (get :c 0)))
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

(defmethod norm-expr :number   [x] (Sum. {:c (double x)}))
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
                  (= :c ka) kb
                  (= :c kb) ka

                  (and (instance? Product ka)
                       (instance? Product kb))
                  (Product. (merge-with + (:factors ka) (:factors kb)))

                  :else (throw (ex-info "Can't multiply these" {:a ka :b kb})))
                wc])
             (reduce ;; roll up any products that came out twice
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

;; (defmethod norm-expr :default  [[f & args]]
;;   (Sum. {(Abnormal. f (doall (map norm-expr args))) 1}))


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
    (binding [*lp-vars* (:vars lp)]
      (let [tidy-constraints
            #(if (map? %) %
                 (into {} (map-indexed vector %)))

            {obj :objective min :minimize max :maximize
             subject-to :subject-to
             constraints :constraints}
            lp
            lp (cond-> lp (not obj)
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

            lp (->> lp
                    (s/transform [:vars s/MAP-VALS] add-bounds)
                    (s/transform [:constraints s/MAP-VALS]
                                 (fn [c]
                                   (if (and (seq? c) (not (vector? c)))
                                     (norm-expr [:and c])
                                     (norm-expr c)
                                     )))
                    
                    (s/transform [:objective] norm-expr))
            ]
        ;; Convert to normalized program record
        (map->Program lp)))))

(defmulti linear? class)

(defmethod linear? Number [x] true)
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

(defn merge-results [lp result]
  (-> lp
      (update :vars
              #(merge-with merge %1 %2) (:vars result))
      (assoc :solution (:solution result))))

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

(defn linear-variable [p] (first (keys (:factors p))))

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

;; (defmethod print-method Sum [x ^java.io.Writer w]
;;   (print-method
;;    `[:S ~@(for [[t m] (:terms x) :when (not (is-zero? m))]
;;             (cond
;;               (= :c t) m
;;               (is-one? m) t
;;               :else [:* m t])
;;             )]
;;    w))


;; (defmethod print-method Product [x ^java.io.Writer w]
;;   (print-method
;;    (if (= 1 (count (:factors x)))
;;      (let [[x n] (first (:factors x))]
;;        (if (is-one? n) x [:** x n]))
;;      `[:P ~@(for [[x n] (:factors x)]
;;               (if (is-one? n) x [:** x n]))])
;;    w))


;; (binding [*lp-vars* {:x {} :y {}}]
;;   (norm-exprs
;;    (list
;;     [:+ (for [v [:x :y] i (range 2)]
;;           [:* v (inc i)]
;;           )])
;;    ))

