(ns lp.core
  (:require [clojure.set :refer [map-invert]]))

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

(def ZERO (Sum. {1 0.0}))
(def ONE  (Sum. {1 1.0}))

(defn is-constant?
  ([x] (or (number? x)
           (instance? Sum x)
           (= #{1} (set (keys (:terms x))))
           (empty? (keys (:terms x)))))
  
  ([x y]
   (or (and (number? x) (== y x))
       (and (instance? Sum x)
            (when-let [xv (-> x :terms (get 1))]
              (== y xv))
            (= 1 (count (:terms x)))))))

(defn is-one?  [x] (is-constant? x 1))
(defn is-zero? [x] (is-constant? x 0))

(defn is-logical? [x]
  (or (instance? Constraint x)
      (instance? Conjunction x)))

(defn constant-value [x]
  (or
   (cond
     (number? x) x

     (instance? Sum x)
     (-> x :terms (get 1 0)))
   0))

(defmulti norm-expr expression-type)

(defmethod norm-expr :number   [x] (Sum. {1 (double x)}))
(defmethod norm-expr :boolean  [x] (norm-expr (if x 1 0)))
(defmethod norm-expr :variable [x]
  (let [v (get *lp-vars* x)]
    (if (:fixed v)
      ;; Constant
      (norm-expr (:value v))
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
                  (= 1 ka) kb
                  (= 1 kb) ka

                  (and (instance? Product ka)
                       (instance? Product kb))
                  ;; TODO x^0 = 1
                  (-> (Product. (merge-with + (:factors ka) (:factors kb))))
                  
                  :else (throw (ex-info "Can't multiply these" {:a ka :b kb})))
                wc])
             (into {})
             (Sum.))

        :else
        (throw (ex-info "Unable to mul" {:val val}))))
    ONE
    (map norm-expr args))))

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
    (map norm-expr args))))

(defmethod norm-expr :-        [[_ & args]]
  (let [[f & r] (doall (map norm-expr args))]
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
  (let [args (doall (map norm-expr args))]
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
  (let [args (doall (map norm-expr args))]
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
        args))
      (throw (ex-info ":and only applicable to constraints or other ands" {:args args})))))

(defmethod norm-expr :=        [[_ & args]]
  ;; for our two expressions to be equal, they must sum to zero
  (let [expr (norm-expr `[:- ~@args])
        k (constant-value expr) ;; extract constant term
        expr (norm-expr `[:- ~expr ~k]) ;; a bit lazy, but take it off.
        ]
    (Constraint. expr (- k) (- k))))

(defmethod norm-expr :default  [[f & args]]
  (Sum. {(Abnormal. f (doall (map norm-expr args))) 1}))

(defn add-bounds [var]
  (cond-> var
    (= (:type var) :non-negative)
    (update :lower (fn [l] (max (or l 0) 0)))
    
    (= (:type var) :binary)
    (assoc :lower 0 :upper 1)))

(defn normalize
  "Given `lp`, return a normalized form of it.

  In the normalized form:
  - Variable names are the same
  - The objective and constraints are, where possible, transformed into a sumproduct
  - Constraints are not fixed up for canonical form
  "
  [lp]
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
                  (s/transform [:constraints s/MAP-VALS] norm-expr)
                  (s/transform [:objective] norm-expr))
          ]
      ;; Convert to normalized program record
      (map->Program lp))))

(defmulti linear? class)

(defmethod linear? Number [x] true)
(defmethod linear? Sum [x] (every? linear? (keys (:terms x))))
(defmethod linear? Product [x]
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

(defn merge-results [lp vars]
  (update
   lp :vars
   #(merge-with merge %1 %2) ;; this will add :value if it was missing.
   vars))

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

