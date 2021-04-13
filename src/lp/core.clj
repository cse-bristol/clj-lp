;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns lp.core
  (:require [clojure.set :refer [map-invert]]
            [com.rpl.specter :as s]
            [clojure.math.combinatorics :refer [cartesian-product]]
            [clojure.spec.alpha :as spec]
            [lp.dsl]))

(defn- singleton? [coll] (and (seq coll) (empty? (rest coll))))

(defprotocol ILinear
  (constant-value [self])
  (constant? [self])
  (gradient [self]))

(defprotocol ILinearizable
  (linearize [self]))

(defprotocol ILogical
  (all-constraints [s]))

(defrecord Constraint [body lower upper]
  ILinearizable
  (linearize [x] x)

  ILogical
  (all-constraints [self]
    ;; this filters out constraints which are trivial
    (when (and (or lower upper)
               (not (and (constant? body)
                         (let [k (constant-value body)]
                           (<= (or k lower) k (or k upper))))))
      [self]))

  ILinear
  (constant-value [_] (constant-value body))
  (gradient [_] (gradient body))
  (constant? [_] (constant? body)))

(defrecord Conjunction [body]
  ILinearizable
  (linearize [x] x)
  
  ILogical
  (all-constraints [self]
    (loop [out (transient [])
           body body]
      (if (empty? body) (persistent! out)
          (let [[b & body] body]
            (recur (reduce conj! out (all-constraints b)) body))))))

(defrecord Program [sense objective vars constraints]
  ILogical
  (all-constraints [_]
    (loop [out  (transient [])
           cons constraints]
      (if (empty? cons) (persistent! out)
          (let [[con & cons] cons]
            (recur
             (reduce conj! out (all-constraints con))
             cons)))))

  ILinear
  (constant-value [_] (constant-value objective))
  (constant? [_] (constant? objective))
  (gradient [_] (gradient objective)))

(defrecord Linear [constant parts]
  ILinearizable
  (linearize [self] self)

  ILinear
  (constant-value [self] constant)
  (constant? [self] (or (empty? parts) (every? zero? (vals parts))))
  (gradient [self] parts))

(extend-protocol ILinear
  Number
  (constant-value [n] n)
  (constant? [n] true)
  (gradient [n] {}))

(def ^:dynamic *variables* {})

(defn lp-var [x]
  (if-let [v (get *variables* x)]
    (if (or (:fixed v)
            (and (:lower v)
                 (:upper v)
                 (== (:lower v) (:upper v))))
      (linearize (:value v (:lower v)))
      (Linear. 0 {x 1}))
    (throw (ex-info "Cannot linearize" {:x x}))))

(let [handle
      (fn handle [args out]
        (reduce
         (fn [out arg]
           (if (and (seq? arg) (not (vector? arg)))
             (handle arg out)
             (conj! out (linearize arg))))
         out args))

      handle-nn
      (fn handle-nn [args out]
        (reduce
         (fn [out arg]
           (cond
             (and (seq? arg) (not (vector? arg)))
             (handle-nn arg out)

             (not (nil? arg))
             (conj! out (linearize arg))

             :else
             out))
         
         out args))
      ]
  (defn linearize-args [vector]
    (persistent! (handle (rest vector) (transient []))))

  (defn linearize-non-nil-args [vector]
    (persistent! (handle-nn (rest vector) (transient []))))
  )

(defn- sprod [constant grad]
  (if (empty? grad)
    constant
    (Linear. constant grad)))

(defn sum [xs]
  (loop [xs     xs
         constant 0
         grad (transient {})]
    (if (empty? xs)
      (sprod constant (persistent! grad))
      (let [[x & xs] xs]
        (recur xs
               (+ constant (constant-value x))
               (if (constant? x)
                 grad
                 (reduce-kv
                  (fn [out x k]
                    (let [k' (+ k (get out x 0))]
                      (if (zero? k')
                        (dissoc! out x)
                        (assoc! out x k'))))
                  grad
                  (gradient x))
                 ))))))

(let [grad*
      (fn [grad k]
        ;; grad can't usefully be transient because we need its keys
        (persistent!
         (reduce-kv
          (fn [a x k1]
            (let [k' (* k1 k)]
              (if (zero? k')
                (dissoc! a x)
                (assoc! a x k'))))
          (transient {})
          grad)))]
  
  (defn product [xs]
    (loop [xs       xs
           constant 1
           grad     {}]
      (if (empty? xs)
        (sprod constant grad) ;; uck
        (let [[x & xs] xs]
          (cond
            (constant? x)
            (let [k (constant-value x)]
              (recur
               xs
               (* constant k)
               (grad* grad k)))

            (empty? grad)
            (recur xs
                   (* constant (constant-value x))
                   (grad* (gradient x) constant))
            
            :else
            (throw (ex-info "Nonlinear product" {:grad grad
                                                 :constant constant
                                                 :x x}
                            ))))))))

(defn divide [vals]
  (if (= 1 (bounded-count 2 vals))
    ;; it's 1/x which we can do if x is constant
    (let [val (first vals)]
      (if (constant? val)
        (Linear. (/ (constant-value val)) {})
        (throw (ex-info "Cannot take the reciprocal of a variable" {:variable val}))))

    ;; it's (first / (product rest))
    (let [[h & t] vals
          tprod   (product t)]
      (if (constant? tprod)
        (let [k (constant-value tprod)]
          (Linear.
           (/ (constant-value h) k)
           (persistent!
            (reduce-kv
             (fn [a x v] (assoc! a x (/ v k)))
             (transient {})
             (gradient h)))))
        (throw (ex-info "Cannot divide by a variable" {:variable tprod})))))
  
  ;; x/k  ~> (1/k) x
  ;; k1/k ~> k2
  ;; x/x ~> 1

  ;; in principle we could express k^-1 as a term, in case later we *k
  )

(defn sub [xs]
  (if (= 1 (bounded-count 2 xs))
    (let [x (first xs)]
      (Linear.
       (- (constant-value x))
       (persistent!
        (reduce-kv
         (fn [a k v] (assoc! a k (- v)))
         (transient {})
         (gradient x)))))

    (loop [constant (constant-value (first xs))
           grad      (transient (gradient (first xs)))
           xs       (rest xs)]
      (if (empty? xs)
        (sprod constant (persistent! grad))
        (let [[x & xs] xs]
          (recur
           (- constant (constant-value x))
           (reduce-kv
            (fn [out x k]
              (let [k' (- (get out x 0) k)]
                (if (zero? k')
                  (dissoc! out x)
                  (assoc! out x k'))))
            grad (gradient x))
           xs))))))

;; these are for logical expressions, which are a bit different

(defn logand [input vals-in]
  (if (= 1 (bounded-count 2 vals-in))
    (first vals-in)
    (Conjunction.
     (loop [out (transient [])
            vals vals-in]
       (if (empty? vals) (persistent! out)
           (let [[x & vals] vals]
             (cond
               (instance? Constraint x)
               (recur (conj! out x) vals)

               (instance? Conjunction x)
               (recur (reduce conj! out (:body x)) vals)

               :else (throw (ex-info "Non-logical argument given to :and" {:argument x
                                                                           :arguments vals-in
                                                                           })))))))))

(defn eql [vals]
  (linearize
   (into [:and]
         (for [[a b] (partition-all 2 1 vals) :when (and a b)]
           (let [d (sub [a b])
                 k (constant-value d)
                 e (sub [d k])]
             (Constraint. e (- k) (- k)))))))

(defn less [vals]
  (case (count vals)
    2 (let [delta (sub vals)
            k (constant-value delta)
            body (sub [delta k])]
        (Constraint. body nil (- k)))
    3 (let [[lb mid ub] vals]
        (if (and (constant? lb)
                 (constant? ub))
          (let [k (constant-value mid)]
            (Constraint. (sub [mid k])
                         (- (constant-value lb) k)
                         (- (constant-value ub) k)))

          (linearize [:and [:<= lb mid] [:<= mid ub]])))

    (linearize
     (into [:and]
           (for [[l g] (partition-all 2 1 vals) :when (and l g)]
             [:<= l g])))))

(defn more [vals] (less (reverse vals)))

(extend-protocol ILinearizable
  Number
  (linearize [x] x)

  clojure.lang.IPersistentVector
  (linearize [x]
    (if (contains? *variables* x)
      (lp-var x)
      
      (let [f (nth x 0)
            f (cond
                (= + f) :+
                (= * f) :*
                (= / f) :/
                (= - f) :-
                (= = f) :=
                (= <= f) :<=
                (= >= f) :>=
                true f)]
        (case f
          :* (product (linearize-args x))
          :+ (sum (linearize-args x))
          :/ (divide (linearize-args x))
          :- (sub (linearize-args x))
          :=  (with-meta (eql (linearize-args x)) {:lp/input x})
          :<= (with-meta (less (linearize-args x)) {:lp/input x})
          :>= (with-meta (more (linearize-args x)) {:lp/input x})
          :and (with-meta (logand x (linearize-non-nil-args x)) {:lp/input x})
          ::upper (if-let [v (get *variables* (nth x 1))]
                    (linearize (:upper v Double/POSITIVE_INFINITY))
                    (throw (ex-info "Upper bound for unknown variable" {:expression x})))
          ::lower (if-let [v (get *variables* (nth x 1))]
                    (linearize (:lower v Double/NEGATIVE_INFINITY))
                    (throw (ex-info "Upper bound for unknown variable" {:expression x})))
          ::<M ;; x <= x_on * M
          (linearize [:<=
                      (nth x 1)
                      [* (nth x 2) [::upper (nth x 1)]]])
          
          (lp-var x)
          ))))

  Object
  (linearize [x] (lp-var x))

  nil
  (linearize [x] (linearize false))

  Boolean
  (linearize [x] (if x 1 0)))


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
      (let [flatten-seq
            (fn flatten-seq [constraints]
              (if (or (seq? constraints)
                      (and (vector? constraints)
                           (coll? (first constraints))))
                (mapcat flatten-seq constraints)
                (list constraints)))
            
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
                       constraints (update :constraints flatten-seq)
                       subject-to  (-> (update :constraints concat
                                               (flatten-seq subject-to))
                                       (dissoc :subject-to))
                       ))
            
            lp (-> lp
                   (update :vars expand-indices)
                   (->> (s/transform [:vars s/MAP-VALS] add-bounds)))

            lp (binding [*variables* (:vars lp)]
                 (->> lp
                      (s/transform
                       [:constraints s/ALL]
                       (fn [c]
                         (when c
                           (if (and (seq? c) (not (vector? c)))
                             (linearize [:and c])
                             (linearize c)))))
                      
                      (s/setval [:constraints s/ALL nil?] s/NONE) ;; remove nils
                      (s/transform [:objective] linearize)))
            ]

        ;; Convert to normalized program record
        (map->Program lp))
      (throw (ex-info
              "Invalid linear program"
              {:explain-data (spec/explain-data :lp/program lp)
               :explain-message (spec/explain-str :lp/program lp)})))))

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

(defn gradient-double
  "Gets the gradient of x, taking the coefficients as doubles"
  [x]
  (let [g (gradient x)]
    (persistent!
     (reduce-kv
      (fn [a k v]
        (assoc! a k (double v)))
      (transient {})
      g))))

(defn constant-double
  "Gets the constant part of x, as a double"
  [x]
  (double (constant-value x)))
