;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns lp.core
  (:require [clojure.set :refer [map-invert]]
            [com.rpl.specter :as s]
            [clojure.math.combinatorics :refer [cartesian-product]]
            [clojure.spec.alpha :as spec]
            [lp.dsl]))

(declare expand-indices)

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

(def ^:dynamic *variables* nil)
(def ^:dynamic *generated*
  "Used to allow program expansion to introduce new variables and constraints.
  This is less pure than returning more things from ILinearizable, but overall
  simpler, as otherwise we need a lot of boilerplate for passing stuff back up
  the stack."
  nil)

(defn lookup-var [x]
  (or (get @*variables* x)
      (get (:vars @*generated*) x)))

(defn lp-var [x]
  (if-let [v (lookup-var x)]    
    (if (or (:fixed v)
            (and (:lower v)
                 (:upper v)
                 (== (:lower v) (:upper v))))
      (linearize (:value v (:lower v)))
      (Linear. 0 {x 1}))
    (throw (ex-info "Cannot linearize" {:x x
                                        :vars @*variables*
                                        :gen (:vars @*generated*)
                                        }))))

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
         constant 0.0
         grad (transient {})]
    (if (empty? xs)
      (sprod constant (persistent! grad))
      (let [[x & xs] xs]
        (recur xs
               (+ constant (double (constant-value x)))
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
           constant 1.0
           grad     {}]
      (if (empty? xs)
        (sprod constant grad) ;; uck
        (let [[x & xs] xs]
          (cond
            (constant? x)
            (let [k (double (constant-value x))]
              (recur
               xs
               (* constant k)
               (grad* grad k)))

            (empty? grad)
            (recur xs
                   (* constant (double (constant-value x)))
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

(defn gen-var
  "Generate a new unique variable index for use in this LP"
  []
  [::gen (:next-var (swap! *generated* update :next-var inc))])

(defmulti linearize-expression
  "Linearize an expression written in a vector - this is a multimethod
  to allow extension with new shorthands. The return value will be linearised
  and can have metadata :vars and :constraints to add vars and constraints
  into the program."
  (fn [expression] (first expression)))

(defmethod linearize-expression :default
  [x] (lp-var x))

(defn add-bounds [var]
  (try (cond-> var
         (= (:type var) :non-negative)
         (update :lower (fn [l] (max (or l 0) 0)))
         
         (= (:type var) :binary)
         (assoc :lower 0 :upper 1))
       (catch Exception e
         (throw (ex-info "Adding bounds to var" {:var var} e))
         )))

(defn bounds
  "Get the bounds on x, something ILinear, as [lower upper]

  x must already be ILinear, rather than ILinearisable, because calling
  (linearise x) may have side-effects which mean we don't want to do
  it more than once (creating vars / constraints).

  This is an unfortunate consequence of implementing that using
  side-effects rather than keeping it pure."
  [x]
  (if (satisfies? ILinear x)
    (let [c (constant-value x)
          g (gradient x)]
      (loop [u c
             l c
             g g]
        (if (seq g)
          (let [[[x k] & g] g
                v (lookup-var x)
                xu (* k (:upper v Double/POSITIVE_INFINITY))
                xl (* k (:lower v Double/NEGATIVE_INFINITY))
                ]
            (recur
             (+ u (max xu xl))
             (+ l (min xu xl))
             g))
          [l u])))
    (throw (ex-info "Cannot get bounds for something that isn't already linear" {:x x}))))

(extend-protocol ILinearizable
  Number
  (linearize [x] x)

  clojure.lang.IPersistentVector
  (linearize [x]
    (if (lookup-var x)
      (lp-var x)
      
      (let [f (nth x 0)
            f (cond
                (= + f)  :+
                (= * f)  :*
                (= / f)  :/
                (= - f)  :-
                (= = f)  :=
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

          ;; TODO extend upper and lower to compute upper/lower for expressions
          ::upper (if-let [v (lookup-var (nth x 1))]
                    (linearize (:upper v Double/POSITIVE_INFINITY))
                    (throw (ex-info "Upper bound for unknown variable" {:expression x})))
          ::lower (if-let [v (lookup-var (nth x 1))]
                    (linearize (:lower v Double/NEGATIVE_INFINITY))
                    (throw (ex-info "Upper bound for unknown variable" {:expression x})))
          ::<M ;; x <= x_on * M
          (linearize [:<=
                      (nth x 1)
                      [* (nth x 2) [::upper (nth x 1)]]])

          (let [result (linearize-expression x)
                {:keys [vars constraints]} (meta result)]
            ;; tack on any newly introduced vars and constraints - this has to happen
            ;; here so we can call linearize below and it will know about our new vars
            (swap! *generated*
                   (fn [g]
                     (assoc g
                            :vars (merge (:vars g)
                                         (->>
                                          (expand-indices vars)
                                          (s/transform [s/MAP-VALS] add-bounds)))
                            :constraints (into (:constraints g) constraints))))
            ;; try again, so that linearize-expression can just return another expression
            ;; and we will process it here.
            (linearize result))))))

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





(defn- adjust-precision-and-scale
  "Update the (already linearized) objective of lp to have a certain scale and precision."
  [lp]
  (letfn [(digits [x]
            (let [r (int
                     (Math/ceil
                      (Math/abs
                       (Math/log10 x))))]
              (if (< x 1.0) (- r) r)))

          (round-digits [x d]
            (let [e (Math/pow 10.0 (double d))]
              (* (bigdec e) (Math/round (/ x e)))))
          
          (to-scale [x scale]
            (if (< x scale)
              ;; 1sf
              (round-digits x (digits x))
              (round-digits x (digits scale))))]
    
    (let [objective-scale     (:objective-scale lp 1.0)
          objective-precision (:objective-precision lp 0.0)]
      (cond-> lp
        (not (== 1.0 objective-scale))
        (update :objective
                (fn [objective]
                  (linearize [:/ objective objective-scale])))
        
        (not (== 0.0 objective-precision))
        (update :objective
                (let [vars (:vars lp)]
                  (fn [objective]
                    (let [g (gradient objective)
                          c (constant-value objective)]
                      (->Linear c (into
                                   {}
                                   (for [[x k] g] ;; this entry corresponds to k*x in the equation
                                     (let [var (get vars x)
                                           ;; this is after expand-indices so the bounds are numbers
                                           u   (:upper var Double/MAX_VALUE)
                                           l   (:lower var (- Double/MAX_VALUE))

                                           r     (max (Math/abs u) (Math/abs l))
                                           scale (/ objective-precision r)
                                           k2    (to-scale k scale)
                                           ]
                                       [x (if (> (Math/abs (- k k2)) objective-precision)
                                            (do
                                              (binding [*out* *err*]
                                                (println "Objective precision adjustment imposisble for" x (Math/abs (- k k2)) objective-precision))
                                              k) k2)]
                                       ))))))))
        ))))

(defn normalize
  "Given `lp`, return a normalized form of it, unless already done.

  In the normalized form:
  - Variable names are the same
  - The objective and constraints are, where possible, transformed into a sumproduct
  - Constraints are not fixed up for canonical form
  - Nonlinear features have been converted into linear ones
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

            lp (binding [*variables* (atom (:vars lp))
                         *generated* (atom {:next-var 0
                                            :vars {}
                                            :constraints nil})]
                 ;; we iterate on expanding the LP until nothing has produced any extra constraints
                 (-> (loop [lp lp]
                       (let [lp
                             (->> lp
                                  (s/transform
                                   [:constraints s/ALL]
                                   (fn [c]
                                     (when c
                                       (if (and (seq? c) (not (vector? c)))
                                         (linearize [:and c])
                                         (linearize c)))))
                                  
                                  (s/setval [:constraints s/ALL nil?] s/NONE) ;; remove nils
                                  (s/transform [:objective] linearize))
                             ]
                         (let [{gen-vars :vars gen-cons :constraints} @*generated*]
                           (swap! *generated* assoc :vars {} :constraints nil)
                           ;; add in new variables and constraints
                           
                           (let [lp (-> (update lp :vars merge gen-vars)
                                        (update :constraints into gen-cons))]
                             ;; keep going if we made any new constraints
                             ;; this may not be needed but it can't hurt
                             (swap! *variables* merge (:vars lp))
                             (cond-> lp (seq gen-cons) (recur))))))
                     (adjust-precision-and-scale)))]

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
             (contains? v :value)
             (update-in [var :value] assoc index (:value v))
             
             (contains? v :dual-value)
             (update-in [var :dual-value] assoc index (:dual-value v))
             
             (contains? v :status)
             (update-in [var :status] assoc index (:status v))

             (contains? v :fixed)
             (update-in [var :fixed] assoc index (:fixed v))

             (contains? v :upper)
             (update-in [var :upper] assoc index (:upper v))

             (contains? v :lower)
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

(defn empty-indexes?
  "Used to tell whether a variable actually doesn't exist because its index set
  is empty"
  [var]
  (when (contains? var :indexed-by)
    (let [indexes (:indexed-by var)]
      (or (empty? indexes) ;; directly empty
          (and (vector? indexes) (some empty? indexes)))))) ;; empty cartesian product

(defn merge-results
  "Add results back onto an lp.
  If the original lp is not normalized it might have vars that are :indexed-by
  The :value for these should be of the form {[index] value ...}
  "
  [lp result]

  (let [result-vars (collapse-indices (:vars lp) (:vars result))]
    (-> lp
        (update :vars
                (fn [in-vars]
                  (let [in-vars (s/setval [s/MAP-VALS
                                           (s/pred empty-indexes?)
                                           :value] s/NONE in-vars)
                        merged-vars (merge-with merge in-vars result-vars)]
                    (fix-var-types merged-vars))))
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
