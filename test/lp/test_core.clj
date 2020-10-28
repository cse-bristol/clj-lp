(ns lp.test-core
  (:require [lp.core2 :as sut]
            [clojure.test :as t]
            [lp.core :as lp]))

(t/deftest constant-folding
  (let [has-value
        (fn [value expr]
          (let [expr (sut/linearize expr)]
            (t/is (sut/constant? expr))
            (t/is (== value (sut/constant-double expr)))))]

    (has-value 0 0)
    (has-value 1 1)
    (has-value -1 -1)

    (has-value 1 [:+ 1 0])
    (has-value 2 [:+ 1 1])
    (has-value 0 [:- 1 1])

    (has-value 0 [:* 0 10])
    (has-value 10 [:* 1 10])
    (has-value -10 [:* [:- 1] 10])
    (has-value 15 [:* 3 5])

    (has-value 15 [:* 3 [:+ 2 3]])

    (has-value 1 [:/ [:- 1] -1])
    (has-value 0.5 [:/ 1 2])

    (binding [sut/*variables* {:a {:value 10 :fixed true}}]
      (has-value 10 :a)
      (has-value 20 [:* 2 :a])
      (has-value 0 [:- :a :a])
      (has-value 100 [:* :a :a]))
    
    ))

(t/deftest gradients
  (let [p {:minimize [:+ [:* 3 :x] [:* 2 :y] 9]
           :vars {:x {} :y {}}}
        p (sut/normalize p)
        g (sut/gradient-double p)
        ]
    (t/is
     (= (set (keys g))
        #{:y :x}))

    (t/is (== 9 (sut/constant-double p)))
    (t/is (== 2 (:y g)))
    (t/is (== 3 (:x g)))
    ))

(t/deftest constraint-bodies-no-constant
  ;; in the normalized form, all constraints should have the constant
  ;; part moved outside the main expression and into the bound.
  (let [p {:minimize [:+ [:* 3 :x] [:* 2 :y] 9]
           :subject-to (list [:<= :x [:+ :y 33]])
           :vars {:x {} :y {}}}
        p (sut/normalize p)
        c (first (sut/all-constraints p))
        ]
    (t/is (instance? lp.core2.Constraint c))
    (t/is (zero? (sut/constant-double (:body c))))
    ;; this should either be
    ;; x - y <= 33
    ;; or y - x >= -33

    (t/is (or (and (== 33 (:upper c))
                   (== 1 (:x (sut/gradient-double c)))
                   (== -1 (:y (sut/gradient-double c)))
                   )

              (and (== -33 (:lower c))
                   (== -1 (:x (sut/gradient-double c)))
                   (== 1 (:y (sut/gradient-double c)))
                   )))))

(t/deftest variable-shorthands
  (t/is (= (sut/expand-indices
            {:x {:indexed-by [#{1 2} #{:a :b}]}})
           {[:x 1 :a] {}
            [:x 2 :a] {}
            [:x 1 :b] {}
            [:x 2 :b] {}}))

  (t/is (= (sut/expand-indices
            {:x {:indexed-by [#{1} #{:a}]
                 :value {[1 :a] 33}
                 :lower {[1 :a] 10}
                 :upper {[1 :a] 11}
                 :fixed true
                 }})
           {[:x 1 :a] {:value 33 :lower 10 :upper 11 :fixed true}}))

  (t/is (= (sut/expand-indices
            {:x {:indexed-by [#{1} #{:a}]
                 :value (fn [x y] [x y])}})
           {[:x 1 :a] {:value [1 :a]}}))

  (t/is (= (sut/expand-indices
            {:x {:indexed-by #{[1 2 3] [4 5 6]}
                 :value (fn [x y z] (+ x y z))}})

           {[:x 1 2 3] {:value 6}
            [:x 4 5 6] {:value 15}}))

  (t/is (= (sut/expand-indices {:x {:indexed-by [#{1 2}] :value {1 3, 2 9}}})
           {[:x 1] {:value 3}, [:x 2] {:value 9}}))

  (let [orig-vars {:x {:indexed-by [#{1 2} #{:a :b}]}}
        new-vars  {[:x 1 :a] {:value 1}
                   [:x 2 :a] {:value 2}
                   [:x 1 :b] {:value 3}
                   [:x 2 :b] {:value 4}}
        ]
    (t/is (= {[1 :a] 1
              [2 :a] 2
              [1 :b] 3
              [2 :b] 4}
             (-> (sut/collapse-indices orig-vars new-vars)
                 :x :value))))

  ;; this seems like it could be checked with test.check somehow.
  (let [orig-vars {:x {:indexed-by [#{1 2}]}}
        results   {[:x 1] {:value 1}
                   [:x 2] {:value 9}}
        ]
    (t/is (= {1 1, 2 9} (-> (sut/collapse-indices orig-vars results) :x :value)))))

(t/deftest result-var-types
  (t/is
   (= true
      (-> {:x {:type :binary :value 1}}
          (sut/fix-var-types)
          :x :value)))
  (t/is
   (= false
      (-> {:x {:type :binary :value 0.001}}
          (sut/fix-var-types)
          :x :value)))
  (t/is
   (= false
      (-> {:x {:type :binary :value false}}
          (sut/fix-var-types)
          :x :value)))
  (t/is
   (= 1
      (-> {:x {:type :integer :value 1.0}}
          (sut/fix-var-types)
          :x :value)))
  (t/is
   (= 1
      (-> {:x {:type :integer :value 1.0002}}
          (sut/fix-var-types)
          :x :value)))
  (t/is
   (= {:a false :b true}
      (-> {:x {:type :binary :value {:a 0 :b 1}}}
          (sut/fix-var-types)
          :x :value)))
  (t/is
   (= {:a 0 :b 2}
      (-> {:x {:type :integer :value {:a 0.0 :b 2.0}}}
          (sut/fix-var-types)
          :x :value))))

(t/deftest upper-bound-access
  (let [e (sut/normalize
           {:minimize [:+
                       [:lp.core/lower :x]
                       :x
                       [:lp.core/upper :x]]
            :vars {:x {:upper 5 :lower 1}}})
        o (:objective e)
        ]
    (t/is (== 6 (sut/constant-value o)))))

#_ (t/deftest fixed-indexed-values
  (let [is #{:a :b}
        js #{:x :y}
        lp {:vars {:x {:type :integer :indexed-by [is js]
                       :value (fn [i j] 12)
                       :fixed (fn [i j] (or (= :a i) (= :y j)))
                       
                       }}}

        results
        {:vars {[:x :b :x] {:value 99}}}

        lp (sut/merge-results lp results)
        ]

    lp
    )
  )
