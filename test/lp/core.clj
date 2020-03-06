(ns lp.core
  (:require [lp.core :as sut]
            [clojure.test :as t]
            [lp.core :as lp]))

(t/deftest constant-folding
  (let [has-value
        (fn [value expr]
          (let [expr (sut/norm-expr expr)]
            (t/is (sut/is-constant? expr))
            (t/is (== value (sut/constant-value expr)))))]

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

    (binding [sut/*lp-vars* {:a {:value 10 :fixed true}}]
      (has-value 10 :a)
      (has-value 20 [:* 2 :a])
      (has-value 0 [:- :a :a])
      (has-value 100 [:* :a :a]))
    
    ))

(t/deftest linear-expressions
  (let [linear (fn [expr] (sut/linear? (sut/norm-expr expr)))]
    (binding [sut/*lp-vars* {:a {} :b {}}]
      (t/is (linear :a))
      (t/is (linear 0))
      (t/is (not (linear [:* :a :a])))
      (t/is (linear [:+ :a :b]))
      (t/is (not (linear [:* :a :b])))
      (binding [sut/*lp-vars* {:a {} :b {:value 0 :fixed true}}]
        (t/is (linear (sut/norm-expr [:* :a :b])))
        (t/is (sut/is-constant? (sut/norm-expr [:* :a :b]))))
      (binding [sut/*lp-vars* {:a {} :b {:value 1 :fixed true}}]
        (t/is (linear (sut/norm-expr [:* :a :b])))
        (t/is (not (sut/is-constant? (sut/norm-expr [:* :a :b])))))
      )))

(t/deftest gradients
  (let [p {:minimize [:+ [:* 3 :x] [:* 2 :y] 9]
           :vars {:x {} :y {}}}
        p (sut/normalize p)
        g (sut/linear-coefficients (:objective p))
        ]
    (t/is
     (= (set (keys g))
        #{:y :x ::sut/c}))

    (t/is (== 9 (::sut/c g)))
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
        c (first (sut/constraint-bodies p))
        ]
    (t/is (instance? lp.core.Constraint c))
    (t/is (zero? (sut/constant-value (:body c))))
    ;; this should either be
    ;; x - y <= 33
    ;; or y - x >= -33

    (t/is (or (and (== 33 (:upper c))
                   (== 1 (:x (sut/linear-coefficients c)))
                   (== -1 (:y (sut/linear-coefficients c)))
                   )

              (and (== -33 (:lower c))
                   (== -1 (:x (sut/linear-coefficients c)))
                   (== 1 (:y (sut/linear-coefficients c)))
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

