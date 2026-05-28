(ns lp.test-diagnostics
  (:require [lp.diagnostics :as sut]
            [clojure.test :as t]))

(t/deftest known-coefficient-range
  ;; Row 0: 2x + 6y <= 10  → coeffs 2, 6   ratio 3
  ;; Row 1: 1000x + z <= 100 → coeffs 1000, 1  ratio 1000
  ;; Global min = 1.0 (z in row 1), global max = 1000.0 (x in row 1)
  ;; Worst ratio = 1000.0 (row 1)
  (let [lp     {:minimize [:+ :x :y]
                :vars {:x {:type :non-negative}
                       :y {:type :non-negative}
                       :z {:type :non-negative}}
                :constraints [[:<= [:+ [:* 2 :x] [:* 6 :y]] 10]
                              [:<= [:+ [:* 1000 :x] :z] 100]]}
        result (sut/coefficient-diagnostics lp)]
    (prn result)
    (t/is (== 1.0    (:value (:coeff-min result))))
    (t/is (== 1000.0 (:value (:coeff-max result))))
    (t/is (== 1000.0 (:value (:worst-row-ratio result))))))

(t/deftest single-coefficient-constraint
  ;; One variable, implicit coefficient 1 → ratio must equal 1.
  (let [lp     {:minimize :x
                :vars {:x {:type :non-negative}}
                :constraints [[:<= :x 5]]}
        result (sut/coefficient-diagnostics lp)]
    (t/is (== 1.0 (:value (:coeff-min result))))
    (t/is (== 1.0 (:value (:coeff-max result))))
    (t/is (== 1.0 (:value (:worst-row-ratio result))))))

(t/deftest no-constraints
  ;; Problem with no constraints → every key nil, no throw.
  (let [lp     {:minimize :x
                :vars {:x {:type :non-negative}}}
        result (sut/coefficient-diagnostics lp)]
    (t/is (nil? (:coeff-min result)))
    (t/is (nil? (:coeff-max result)))
    (t/is (nil? (:worst-row-ratio result)))))

(t/deftest and-compound-constraint
  ;; :and is flattened; each sub-constraint is its own row.
  ;; Row 0: 3x   → ratio 1
  ;; Row 1: 100y → ratio 1
  ;; Global min = 3.0, max = 100.0, worst ratio = 1.0
  (let [lp     {:minimize [:+ :x :y]
                :vars {:x {:type :non-negative}
                       :y {:type :non-negative}}
                :constraints [[:and
                               [:<= [:* 3 :x] 9]
                               [:<= [:* 100 :y] 200]]]}
        result (sut/coefficient-diagnostics lp)]
    (t/is (== 3.0   (:value (:coeff-min result))))
    (t/is (== 100.0 (:value (:coeff-max result))))
    (t/is (== 1.0   (:value (:worst-row-ratio result))))))

(t/deftest constant-only-constraint-no-throw
  ;; [:<= 6 5] is an infeasible all-constant constraint (no variable terms).
  ;; It must not throw and must not affect the coefficient statistics.
  (let [lp     {:minimize :x
                :vars {:x {:type :non-negative}}
                :constraints [[:<= 6 5]
                              [:<= :x 10]]}
        result (sut/coefficient-diagnostics lp)]
    (t/is (== 1.0 (:value (:coeff-min result))))
    (t/is (== 1.0 (:value (:coeff-max result))))
    (t/is (== 1.0 (:value (:worst-row-ratio result))))))
