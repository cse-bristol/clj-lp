(ns lp.core
  (:require [lp.core :as sut]
            [clojure.test :as t]))

(t/deftest norm-simple-exprs
  (binding [sut/*lp-vars* {:a {} :b {}}]
    (let [normed-expr (sut/norm-expr [:* [:+ :a :b] [:+ :a :b]])]
      (t/is (not (sut/linear? normed-expr)))
      )

    (let [lin-expr (sut/norm-expr [:+ [:* :a 10] [:* 2 [:- :b 3]] [:- [:+ :a :b :b]]])]
      ;; 10 a + 2(b - 3) + (- (a + b + b))
      ;; 10 a + 2b - 6 -a -2b
      ;; 9a - 6
      (t/is (sut/linear? lin-expr))
      (t/is (=
             {:a 9.0 :lp.core/c -6.0}
             (sut/linear-coefficients lin-expr)
             )))))


