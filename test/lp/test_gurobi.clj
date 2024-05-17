;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns lp.test-gurobi
  (:require [clojure.test :as t]
            [lp.diet :refer [diet]]
            [lp.gurobi :as gurobi]))

(defn- ≅ [a b]
  (< (Math/abs (- a b)) 0.001))

(t/deftest test-diet-result
  (let [result (gurobi/solve diet)
        delta (Math/abs (- (:value (:solution result))
                           3.15
                           ))
        vars (:vars result)
        ]
    (t/is (≅ 3.15 (:value (:solution result))))
    (t/is (≅ 1.94444444 (:value (:corn vars))))
    (t/is (≅ 10.0 (:value (:milk vars))))
    (t/is (≅ 10.0 (:value (:bread vars))))))


(t/deftest test-binary-variables
  (let [switches (set (range 5))
        result (gurobi/solve
                {:maximize [:+ (for [x switches] [:switch x])]
                 :vars {:switch {:indexed-by [switches]
                                 :type :binary}}
                 :subject-to
                 (list
                  [= 1 [+ [:switch 0] [:switch 3]]]
                  [<= [+ [:switch 0] [:switch 1] [:switch 3]] 2]
                  [= [:switch 1] [:switch 4]])
                 })

        switches (-> result :vars :switch :value)
        
        ]
    ;; only 1 of 0 and 3 can be true
    ;; only 2 of 0 1 and 3 can be true
    ;; 1 = 4
    ;; 4 is otherwise free
    ;; 2 is free

    (t/is (or
           (=
            {0 false ;; either 0 is true and 3 is false, or vice-versa
             1 true
             2 true ;; 2 is free, so why not
             3 true
             4 true
             }
            switches)
           (=
            {0 true
             1 true
             2 true ;; 2 is free, so why not
             3 false
             4 true
             }
            switches)))))

(t/deftest test-entirely-false-fixed-variables
  (let [switches (set (range 5))
        result (gurobi/solve
                {:maximize [:+ (for [x switches] [:switch x])]
                 :vars {:switch {:indexed-by [switches]
                                 :value (constantly false)
                                 :fixed (constantly true)
                                 :type :binary}}
                 :subject-to
                 (list)
                 })

        switches (-> result :vars :switch :value)
        
        ]
    (t/is #{false} (set (vals switches)))))

