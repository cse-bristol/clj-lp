;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns lp.test-scip
  (:require [clojure.test :as t]
            [lp.diet :refer [diet]]
            [lp.scip :as scip]
            [lp.ext :as ext]))

(defn- ≅ [a b]
  (< (Math/abs (- a b)) 0.001))

(t/deftest test-diet-result
  (let [result (scip/solve diet)
        vars (:vars result)]
    (t/is (≅ 3.15 (:value (:solution result))))
    (t/is (≅ 1.94444444 (:value (:corn vars))))
    (t/is (≅ 10.0 (:value (:milk vars))))
    (t/is (≅ 10.0 (:value (:bread vars))))))

(t/deftest test-diet-result-scale
  (let [result (scip/solve (assoc diet
                                  :objective-scale 10.0
                                  :objective-precision 1.0))
        vars (:vars result)]

    ;; we don't check objective as we will have made that wrong
    ;; in the use of objective-precision.
    (t/is (≅ 1.94444444 (:value (:corn vars))))
    (t/is (≅ 10.0 (:value (:milk vars))))
    (t/is (≅ 10.0 (:value (:bread vars))))))

(t/deftest test-binary-variables
  (let [switches (set (range 5))
        result (scip/solve
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
        result (scip/solve
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

(t/deftest piecewise-test
  (t/is
   (= 7.0
      (:value (:x (:vars
                   (scip/solve
                    {:vars {:y {}
                            :x {}}
                     :maximize :y
                     :subject-to
                     (list
                      [:= :y [::ext/piecewise
                              [[0 10] [4 15] [8 -1]]
                              :x]]
                      [:<= :y 13]
                      )})))))))

(t/deftest if-test
  (t/testing "simple constants"
    (-> (scip/solve
         {:vars {:x {:type :binary}} :maximize [::ext/if :x 10 4]})
        :vars :x :value
        (= true)
        (t/is))
    (-> (scip/solve
         {:vars {:x {:type :binary}} :maximize [::ext/if :x 4 10]})
        :vars :x :value
        (= false)
        (t/is)))

  (t/testing "subexpressions"
    (-> (scip/solve
         {:vars {:x {:type :binary}
                 :a {:lower 10 :upper 30}
                 :b {:lower 3 :upper 9}
                 :c {:lower 1 :upper 2}}
          :maximize [::ext/if :x
                     [:+ [:* :a 3] [:* :b -2] :c]
                     [:+ [:* :a -1] [:* :b -2] [:* :c 3]]]
          :subject-to (list [:>= :a :c])
          })
        (:vars)
        (select-keys [:x :a :b :c])
        (update-vals :value)
        (= {:x true :a 30.0 :b 3.0 :c 2.0})
        (t/is))))

(t/deftest test-warm-start-from-solution
  ;; binary values read back from a solution are booleans, and warm
  ;; starting from them should work like warm starting from numbers
  (let [switches (set (range 5))
        program  {:maximize [:+ (for [x switches] [:switch x])]
                  :vars {:switch {:indexed-by [switches] :type :binary}}
                  :subject-to (list [:<= [:+ (for [x switches] [:switch x])] 3])}
        first    (scip/solve program)
        second   (scip/solve first :warm-start? true)]
    (t/is (boolean? (-> first :vars :switch :value (get 0))))
    (t/is (≅ 3.0 (:value (:solution second))))))
