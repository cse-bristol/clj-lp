;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns lp.test-minuc
  (:require  [clojure.test :as t]
             [lp.scip :as scip]
             [lp.core :as lp]))

(def insoluble
  {:minimize [+ :x :y]
   :vars {:x {} :y {}}
   :subject-to
   [^{:name :first} [<= :y :x]
    ^{:name :second} [>= 5 :x]
    ^{:name :third} [>= :y 6]
    ]})

(t/deftest test-insoluble
  (let [bad-cons (scip/minuc insoluble)]
    (t/is (not-empty bad-cons))
    (t/is (#{:first :second :third} (:name (meta (first bad-cons)))))
    (t/is (= 1 (count bad-cons)))))

(def insoluble-2
  {:minimize [+ :x :y]
   :vars {:x {} :y {}}
   :subject-to
   [[:and]
    [:and]
    [:and
     ^{:name :first} [<= :y :x]
     (list)

     ^{:name :second} [>= 5 :x]
     ^{:name :third} [>= :y 6]
     ]]})

(t/deftest test-insoluble-2
  (let [bad-cons (scip/minuc insoluble-2)]
    (t/is (not-empty bad-cons))
    (t/is (#{:first :second :third} (:name (meta (first bad-cons)))))
    (t/is (= 1 (count bad-cons)))))
