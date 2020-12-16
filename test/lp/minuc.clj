(ns lp.minuc
  (:require  [clojure.test :as t]))

(def insoluble
  {:minimize [+ :x :y]
   :vars {:x {} :y {}}
   :subject-to
   [^{:name :first} [<= :y :x]
    ^{:name :second} [>= 5 :x]
    ^{:name :third} [>= :y 6]
    ]})

