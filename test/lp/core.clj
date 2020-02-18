(ns lp.core
  (:require [lp.core :as sut]
            [clojure.test :as t]))

(t/deftest norm-*
  (binding [sut/*lp-vars* {:a {} :b {}}]
    (t/is (=
           (sut/norm-expr
            [:+
             [:* :a :a]
             [:* :b :b]
             [:* 2.0 :b :a]
             ])
           (sut/norm-expr
            [:*
             [:+ :a :b]
             [:+ :a :b]]))))

  )
