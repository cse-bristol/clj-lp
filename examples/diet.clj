(ns lp.examples
  (:require [lp.glpk :as glpk]
            [lp.scip :as scip]
            [lp.core :as lp]
            [lp.io :as lpio]))

(def foods #{:corn :milk :bread})
(def nutrients #{:vitA :calories})

(def food-params
  {:corn  {:vitA 107 :calories 72  :cost 0.18 :max 10}
   :milk  {:vitA 500 :calories 121 :cost 0.23 :max 10}
   :bread {:vitA 0   :calories 65  :cost 0.05 :max 10}})

(def nutrient-limits
  {:vitA {:lower 5000 :upper 50000}
   :calories {:lower 2000 :upper 2250}})

(def diet
  {:minimize
   [:+ (for [f foods] [:* f (get-in food-params [f :cost])])]

   :vars
   (->> (for [f foods]
          [f {:type :integer
              :lower 0 :upper (get-in food-params [f :max])}])
        (into {}))

   :subject-to
   (for [n nutrients]
     [:<=
      (get-in nutrient-limits [n :lower])
      [:+ (for [f foods] [:* f (get-in food-params [f n])])]
      (get-in nutrient-limits [n :upper])])})

(comment
  (scip/solve
   diet
   :time-limit 50
   )

  )
