(ns lp.dsl
  (:require [clojure.spec.alpha :as s]))

(s/def :lp/program
  (s/and
   (s/keys
    :req-un [:lp/vars]
    :opt-un
    [:lp/maximize
     :lp/minimize
     :lp/subject-to
     :lp/objective
     :lp/sense
     :lp/constraints])
   (s/or
    :has-maximize
    (s/and #(contains? % :maximize)
           #(not (contains? % :minimize))
           #(not (contains? % :objective))
           #(not (contains? % :sense)))
    :has-minimize
    (s/and #(contains? % :minimize)
           #(not (contains? % :maximize))
           #(not (contains? % :objective))
           #(not (contains? % :sense)))
    :has-objective
    (s/and #(contains? % :objective)
           #(not (contains? % :maximize))
           #(not (contains? % :minimize))))))

(s/def :lp/vars
  (s/map-of identity :lp/var))

(s/def :lp/var
  (s/keys
   :opt-un [:lp/type :lp/lower :lp/upper :lp/value :lp/fixed :lp/indexed-by]))

(s/def :lp/type #{:integer :non-negative :binary})

(s/def :lp/constraints
  (s/or
   :labelled-constraints (s/map-of identity :lp/constraint)
   :unlabelled-constraints (s/coll-of :lp/constraint)))

(s/def :lp/subject-to :lp/constraints)
(s/def :lp/sense #{:minimize :maximize})

(s/def :lp/expr any?)
(s/def :lp/objective :lp/expr)
(s/def :lp/constraint :lp/expr)





