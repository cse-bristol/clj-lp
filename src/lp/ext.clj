(ns lp.ext
  " LP extensions to support nonlinear programming

  Implemented special forms are:

  PIECEWISE CURVES:
  
  e.g. [::ext/piecewise [[0 1] [2 3] [4 9]] :X] will interpolate y 1..3 for x 0..2 and so on

  this can only do one dimensional interpolation. it introduces one indicator and two lambda
  variables per line segment, plus constraints to make it work.

  IF

  [:lp.ext/if :X :Y :Z]

  aka y if x is true else z

  if x is an expression an indicator will be introduced
  "
  (:require [clojure.walk :as walk]
            [com.rpl.specter :as s]
            [lp.core :as core]))

(defmethod core/linearize-expression ::piecewise
  [[_ matrix x]]
  
  (let [n-segments (dec (count matrix))
        indicator (core/gen-var) ;; the variable we will use for saying if a segment is on
        position  (core/gen-var) ;; the variable(s) we use for saying where we are in the segment

        x-val (core/gen-var)
        y-val (core/gen-var)
        
        index (set (range n-segments))
        vars {indicator {:type :binary :indexed-by [index]}
              ;; we have two position indicators per segment.
              ;; the first one is how far we are from x0 going up
              ;; and the second is how far we are from x1 going down
              position  {:lower 0 :upper 1 :indexed-by [index #{0 1}]}
              x-val     {:lower (reduce min (map first matrix))
                         :upper (reduce max (map first matrix))}
              y-val     {:lower (reduce min (map second matrix))
                         :upper (reduce max (map second matrix))}}
        cons (->
              ;; if using segment i, find position within i
              (for [i index]
                [:=
                 [indicator i]
                 [:+ [position i 0] [position i 1]]])

              (conj
               [:= x-val
                [:+
                 ;; for the single segment we are using, the position
                 ;; values will sum to 1 and pick out the x-range on that segment
                 ;; all others go to zero and contribute nothing to the sum
                 (for [i index] [:* [position i 0] (nth (nth matrix i) 0)])
                 (for [i index] [:* [position i 1] (nth (nth matrix (inc i)) 0)])]])

              (conj
               [:= y-val
                [:+
                 ;; similarly for the y value, one segment will be picked out
                 (for [i index] [:* [position i 0] (nth (nth matrix i) 1)])
                 (for [i index] [:* [position i 1] (nth (nth matrix (inc i)) 1)])]])

              ;; the x value should equal whatever x the user said
              (conj [:= x-val x])
              
              ;; finally constraint to use only one segment
              ;; doesn't have to be =1 because x will force it on
              (conj [:<= 1 [:+ (for [i index] [indicator i])]]))]
    (with-meta
      y-val
      {:vars vars
       :constraints cons})))

(defmethod core/linearize-expression ::if
  [[_ x y z]]
  
  (let [y (core/linearize y)
        z (core/linearize z)
        
        [ly uy] (core/bounds y)
        [lz uz] (core/bounds z)
        not-x [:- 1 x]

        const-y (= ly uy)
        const-z (= lz uz)
        
        P (if const-y
            [:* x ly]
            (core/gen-var))
        Q (if const-z
            [:* not-x lz]
            (core/gen-var))
        ]
    (with-meta
      [:+ P Q]
      {:vars (cond-> {}
               (not const-y) (assoc P {})
               (not const-z) (assoc Q {}))
       
       :constraints
       (list
        ;; if x is zero, P is zero
        ;; if x is 1, P is in the range of Y
        (when-not const-y
          (list [:<= P [:* x uy]]
                [:>= P [:* x ly]]

                ;; if x is 1, P > y and P < y
                [:<= P [:- y [:* not-x ly]]]
                [:>= P [:- y [:* not-x uy]]]))

        ;; analogies for Q
        (when-not const-z
          (list [:<= Q [:* x uz]]
                [:>= Q [:* x lz]]
                [:<= Q [:- z [:* not-x lz]]]
                [:>= Q [:- z [:* not-x uz]]]))
        )})))
