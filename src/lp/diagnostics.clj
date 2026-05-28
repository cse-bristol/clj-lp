(ns lp.diagnostics
  (:require [lp.core :as lp]))

(defn- abs-nonzero-coeffs [constraint]
  (seq (keep (fn [v]
               (let [a (Math/abs (double v))]
                 (when-not (zero? a) a)))
             (vals (lp/gradient-double constraint)))))

(defn coefficient-diagnostics
  "Compute coefficient conditioning statistics for a problem.
   
   Returns a map:
   ```
   {:coeff-min       {:value <double> :constraint <id>}
    :coeff-max       {:value <double> :constraint <id>}
    :worst-row-ratio {:value <double> :constraint <id>}}
   ```
   `:coeff-min` / `:coeff-max` are the global minimum and maximum nonzero absolute
   coefficient across all constraints.  
   `:worst-row-ratio` is the maximum ratio of (largest / smallest) absolute nonzero
   coefficient within a single constraint row."
  [lp]
  (let [lp          (lp/normalize lp)
        constraints (map-indexed vector (lp/all-constraints lp))]
    (reduce
     (fn [{gmin :coeff-min gmax :coeff-max gratio :worst-row-ratio :as acc}
          [i constraint]]
       (let [coeffs (abs-nonzero-coeffs constraint)]
         (if (nil? coeffs)
           acc
           (let [id        (format "C%d" i)
                 row-min   (apply min coeffs)
                 row-max   (apply max coeffs)
                 row-ratio (/ row-max row-min)]
             (cond-> acc
               (or (nil? gmin) (< row-min (:value gmin)))
               (assoc :coeff-min {:value row-min :constraint id})

               (or (nil? gmax) (> row-max (:value gmax)))
               (assoc :coeff-max {:value row-max :constraint id})

               (or (nil? gratio) (> row-ratio (:value gratio)))
               (assoc :worst-row-ratio {:value row-ratio :constraint id}))))))
     {:coeff-min nil :coeff-max nil :worst-row-ratio nil}
     constraints)))
