(ns lp.glpk
  (:require [lp.core :as lp]
            [lp.io :as lpio :refer [with-temp-dir]]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as s]))

(let [var-status-codes
      {"b" :basic
       "l" :bounded-below
       "u" :bounded-above
       "f" :free
       "s" :fixed}




      ]
  (defn glpsol [text index-to-variable]
    (let [lines (-> text
                    (s/split-lines)
                    (->> (map #(s/split % #" "))
                         (group-by first)))

          [status] (get lines "s") ;; status line

          read-mip-sol
          (fn [[_ _ status obj-val] lines]
            (let [obj-val (Double/parseDouble obj-val)]
              {:solution
               {:value obj-val
                :status
                (cond
                  (= "n" status) :infeasible
                  (= "u" status) :unknown
                  (> (Math/abs obj-val) 1e18) :unbounded
                  (= "f" status) :feasible
                  (= "o" status) :optimal)}
               
               :vars
               (->>
                (for [[_ n value] (get lines "j")]
                  [(get index-to-variable (dec (Integer/parseInt n)))
                   (cond-> {}
                     value (assoc :value  (Double/parseDouble value)))])
                (into {}))}))

          read-bas-sol
          (fn [[prows pcols pstat dstat obj-val] lines]
            {:solution
             {:value (Double/parseDouble obj-val)
              :status
              (cond
                (or (= dstat "n") (= pstat "n")) :unbounded
                (= pstat "i") :infeasible
                (= pstat "u") :unknown
                (= pstat "f") :optimal)}
             
             :vars
             (->>
              (for [[_ n st primal dual] (get lines "j")]
                [(get index-to-variable (dec (Integer/parseInt n)))
                 (cond-> {}
                   st (assoc :status (var-status-codes st))
                   primal (assoc :value  (Double/parseDouble primal))
                   dual (assoc :dual-value   (Double/parseDouble dual))
                   )])
              (into {}))})
          ]
      (case (second status)
        "mip" (read-mip-sol (drop 2 status) lines)
        "bas" (read-bas-sol (drop 2 status) lines)
        ;; "ipt" (read-ipt-sol (drop 2 status) lines) ;; interior point method

        (throw (ex-info "Unknown status type" {:type type}))
        )
      
      
      )))

(defn solve [lp]
  (let [{problem-text :program var-index :index-to-var}
        (lpio/cplex lp)
        
        result-text
        (with-temp-dir temp-dir
          (spit (io/file temp-dir "problem") problem-text)
          (sh/sh
           "glpsol" "--lp" "problem" "--write" "solution"
           :dir temp-dir)
          (slurp (io/file temp-dir "solution")))

        result (glpsol result-text var-index)]
    (-> (lp/merge-results lp result)
        (assoc
         :state blah
         )
        )))


