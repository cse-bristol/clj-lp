;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

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
       "s" :fixed}]
  
  (defn glpsol [text index-to-variable]
    (let [lines (-> text
                    (s/split-lines)
                    (->> (map #(s/split % #" "))
                         (group-by first)))

          [status] (get lines "s") ;; status line

          read-mip-sol
          (fn [[_ _ status obj-val] lines]
            
            (let [obj-val (try (Double/parseDouble obj-val)
                               (catch NumberFormatException e))]
              
              {:solution
               (cond
                 (= "n" status)
                 {:exists false :reason :infeasible}


                 (> (Math/abs obj-val) 1e18)
                 {:exists false :reason :unbounded}

                 (= "u" status)
                 {:reason :unknown :exists (number? obj-val) :value obj-val}
                 
                 (= "f" status)
                 {:exists true :value obj-val}

                 (= "o" status)
                 {:exists true :optimal true})
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
             (let [reason (cond
                            (or (= dstat "n") (= pstat "n")) :unbounded
                            (= pstat "i")                    :infeasible
                            (= pstat "u")                    :unknown
                            (= pstat "f")                    :optimal)]
               {:value   (try (Double/parseDouble obj-val) (catch NumberFormatException e))
                :exists  (= reason :optimal)
                :optimal (= reason :optimal)
                :reason  reason
                })
             
             :vars
             (->>
              (for [[_ n st primal dual] (get lines "j")]
                [(get index-to-variable (dec (Integer/parseInt n)))
                 (cond-> {}
                   st     (assoc :status (var-status-codes st))
                   primal (assoc :value  (Double/parseDouble primal))
                   dual   (assoc :dual-value   (Double/parseDouble dual))
                   )])
              (into {}))})]
      
      (case (second status)
        "mip" (read-mip-sol (drop 2 status) lines)
        "bas" (read-bas-sol (drop 2 status) lines)
        ;; "ipt" (read-ipt-sol (drop 2 status) lines) ;; interior point method

        (throw (ex-info "Unknown status type" {:type type}))
        )
      
      
      )))

(defn solve [lp & {:as args}]
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
        )))


