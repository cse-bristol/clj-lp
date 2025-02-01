;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns lp.scip
  (:require [lp.core :as lp]
            [lp.io :as lpio]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as s]
            [clojure.test :as test]))

(def scip-setting-keys
  {:time-limit "limits/time"
   :mip-gap "limits/gap"
   :feasibility-tolerance "numerics/feastol"})

(defn- extract-double
  {:test
     #(and
       (test/is (== 0.0 (extract-double "0")))
       (test/is (== Double/POSITIVE_INFINITY
                    (extract-double "+infinity")))
       (test/is (== Double/NEGATIVE_INFINITY
                    (extract-double "-infinity")))
       (test/is (== 6.3 (extract-double "6.3 cats"))))}

  [s]
  
  (try
    (let [spc (s/index-of s \ )
          s (if spc
              (.substring s 0 spc)
              s)]
      (cond
        (= "+infinity" s)
        Double/POSITIVE_INFINITY
        (= "-infinity" s)
        Double/NEGATIVE_INFINITY
        (= "infinity" s)
        Double/POSITIVE_INFINITY

        :else
        (Double/parseDouble s)))
    (catch NumberFormatException e)))

(defn- extract-messages [scip-output]
  (into {}
        (keep
         (fn [line]
           (let [[_ k v] (re-find #"^ *([A-Z][ A-Za-z()]+?) *: *(.+?) *$" line)]
             (when (and (not (s/blank? k)) (not (s/blank? v)))
               [(keyword (.replaceAll (.toLowerCase k) "[\\s()]+" "-")) v])))
         (s/split-lines scip-output))))

(def scip-statuses
  "Taken from scip/src/scip/scip_general.c:547 ish"
  {"unknown"                            :unknown
   "user interrupt"                     :interrupt
   "node limit reached"                 :node-limit
   "total node limit reached"           :total-node-limit
   "stall node limit reached"           :stall-node-limit
   "time limit reached"                 :time-limit
   "memory limit reached"               :memory-limit
   "gap limit reached"                  :gap-limit
   "solution limit reached"             :solution-limit
   "solution improvement limit reached" :improvement-limit
   "restart limit reached"              :restart-limit
   "optimal solution found"             :optimal
   "infeasible"                         :infeasible
   "unbounded"                          :unbounded
   "infeasible or unbounded"            :infeasible
   "termination signal received"        :killed
   ;; "invalid status code <%d>"
   })

(defn format-settings [settings]
  (s/join
   "\n"
   (for [[k v] settings]
     (format "%s = %s" (scip-setting-keys k k) v))))

(defn colon-separate [line]
  (vec (drop 1 (re-find #" *(.+?) *: *(.*) *" line))))

(defn parse-statistics-file
  {:test #(test/is
           (= {:gap 0.0
               :bounds [3.16 3.16]}
              (parse-statistics-file
               (java.io.StringReader.
                "Solution           :
  Solutions found  :          2 (2 improvements)
  First Solution   : +3.89000000000000e+00   (in run 1, after 1 nodes, 0.00 seconds, depth 2, found by <vbounds>)
  Gap First Sol.   :      69.13 %
  Gap Last Sol.    :      37.39 %
  Primal Bound     : +3.16000000000000e+00   (in run 1, after 1 nodes, 0.00 seconds, depth 2, found by <vbounds>)
  Dual Bound       : +3.16000000000000e+00
  Gap              :       0.00 %
  Avg. Gap         :       0.00 % (0.00 primal-dual integral)
"
                ))))}
  [f]
  (try
    (let [lines (-> (slurp f)
                    (s/split-lines)
                    ;; the output is in groups with headings not
                    ;; starting with whitespace
                    (->> (partition-by #(.startsWith % " "))
                         (partition-all 2))
                    ;; so now we have like
                    ;; [[[header],[lines ...]] ...]
                    
                    (->> (map (fn [[[header] lines]]
                                (let [[header-name header-value] (colon-separate header)
                                      line-parts (map colon-separate lines)
                                      ]
                                  
                                  [header-name
                                   (merge (when-not (s/blank? header-value)
                                            {:value header-value})
                                          (into {} (map colon-separate lines)))])))
                         (into {})))
          
          get-double
          (fn [s]
            (try
              (let [[_ v] (re-find
                           #"(([+-]?infinity)|([+-]?\d+(.\d+)?(e[+-]\d+)?))"
                           s)]
                (cond
                  (or (= "infinity" v)
                      (= "infinite" v)) ##Inf
                  (= "+infinity" v) ##Inf
                  (= "-infinity" v) ##-Inf
                  :else (Double/parseDouble v)))
              (catch Exception e))
            )
          ]
      {:gap (let [gap-percent (get-double (get-in lines ["Solution" "Gap"]))]
              (when gap-percent (/ gap-percent 100.0)))
       :bounds [(get-double (get-in lines ["Solution" "Dual Bound"]))
                (get-double (get-in lines ["Solution" "Primal Bound"]))]})
    (catch Exception e {})))

(defn parse-output-file [f var-index]
  (try (let [f                     (slurp f)
             lines                 (s/split-lines f)
             [status-line & lines] lines

             status-text (s/trim (second (s/split status-line #":")))

             status (scip-statuses status-text :unknown)

             [next-line & lines] lines
             ]

         (if (or (not next-line)
                 (= next-line "no-solution-available")
                 (= next-line "no solution available")
                 (not (.startsWith next-line "objective value:")))
           {:solution {:exists false :reason status}}
           (let [objective-value (s/trim (second (s/split next-line #":")))
                 objective-value (try (Double/parseDouble objective-value)
                                      (catch Exception e
                                        (throw (ex-info "Parsing objective value line"
                                                        {:line next-line}
                                                        e))))
                 ]
             (cond->
                 {:solution {:exists  (not (#{:unbounded :infeasible} status))
                             :reason  status
                             :optimal (= status :optimal)}
                  :vars
                  (into {}
                        (for [line lines]
                          (let [[x v] (s/split line #" +")
                                v     (try (Double/parseDouble v)
                                           (catch Exception e
                                             (throw (ex-info
                                                     "Error parsing value line in output"
                                                     {:line line}
                                                     e)))
                                           
                                           )
                                x     (get var-index x x)]
                            [x {:value v}])))
                  }
               (number? objective-value) (assoc-in [:solution :value] objective-value)))))
       (catch Exception e
         (clojure.stacktrace/print-throwable e)
         (println)
         (clojure.stacktrace/print-stack-trace e)
         (println)
         (println (slurp f))
         {:solution {:exists false
                     :reason :error
                     :error  (str "Solution parse error: " (.getMessage e)
                                  )}})))

(def emphasis-values
  #{:counter :cpsolver :easycip
    :feasibility :hardlp :numerics
    :optimality})

(def presolving-emphasis-values
  #{:aggressive :default :fast :off})

(def heuristics-emphasis-values
  #{:aggressive :default :fast :off})

(def ^:dynamic *default-solver-arguments* {:scip "scip"})

(defn version [& {:keys [scip] :or {scip "scip"}}]
  (some->
   (some->>
    (sh/sh scip "-v")
    (:out)
    (re-find #"SCIP version ([0-9.]+)"))
   (nth 1)
   (s/split #"\.")
   (->> (map #(Integer/parseInt %)))
   (vec)))

(defn solve* [lp {:keys [scip
                         instructions
                         presolving-emphasis
                         heuristics-emphasis
                         emphasis]
                  :or   {scip                (:scip *default-solver-arguments*)
                         instructions        (:instructions *default-solver-arguments*)
                         presolving-emphasis (:presolving-emphasis *default-solver-arguments*)
                         heuristics-emphasis (:heuristics-emphasis *default-solver-arguments*)
                         emphasis            (:emphasis *default-solver-arguments*)}
                  :as   settings}]
  {:pre [(or (nil? emphasis)
             (emphasis-values emphasis))
         (or (nil? presolving-emphasis)
             (presolving-emphasis-values presolving-emphasis))
         (or (nil? heuristics-emphasis)
             (heuristics-emphasis-values heuristics-emphasis))]}
  
  (let [{problem-text  :program var-index :index-to-var
         constant-term :constant-term}
        (lpio/cplex lp)

        instructions
        (cond-> instructions
          presolving-emphasis
          (conj (str "set presolving emphasis " (name presolving-emphasis)))

          heuristics-emphasis
          (conj (str "set heuristics emphasis " (name heuristics-emphasis)))

          emphasis
          (conj (str "set emphasis " (name emphasis))))
        ]
    (lpio/with-temp-dir temp
      (spit (io/file temp "problem.lp") problem-text)
      (spit (io/file temp "scip.set")
            (format-settings (dissoc settings
                                     :scip
                                     :instructions
                                     :presolving-emphasis
                                     :heuristics-emphasis
                                     :emphasis)))
      (spit (io/file temp "commands.txt")
            (s/join
             "\n"
             `["read problem.lp"

               ~@instructions
               
               "optimize"
               "set write printzeros true"
               "write solution solution.txt"
               "write statistics statistics.txt"
               "quit"]))
      (let [version (version :scip scip)

            {:keys [exit out err]}
            (sh/sh scip "-s" "scip.set" "-b" "commands.txt" :dir temp)
            
            output-file (io/file temp "solution.txt")
            stats-file  (io/file temp "statistics.txt")
            
            log (s/join "--\n" [out err])

            solution
            (cond
              (not (zero? exit))
              {:solution {:exists false
                          :reason :error
                          :error  exit
                          :log    log}}

              (not (.exists output-file))
              {:solution {:exists false
                          :reason :error
                          :error  "No output file created"
                          :log    log}}

              :else
              (let [outputs    (parse-output-file output-file var-index)
                    statistics (parse-statistics-file stats-file)]
                (-> outputs
                    (update :solution merge
                            {:log log}
                            statistics)
                    ;; SCIP 7 leaves off the constant term in the objective
                    ;; SCIP 9 doesn't. Not sure about scip 8.
                    (cond->
                        (< (or (first version) 7) 9)
                        (update-in [:solution :value]
                                   (fn [a] (when a (+ a (or constant-term 0))))))
                    (update-in [:solution :value]
                               * (:objective-scale lp 1.0)))))
            ]
        (lp/merge-results lp solution)))))


(defn solve [lp & {:keys [scip
                          instructions
                          presolving-emphasis
                          heuristics-emphasis
                          emphasis]
                   :or   {scip                (:scip *default-solver-arguments*)
                          instructions        (:instructions *default-solver-arguments*)
                          presolving-emphasis (:presolving-emphasis *default-solver-arguments*)
                          heuristics-emphasis (:heuristics-emphasis *default-solver-arguments*)
                          emphasis            (:emphasis *default-solver-arguments*)}
                   :as   settings}]
  (solve* lp settings))

(defn minuc
  "Solves the min unsatisfied constraints version of lp.
  Useful for diagnoising why your program is infeasible."
  [lp & {:keys [scip]
                   :or {scip "scip"}
                   :as settings}]

  (let [{problem-text :program var-index :index-to-var normed-lp :lp}
        (lpio/cplex lp)]
    (lpio/with-temp-dir temp
      (spit (io/file temp "problem.lp") problem-text)
      (spit (io/file temp "scip.set") (format-settings (dissoc settings :scip)))
      (spit (io/file temp "commands.txt")
            "read problem.lp
change minuc
optimize
set write printzeros false
write solution solution.txt
write statistics statistics.txt
quit")
      (let [{:keys [exit out err]}
            (sh/sh scip "-s" "scip.set" "-b" "commands.txt" :dir temp)

            output-file (io/file temp "solution.txt")
            stats-file  (io/file temp "statistics.txt")
            
            log (s/join "--\n" [out err])
            cons (lp/all-constraints normed-lp)
            ]
        (cond
              (not (zero? exit))
              (throw (ex-info "Unable to find minuc solution" {:exit exit}))

              (not (.exists output-file))
              (throw (ex-info "Unable to find minuc solution - no output" {:exit exit}))
              
              :else
              (let [outputs (parse-output-file output-file var-index)
                    statistics (parse-statistics-file stats-file)
                    vars (:vars outputs)
                    vars (reduce dissoc vars (keys (:vars normed-lp)))
                    ]
                
                (for [^String var-name (sort (keys vars))]
                  (let [n (.substring var-name 1 (.indexOf var-name "_" 1))
                        n (Integer/parseInt n)]
                    (let [con (nth cons n)]
                      (:lp/input (meta con) con))
                    ))))))))
