(ns lp.scip
  (:require [lp.core :as lp]
            [lp.io :as lpio]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as s]
            [clojure.test :as test]))

(def scip-setting-keys
  {:time-limit "limits/time"
   :mip-gap "limits/gap"})

(defn- extract-double [s]
  (try (Double/parseDouble (first (s/split s #" ")))
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

(defn parse-statistics-file [f]
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
              ;; TODO infinity / -infinity
              (let [[_ v] (re-find
                           #"([+-]?\d+(.\d+)?(e[+-]\d+)?)"
                           s)]
                (Double/parseDouble v))
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
                 (not (.startsWith next-line "objective value:")))
           {:solution {:exists false :reason status}}
           (let [objective-value (s/trim (second (s/split next-line #":")))
                 objective-value (try (Double/parseDouble objective-value)
                                      (catch Exception e
                                        (throw (ex-info "Parsing objective value line"
                                                        {:line next-line}
                                                        e))))
                 ]
             {:solution {:exists  true
                         :value   objective-value
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
                            x     (get var-index x)]
                        [x {:value v}])))
              })))
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

(defn solve [lp & {:keys [scip]
                   :or {scip "scip"}
                   :as settings}]
  (let [{problem-text :program var-index :index-to-var}
        (lpio/cplex lp)]
    (lpio/with-temp-dir temp
      (spit (io/file temp "problem.lp") problem-text)
      ;; (spit (io/file (format "/home/hinton/tmp/lp/%d-problem.lp"
      ;;                        (System/currentTimeMillis)
      ;;                        )) problem-text)
      (spit (io/file temp "scip.set") (format-settings (dissoc settings :scip)))
      (spit (io/file temp "commands.txt")
            "read problem.lp
optimize
set write printzeros true
write solution solution.txt
write statistics statistics.txt
quit")
      (let [{:keys [exit out err]}
            (sh/sh scip "-s" "scip.set" "-b" "commands.txt" :dir temp)

            output-file (io/file temp "solution.txt")
            stats-file  (io/file temp "statistics.txt")
            
            log (s/join "--\n" [out err])

            solution
            (cond
              (not (zero? exit))
              {:solution {:exists false
                          :reason :error
                          :error exit
                          :log log}}

              (not (.exists output-file))
              {:solution {:exists false
                          :reason :error
                          :error "No output file created"
                          :log log}}

              :else
              (-> (parse-output-file output-file var-index)
                  (update :solution merge
                          {:log log}
                          (parse-statistics-file stats-file))))
            ]
        (lp/merge-results lp solution)))))

