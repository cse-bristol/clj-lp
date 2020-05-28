(ns lp.scip
  (:require [lp.core :as lp]
            [lp.io :as lpio]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as s]))

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
   }
  

  )

(defn extract-scip-status [scip-status]
  (let [[_ a b] (re-find #"(.+?) *\[(.+)\]" scip-status)]
    (println a b)
    (cond
      (= a "problem is solved")
      {:reason (scip-statuses b :unknown)}
      
      (= a "solving was interrupted")
      {:reason (scip-statuses b :unknown)
       :exists false}

      (.startsWith b "invalid status code")
      {:exists false :reason :error :error b}
      
      :else
      {}
      )
    ))


(defn solve [lp & {:keys [scipampl]
                   :or   {scipampl "scipampl"}
                   :as   settings}]
  (let [{problem-text :program var-index :index-to-var
         evaluator    :evaluator}
        (lpio/nl lp)
        
        outputs
        (lpio/with-temp-dir temp-dir
          (spit (io/file temp-dir "problem.nl") problem-text)
          (spit (io/file temp-dir "scip.set")
                (s/join
                 "\n"
                 (for [[k v] (dissoc settings :scipampl)]
                   (format "%s = %s" (scip-setting-keys k k) v))))
          (let [run
                (sh/sh
                 ;; need right args here
                 scipampl "problem.nl" "scip.set" :dir temp-dir)

                sol (slurp (io/file temp-dir "problem.sol"))
                ]
            (assoc run :sol sol)
            )
          )

        result (lpio/sol (:sol outputs) var-index)

        ;; SCIP puts some useful stuff in stdout that we can pick up
        {:keys [gap scip-status primal-bound dual-bound]}
        (extract-messages (:out outputs))

        gap          (extract-double gap)
        primal-bound (extract-double primal-bound)
        dual-bound   (extract-double dual-bound)

        result
        (cond-> result
          (#{:feasible :optimal} (-> result :solution :status))
          (assoc-in [:solution :value]
                    (evaluator (:vars result)))

          gap
          (assoc-in [:solution :gap] gap)

          (and primal-bound dual-bound)
          (assoc-in [:solution :bounds] [primal-bound dual-bound])

          scip-status
          (update :solution merge
                    (extract-scip-status scip-status)))]
    (lp/merge-results lp result)))

;; "/nix/store/ijhl69ka24hwg58hg3pmak0zdrkd49y9-run-solver-env/bin/scipampl"

