(ns lp.scip
  (:require [lp.core :as lp]
            [lp.io :as lpio]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as s]))

(def scip-setting-keys
  {:time-limit "limits/time"
   :mip-gap "limits/gap"})

(defn solve [lp & {:keys [scipampl]
                   :or {scipampl "scipampl"}
                   :as settings}]
  (let [{problem-text :program var-index :index-to-var
         evaluator :evaluator}
        (lpio/nl lp)
        
        result-text
        (lpio/with-temp-dir temp-dir
          (spit (io/file temp-dir "problem.nl") problem-text)
          (spit (io/file temp-dir "scip.set")
                (s/join
                 "\n"
                 (for [[k v] (dissoc settings :scipampl)]
                   (format "%s = %s" (scip-setting-keys k k) v))))
          (sh/sh
           ;; need right args here
           scipampl "problem.nl" "scip.set" :dir temp-dir)
          (slurp (io/file temp-dir "problem.sol")))

        ;; need solution format here
        result (lpio/sol result-text var-index)

        result
        (cond
          (#{:feasible :optimal} (-> result :solution :status))
          (assoc-in result [:solution :value]
                    (evaluator (:vars result)))

          true
          result)
        ]
    (lp/merge-results lp result)))

;; "/nix/store/ijhl69ka24hwg58hg3pmak0zdrkd49y9-run-solver-env/bin/scipampl"

