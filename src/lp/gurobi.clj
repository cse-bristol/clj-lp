;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns lp.gurobi
  (:require [lp.core :as lp]
            [lp.io :as lpio]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as s]
            [clojure.data.json :as json]
            [malli.core :as m]
            [malli.transform :as mt]))

(def status->keyword
  {1  :loaded
   2  :optimal
   3  :infeasible
   4  :infeasible-or-unbounded
   5  :unbounded
   6  :cutoff
   7  :iteration-limit
   8  :node-limit
   9  :time-limit
   10 :solution-limit
   11 :interrupted
   12 :numeric
   13 :suboptimal
   14 :in-progress
   15 :user-objective-limit
   16 :work-limit
   17 :memory-limit})

(def GurobiOutput
  (m/schema
   [:map
    [:SolutionInfo
     [:map
      [:Status
       [:enum
        {:decode/json (fn [x] (status->keyword x))}
        :loaded
        :optimal
        :infeasible
        :infeasible-or-unbounded
        :unbounded
        :cutoff
        :iteration-limit
        :node-limit
        :time-limit
        :solution-limit
        :interrupted
        :numeric
        :suboptimal
        :in-progress
        :user-objective-limit
        :work-limit
        :memory-limit]]
      
      [:Runtime :double]
      [:ObjVal {:optional true} :double]
      [:ObjBound {:optional true} :double]
      [:MIPGap {:optional true} :double]]]

    [:Vars {:optional true}
     [:vector
      [:map
       [:VarName :string]
       [:X :double]]]]]))

(let [decode (m/decoder
              GurobiOutput
              mt/json-transformer)
      validate (m/validator GurobiOutput)
      ]
  (defn decode-and-validate [output-file]
    (let [result (-> (with-open [f (io/reader output-file)]
                       (json/read f :key-fn keyword))
                     decode
                     )]
      (assert (validate result))
      result
      )))

(defn parse-output-file [output-file variable-index]
  (let [result (decode-and-validate output-file)

        vars (into
              {}
              (for [v (:Vars result)]
                [(get variable-index (:VarName v) (:VarName v))
                 (:X v)]))

        status (-> result :SolutionInfo :Status)
        ]
    {:solution
     {:exists  (contains? (:SolutionInfo result) :ObjVal)
      :value   (some-> result :SolutionInfo :ObjVal)
      :reason  status
      :optimal (= status :optimal)
      :gap     (-> result :SolutionInfo (:MIPGap 0.0))
      :bounds  [(-> result :SolutionInfo :ObjBound)
                (-> result :SolutionInfo :ObjVal)]}
     :vars vars}))

(defn solve* [lp {:keys [gurobi time-limit mip-gap feasibility-tolerance]
                  :or   {gurobi "gurobi_cl"}}]

  (let [{problem-text :program var-index :index-to-var} (lpio/cplex lp)
        gurobi-parameters
        (cond-> ["ResultFile=solution.json"]
          time-limit (conj (format "TimeLimit=%d" (int time-limit)))
          mip-gap    (conj (format "MIPGap=%f" (double mip-gap)))
          feasibility-tolerance    (conj (format "FeasibilityTol=%s" feasibility-tolerance)))
        ]
    (lpio/with-temp-dir temp
      (spit (io/file temp "problem.lp") problem-text)

      (let [{:keys [exit out err]}
            (apply sh/sh
                   `[~gurobi
                     ~@gurobi-parameters
                     "problem.lp"
                     :dir ~temp])

            output-file (io/file temp "solution.json")

            log (s/join "--\n" [out err])

            solution
            (cond
              (not (zero? exit))
              {:solution {:exists false
                          :reason :error
                          :error  exit
                          :log    log}

               (not (.exists output-file))
               {:solution {:exists false
                           :reason :error
                           :error  "No output file created"
                           :log    log}}

               :else
               (let [outputs (parse-output-file output-file var-index)]
                 (assoc-in outputs [:solution :log] log))
               
               })]
        (lp/merge-results lp solution)))))

(defn solve [lp & {:keys [gurobi
                          time-limit
                          mip-gap
                          feasibility-tolerance]
                   :or {gurobi "gurobi_cl"}
                   :as settings}]
  (solve* lp settings))
