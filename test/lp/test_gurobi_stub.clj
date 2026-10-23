;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns lp.test-gurobi-stub
  "Tests of how we call gurobi_cl, using a stub script in place of gurobi,
  so they can run without a licence."
  (:require [clojure.test :as t]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [lp.io :as lpio]
            [lp.gurobi :as gurobi]))

(defn- run-stub
  "Solve `lp` with a stub gurobi_cl which records its arguments and any
  start file it is given. Returns {:args [...] :start \"...\" or nil}."
  [lp settings]
  (lpio/with-temp-dir record
    (let [stub (io/file record "gurobi_cl")]
      (spit stub
            (str "#!/bin/sh\n"
                 "printf '%s\\n' \"$@\" > '" record "/args'\n"
                 "if [ -f start.mst ]; then cp start.mst '" record "/start.mst'; fi\n"
                 "echo '{\"SolutionInfo\": {\"Status\": 2, \"Runtime\": 0.0, \"ObjVal\": 1.0}, \"Vars\": []}'"
                 " > solution.json\n"))
      (.setExecutable stub true)
      (gurobi/solve* lp (assoc settings :gurobi (.getPath stub)))
      (let [start (io/file record "start.mst")]
        {:args  (s/split-lines (slurp (io/file record "args")))
         :start (when (.exists start) (slurp start))}))))

(def ^:private switches
  {:maximize [:+ [:x 0] [:x 1] :y]
   :vars {:x {:indexed-by [#{0 1}] :type :binary
              :value {0 true 1 false}}
          :y {:type :non-negative :upper 10 :value 2.5}}
   :subject-to (list [:<= [:+ [:x 0] [:x 1]] 1])})

(t/deftest test-no-warm-start-by-default
  (t/testing "without the environment variable, no start is given"
    (with-redefs-fn {#'gurobi/warm-start-enabled? (constantly false)}
      #(let [{:keys [args start]} (run-stub switches {:warm-start? true})]
         (t/is (nil? start))
         (t/is (not-any? #{"InputFile=start.mst"} args)))))

  (t/testing "without warm-start?, no start is given"
    (with-redefs-fn {#'gurobi/warm-start-enabled? (constantly true)}
      #(let [{:keys [args start]} (run-stub switches {})]
         (t/is (nil? start))
         (t/is (not-any? #{"InputFile=start.mst"} args))))))

(t/deftest test-warm-start
  (with-redefs-fn {#'gurobi/warm-start-enabled? (constantly true)}
    #(let [{:keys [args start]} (run-stub switches {:warm-start? true})
           {:keys [index-to-var]} (lpio/cplex switches)
           name-of (into {} (for [[x v] index-to-var] [v x]))
           lines   (set (s/split-lines start))]
       (t/is (some #{"InputFile=start.mst"} args))
       ;; the model file comes after the parameters
       (t/is (= "problem.lp" (last args)))
       (t/is (= #{(str (name-of [:x 0]) " 1")
                  (str (name-of [:x 1]) " 0")
                  (str (name-of :y) " 2.5")}
                lines)))))

(t/deftest test-no-start-for-continuous-programs
  (with-redefs-fn {#'gurobi/warm-start-enabled? (constantly true)}
    #(let [{:keys [args start]}
           (run-stub {:maximize :y
                      :vars {:y {:type :non-negative :upper 10 :value 2.5}}}
                     {:warm-start? true})]
       (t/is (nil? start))
       (t/is (not-any? #{"InputFile=start.mst"} args)))))

(t/deftest test-mip-start-values
  (let [start (lpio/mip-start
               (lpio/cplex {:maximize [:+ :a :b :c :d]
                            :vars {:a {:type :binary :value true}
                                   :b {:type :binary :value false}
                                   :c {:type :binary}
                                   :d {:value ##NaN}}}))]
    (t/is (= 2 (count (s/split-lines start))))
    (t/is (= #{"1" "0"} (set (map #(second (s/split % #" "))
                                  (s/split-lines start)))))))
