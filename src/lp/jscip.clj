;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns lp.jscip
  "Drive SCIP in-process through its C API using JNA, rather than by
  running the scip binary as lp.scip does.

  This needs a shared libscip. It is found by looking at, in order:

  - :library in *default-solver-arguments*
  - the system property lp.jscip.library
  - the environment variable SCIP_LIBRARY
  - JNA's normal search for \"scip\" (jna.library.path, LD_LIBRARY_PATH etc.)

  On nix the scipoptsuite package is statically linked, but scipopt-scip
  has a libscip.so, so you can do

  export SCIP_LIBRARY=$(nix build --no-link --print-out-paths nixpkgs#scipopt-scip)/lib/libscip.so
  "
  (:require [lp.core :as lp]
            [lp.io :as lpio]
            [lp.scip :as scip]
            [clojure.java.io :as io])
  (:import [com.sun.jna NativeLibrary Function Pointer]
           [com.sun.jna.ptr PointerByReference IntByReference]))

(def ^:dynamic *default-solver-arguments* {})

(def ^:private libraries (atom {}))

(defn- library-name []
  (or (:library *default-solver-arguments*)
      (System/getProperty "lp.jscip.library")
      (System/getenv "SCIP_LIBRARY")
      "scip"))

(defn- library ^NativeLibrary []
  (let [n (library-name)]
    (or (get @libraries n)
        (get (swap! libraries assoc n (NativeLibrary/getInstance n)) n))))

(defn- function ^Function [^String fname]
  (.getFunction (library) fname))

(defn- call-int [fname & args]
  (.invokeInt (function fname) (object-array args)))

(defn- call-double [fname & args]
  (.invokeDouble (function fname) (object-array args)))

(defn- call-ptr ^Pointer [fname & args]
  (.invokePointer (function fname) (object-array args)))

(defn- call-string ^String [fname & args]
  (.invokeString (function fname) (object-array args) false))

(defn- call-void [fname & args]
  (.invokeVoid (function fname) (object-array args)))

(def ^:private SCIP_OKAY 1)

(defn- check [fname & args]
  (let [rc (apply call-int fname args)]
    (when-not (== SCIP_OKAY rc)
      (throw (ex-info (str fname " failed with SCIP_RETCODE " rc)
                      {:function fname :retcode rc})))))

(defn- bool [x] (int (if x 1 0)))

(def status->keyword
  "From scip/type_stat.h, onto the same keywords as lp.scip/scip-statuses"
  {0  :unknown
   1  :optimal
   2  :infeasible
   3  :unbounded
   4  :infeasible
   10 :interrupt
   11 :killed
   20 :node-limit
   21 :total-node-limit
   22 :stall-node-limit
   23 :time-limit
   24 :memory-limit
   25 :gap-limit
   26 :primal-limit
   27 :dual-limit
   28 :solution-limit
   29 :improvement-limit
   30 :restart-limit})

(def ^:private emphasis->int
  {:cpsolver 1 :easycip 2 :feasibility 3 :hardlp 4
   :optimality 5 :counter 6 :numerics 10})

(def ^:private setting->int
  {:default 0 :aggressive 1 :fast 2 :off 3})

(defn version []
  [(call-int "SCIPmajorVersion")
   (call-int "SCIPminorVersion")
   (call-int "SCIPtechVersion")])

(defn- param-name [k]
  (let [k (scip/scip-setting-keys k k)]
    (if (keyword? k) (subs (str k) 1) (str k))))

(defn- set-param [scip k v]
  (let [n     (param-name k)
        param (call-ptr "SCIPgetParam" scip n)]
    (when-not param
      (throw (ex-info (str "Unknown SCIP parameter " n) {:parameter k :value v})))
    (case (call-int "SCIPparamGetType" param)
      0 (check "SCIPsetBoolParam" scip n (bool v))
      1 (check "SCIPsetIntParam" scip n (int v))
      2 (check "SCIPsetLongintParam" scip n (long v))
      3 (check "SCIPsetRealParam" scip n (double v))
      4 (check "SCIPsetCharParam" scip n (byte (int (first (str v)))))
      5 (check "SCIPsetStringParam" scip n (str v)))))

(defmacro ^:private with-scip
  "Bind `var` to a fresh SCIP instance with default plugins and an
  empty problem, freeing it afterwards."
  [[var] & body]
  `(let [ref# (PointerByReference.)]
     (check "SCIPcreate" ref#)
     (try
       (let [~var (.getValue ref#)]
         (check "SCIPincludeDefaultPlugins" ~var)
         (check "SCIPcreateProbBasic" ~var "problem")
         ;; otherwise SCIP installs its own SIGINT handler in the JVM
         (set-param ~var "misc/catchctrlc" false)
         ~@body)
       (finally
         (check "SCIPfree" ref#)))))

(defn- build-model
  "Add the variables and constraints of normalized `lp` to `scip`.
  Returns a vector of [var-name SCIP_VAR*] in creation order."
  [scip lp]
  (let [inf       (call-double "SCIPinfinity" scip)
        free-vars (into {} (filter (comp not :fixed second) (:vars lp)))
        objective (lp/gradient-double (:objective lp))
        vars      (vec
                   (map-indexed
                    (fn [i [v {:keys [lower upper type]}]]
                      (let [ref (PointerByReference.)]
                        (check "SCIPcreateVarBasic" scip ref (format "x_%d" i)
                               (double (or lower (- inf)))
                               (double (or upper inf))
                               (double (get objective v 0.0))
                               (int (case type :binary 0 :integer 1 3)))
                        (check "SCIPaddVar" scip (.getValue ref))
                        [v (.getValue ref)]))
                    free-vars))
        var-index (into {} vars)]
    (check "SCIPsetObjsense" scip (int (if (= :maximize (:sense lp)) -1 1)))
    (doseq [[index {body :body ub :upper lb :lower}]
            (map-indexed vector (lp/all-constraints lp))]
      (let [constant-term (or (lp/constant-double body) 0)
            grad          (remove (comp zero? second) (lp/gradient-double body))
            ref           (PointerByReference.)]
        (doseq [[x _] grad]
          (when-not (var-index x)
            (throw (ex-info "A variable has appeared in an expression which was not in the variable list" {:variable x}))))
        (check "SCIPcreateConsBasicLinear" scip ref (format "C%d" index)
               (int (count grad))
               (when (seq grad) (into-array Pointer (map (comp var-index first) grad)))
               (when (seq grad) (double-array (map second grad)))
               (double (if lb (- lb constant-term) (- inf)))
               (double (if ub (- ub constant-term) inf)))
        (check "SCIPaddCons" scip (.getValue ref))
        (check "SCIPreleaseCons" scip ref)))
    vars))

(defn- release-vars [scip vars]
  (doseq [[_ ^Pointer p] vars]
    (check "SCIPreleaseVar" scip (PointerByReference. p))))

(defn- add-partial-solution
  "Give SCIP the :value of each var in `lp` as a partial solution for warm starting"
  [scip lp vars]
  (let [ref (PointerByReference.)]
    (check "SCIPcreatePartialSol" scip ref nil)
    (doseq [[v p] vars
            :let [value (:value (get (:vars lp) v))]
            :when (some? value)]
      (check "SCIPsetSolVal" scip (.getValue ref) p
             (double (cond (true? value) 1.0
                           (false? value) 0.0
                           :else value))))
    (check "SCIPaddSolFree" scip ref (IntByReference.))))

(defn- run-solve
  "SCIPsolve on another thread, so that if this thread is interrupted
  we can ask SCIP to stop, like lp.proc/run does to its process."
  [scip]
  (let [result (future (call-int "SCIPsolve" scip))]
    (try
      (let [rc @result]
        (when-not (== SCIP_OKAY rc)
          (throw (ex-info (str "SCIPsolve failed with SCIP_RETCODE " rc)
                          {:function "SCIPsolve" :retcode rc}))))
      (catch InterruptedException e
        (call-int "SCIPinterruptSolve" scip)
        (try (deref result) (catch Exception _))
        (throw e)))))

(defn- read-solution
  "Get the solution state out of `scip` in the form lp.core/merge-results wants"
  [scip vars]
  (let [status (status->keyword (call-int "SCIPgetStatus" scip) :unknown)
        sol    (call-ptr "SCIPgetBestSol" scip)]
    (if-not sol
      {:solution {:exists false :reason status}}
      {:solution {:exists  (not (#{:unbounded :infeasible} status))
                  :reason  status
                  :optimal (= status :optimal)
                  :value   (call-double "SCIPgetSolOrigObj" scip sol)
                  :gap     (call-double "SCIPgetGap" scip)
                  :bounds  [(call-double "SCIPgetDualbound" scip)
                            (call-double "SCIPgetPrimalbound" scip)]}
       :vars
       (into {}
             (for [[v p] vars]
               [v {:value (call-double "SCIPgetSolVal" scip sol p)}]))})))

(defn solve* [lp {:keys [instructions
                         presolving-emphasis
                         heuristics-emphasis
                         emphasis
                         warm-start?]
                  :or   {instructions        (:instructions *default-solver-arguments*)
                         presolving-emphasis (:presolving-emphasis *default-solver-arguments*)
                         heuristics-emphasis (:heuristics-emphasis *default-solver-arguments*)
                         emphasis            (:emphasis *default-solver-arguments*)
                         warm-start?         false}
                  :as   settings}]
  {:pre [(or (nil? emphasis)
             (scip/emphasis-values emphasis))
         (or (nil? presolving-emphasis)
             (scip/presolving-emphasis-values presolving-emphasis))
         (or (nil? heuristics-emphasis)
             (scip/heuristics-emphasis-values heuristics-emphasis))]}
  (when (seq instructions)
    (throw (ex-info "lp.jscip does not support :instructions, as there is no SCIP shell"
                    {:instructions instructions})))
  (let [normed-lp     (lp/normalize lp)
        constant-term (lp/constant-double (:objective normed-lp))
        params        (dissoc settings
                              :library
                              :instructions
                              :presolving-emphasis
                              :heuristics-emphasis
                              :emphasis
                              :decimals
                              :warm-start?)]
    (lpio/with-temp-dir temp
      (let [log-file (io/file temp "log.txt")
            solution
            (try
              (with-scip [scip]
                (call-void "SCIPsetMessagehdlrQuiet" scip (bool true))
                (call-void "SCIPsetMessagehdlrLogfile" scip (.getCanonicalPath log-file))
                (let [vars (build-model scip normed-lp)]
                  (try
                    (doseq [[k v] params] (set-param scip k v))
                    (when emphasis
                      (check "SCIPsetEmphasis" scip (int (emphasis->int emphasis 0)) (bool true)))
                    (when presolving-emphasis
                      (check "SCIPsetPresolving" scip (int (setting->int presolving-emphasis)) (bool true)))
                    (when heuristics-emphasis
                      (check "SCIPsetHeuristics" scip (int (setting->int heuristics-emphasis)) (bool true)))
                    (when warm-start?
                      (add-partial-solution scip normed-lp vars))
                    (run-solve scip)
                    (read-solution scip vars)
                    (finally (release-vars scip vars)))))
              (catch clojure.lang.ExceptionInfo e
                (if (:retcode (ex-data e))
                  {:solution {:exists false
                              :reason :error
                              :error  (.getMessage e)}}
                  (throw e))))

            log (when (.exists log-file) (slurp log-file))]
        (lp/merge-results
         lp
         (-> solution
             (assoc-in [:solution :log] log)
             (update-in [:solution :value]
                        (fn [a] (when a (* (:objective-scale lp 1.0)
                                           (+ a (or constant-term 0))))))))))))

(defn solve [lp & {:keys [presolving-emphasis
                          heuristics-emphasis
                          emphasis]
                   :or   {presolving-emphasis (:presolving-emphasis *default-solver-arguments*)
                          heuristics-emphasis (:heuristics-emphasis *default-solver-arguments*)
                          emphasis            (:emphasis *default-solver-arguments*)}
                   :as   settings}]
  (solve* lp settings))

(defn minuc
  "Solves the min unsatisfied constraints version of lp.
  Useful for diagnoising why your program is infeasible."
  [lp & {:as settings}]
  (let [normed-lp (lp/normalize lp)
        cons      (lp/all-constraints normed-lp)]
    (with-scip [scip]
      (call-void "SCIPsetMessagehdlrQuiet" scip (bool true))
      (let [vars (build-model scip normed-lp)]
        (try
          (doseq [[k v] (dissoc settings :library)] (set-param scip k v))
          (let [success (IntByReference.)]
            (check "SCIPtransformMinUC" scip success)
            (when (zero? (.getValue success))
              (throw (ex-info "Unable to transform to minuc problem" {}))))
          (run-solve scip)
          (let [sol (call-ptr "SCIPgetBestSol" scip)
                _   (when-not sol
                      (throw (ex-info "Unable to find minuc solution - no solution" {})))
                own-vars (set (map second vars))
                n-vars   (call-int "SCIPgetNOrigVars" scip)
                all-vars (when (pos? n-vars)
                           (.getPointerArray (call-ptr "SCIPgetOrigVars" scip) 0 (int n-vars)))]
            (doall
             (for [^String var-name
                   (sort
                    (for [p all-vars
                          :when (not (own-vars p))
                          :when (> (call-double "SCIPgetSolVal" scip sol p) 0.5)]
                      (call-string "SCIPvarGetName" p)))]
               (let [n (Integer/parseInt (.substring var-name 1 (.indexOf var-name "_" 1)))
                     con (nth cons n)]
                 (:lp/input (meta con) con)))))
          (finally (release-vars scip vars)))))))
