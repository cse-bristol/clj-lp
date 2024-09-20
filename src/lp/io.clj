;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns lp.io
  (:require [lp.core :as lp]
            [clojure.java.io :as io]
            [clojure.set :refer [map-invert]]
            [clojure.string :as s])
  (:import [java.nio.file Path Files]))

(defn delete-files
  [& fs]
  (when-let [f (first fs)]
    (if-let [cs (seq (.listFiles (io/file f)))]
      (recur (concat cs fs))
      (do (io/delete-file f)
          (recur (rest fs))))))

(def ^:dynamic *keep-temp-dir*
  (= "true" (System/getProperty "lp.io/*keep-temp-dir*")))

(defmacro with-temp-dir [var & body]
  `(let [~var (.toFile
               (Files/createTempDirectory
                "linear-program"
                (into-array java.nio.file.attribute.FileAttribute [] )))]
     (try
       (do ~@body)
       (finally
         (if *keep-temp-dir*
           (binding [*out* *err*]
             (println "Keeping temporary files in" (.getName ~var)))
           (delete-files ~var))
         ))))

(defmacro doindexed
  "Iterate for ITEM in SEQUENCE, with INDEX counting from 0"
  {:style/indent 1}
  [[index item sequence] & body]
  `(loop [~index 0
          items# (seq ~sequence)]
     (when-not (empty? items#)
       (do (let [~item (first items#)] ~@body)
           (recur (inc ~index) (rest items#))))))

(defn cplex
  "Write a cplex input file; actually for glpk, after

  https://hpc.nih.gov/apps/glpk/latest/glpk.pdf

  Given `lp`, returns {:program \"cplex text\" :vars {0 :x 1 :y etc}
  "
  [lp & {:keys [comments var-names decimals] :or {decimals 5}}]
  (let [lp         (lp/normalize lp)

        df (java.text.DecimalFormat. (str "0." (.repeat "#" decimals)))
        
        free-vars  (into {} (filter (comp not :fixed second) (:vars lp)))
        var-order  (map-indexed
                    (if var-names
                      (fn [i v]
                        (let [x (str "x_" i "_"
                                    (.replaceAll (str v) "[^A-Za-z0-9]+" "_"))]
                          [x v]))
                      (fn [i v] [(format "x_%d" i) v]))
                    (keys free-vars))
        var-rindex (into {} var-order)
        var-index  (map-invert var-rindex)

        print-sum
        (fn [^StringBuffer sb sum & {:keys [constant-value new-lines]}]
          (let [grad (lp/gradient-double sum)]
            (doseq [[x k] grad]
              (when-not (zero? k)
                (if (>= k 0)
                  (.append sb "+ ")
                  (.append sb "- "))
                
                (when-not (== 1 (Math/abs (double k)))
                  (.append sb (.format df (Math/abs (double k))))
                  (.append sb " "))
                
                (let [i (get var-index x)]
                  (when-not i
                    (throw (ex-info "A variable has appeared in an expression which was not in the variable list" {:variable x})))
                  (.append sb (.toString i)))

                (.append sb (if new-lines
                              "\n    "
                              " "))))
            
            (when constant-value
                (let [c (lp/constant-double sum)]
                  (when-not (zero? c)
                    (if (>= c 0)
                      (.append sb "+ ")
                      (.append sb "- "))
                    (.append sb (.format df (Math/abs (double c)))))))))
        ]

    {:index-to-var var-rindex
     :var-to-index var-index
     :constant-term (lp/constant-double (:objective lp))
     :lp lp
     
     :program
     (let [sb (StringBuffer.)]
       (.append sb (name (:sense lp)))
       (.append sb  " ")
       (print-sum sb (:objective lp) :constant-value true :new-lines true)

       ;; constraints
       (when-let [cons (seq (lp/all-constraints lp))]
         (.append sb "\n\nsubject to\n")
         (doseq [[index constraint] (map-indexed vector cons)]
           (let [{body :body ub :upper lb :lower} constraint
                 
                 constant-term (or (lp/constant-double body) 0)
                 lb (and lb (- lb constant-term))
                 ub (and ub (- ub constant-term))
                 ]
             (when comments
               (.append sb
                        (str "\\ Constraint " index
                             "=>" (:input (meta constraint))
                             "\n")))
             (.append sb (format "C%d: " index))
             (print-sum sb body)
             (cond
               (and ub lb (== ub lb))
               (doto sb
                 (.append " = ")
                 (.append (.format df ub))
                 (.append "\n"))

               (and ub (not lb))
               (doto sb
                 (.append " <= ")
                 (.append (.format df ub))
                 (.append "\n"))

               (and lb (not ub))
               (doto sb
                 (.append " >= ")
                 (.append (.format df lb))
                 (.append "\n"))

               (and lb ub)
               (do (doto sb
                     (.append "<= ")
                     (.append (.format df ub))
                     (.append "\n"))
                   (print-sum sb body)
                   (doto sb
                     (.append ">= ")
                     (.append (.format df lb))
                     (.append "\n"))
                   )))))

       ;; variable bounds
       (.append sb "\nbounds\n")
       (doseq [[i v] var-order]
         (let [{lb :lower ub :upper} (get (:vars lp) v)]
           (cond
             (and lb ub (== lb ub))
             (.append sb (format "%s = %s\n" i (.format df lb)))

             (and (not lb) (not ub))
             (.append sb  (format "%s free\n" i))

             (not ub)
             (.append sb (format "%s >= %s\n" i (.format df lb)))
             
             (not lb)
             (.append sb (format "%s <= %s\n" i (.format df ub)))
             
             :else
             (.append sb (format
                          "%s <= %s <= %s\n"
                          (.format df lb) i (.format df ub))))))

       ;; variable types
       (let [{int-vars :integer
              bin-vars :binary}
             (group-by (comp :type second) free-vars)]
         (when (seq int-vars)
           (.append sb "integer\n")
           (doseq [n (map first int-vars)]
             (.append sb (format "%s\n" (get var-index n)))))
         (when (seq bin-vars)
           (.append sb "binary\n")
           (doseq [n (map first bin-vars)]
             (.append sb (format "%s\n" (get var-index n))))))
       
       (.append sb "end\n")

       (.toString sb)
       )}))

(comment
  (defn nl
    "Format an nl file for an lp.
  After https://cfwebprod.sandia.gov/cfdocs/CompResearch/docs/nlwrite20051130.pdf

  Return a map having {:program and :vars}

  :program is the nl file as a string
  :vars is a map from variable number to variable name in the input program.

  Currently requires that the input program be linear, although nl files do not.

  May not work right - seems to make scipampl die with large programs
  "
    [lp]
    ;; precondition - lp is linear?
    (let [lp (lp/normalize lp)

          var-order
          (vec
           (sort-by
            (fn [[_ var]]
              (case (:type var)
                :binary 1
                :integer 2
                0))
            (remove (comp :fixed second) (:vars lp))))
          
          index-to-var (into {}  (map-indexed vector (map first var-order)))
          var-to-index (map-invert index-to-var)

          n-vars (count var-order)
          
          {n-binary-vars :binary n-integer-vars :integer}
          (frequencies (map (comp :type second) var-order))

          constraints (lp/all-constraints lp)
          
          n-constraints       (count constraints)
          n-objectives        1
          n-range-constraints 0
          n-eq-constraints    (count (filter #{:=} (map first constraints)))
          
          n-linear-constraints n-constraints

          count-variables
          (fn [x] (count (lp/gradient-double x)))
          
          nz-jacobians  (reduce + 0 (for [c constraints] (count-variables (:body c))))
          gradients     (count-variables (:objective lp))
          
          print-gradient
          (fn [x]
            (let [gradient (lp/gradient-double x)]
              (doseq [[x k] gradient]
                (when-let [i (var-to-index x)]
                  (println i k)))))

          print-bound
          (fn [lb ub thing]
            (cond
              (and lb ub (== lb ub))
              (printf "4 %s\n" lb)

              (and ub (not lb))
              (printf "1 %s\n" ub)
              
              (and lb (not ub))
              (printf "2 %s\n" lb)

              (and lb ub)
              (printf "0 %s %s\n" lb ub)

              (and (not lb) (not ub))
              (println "3") ;; free
              
              ;; TODO other types
              :else (throw (ex-info "Bounds type not implemented" {:thing thing}))
              ))

          count-constraints
          (fn [var-name]
            (count (filter
                    (fn [{body :body}] (contains? (lp/gradient-double body) var-name))
                    constraints)))
          ]
      
      {:index-to-var index-to-var
       :var-to-index var-to-index
       :evaluator
       (let [obj (:objective lp)
             C (lp/constant-double obj)
             G (lp/gradient-double obj)]
         (if (empty? G)
           (constantly C)
           (fn [vars]
             (cond
               (empty? vars)
               nil

               (every?
                number?
                (map (comp :value vars) (keys G)))
               (reduce-kv
                (fn [a x coeff]
                  (+ a (* coeff (:value (vars x)))))
                C G)

               :else
               (do (println "Missing some vars in solution?")
                   (println "Given: " vars)
                   (println "Missing: " (remove
                                         (comp number? :value vars)
                                         (keys G))))
               ))))
       
       :program
       (with-out-str
         (println "g3 1 1 0") ;; I don't know what 3 1 1 0 is

         (doseq [line
                 [[n-vars n-constraints n-objectives n-range-constraints n-eq-constraints]
                  [0 0] ;; nonlinear constraints and objectives
                  [0 0] ;; network constraints
                  [0 0 0] ;; nonlinear variables
                  [0 0 0 1] ;; flags? what's flags?
                  [(or n-binary-vars 0) (or n-integer-vars 0)
                   0 0 0] ;; discrete vars: binary, int, nonlinear b,c,o
                  [nz-jacobians gradients] ;; gradients is number of vars in objective
                  [0 0] ;; max name lengths
                  [0 0 0 0 0] ;; common expressions
                  ]]
           (doseq [val line]
             (print " ")
             (print val))
           (println))

         (doindexed [i constraint constraints]
                    (printf "C%d\n" i)
                    ;; TODO I think this below is wrong, but not a problem
                    (printf "n%s\n" (lp/constant-double (:body constraint)))
                    )

         ;; objective sense
         (printf "O0 %d\n" (if (= :maximize (:sense lp)) 1 0))

         ;; objective body
         (printf "n%s\n" (lp/constant-double (:objective lp)))

         ;; dual and primal values for variables
         ;; x2
         ;; v0 val0
         ;; v2 val2 ...
         ;; d1
         ;; v3 dual3 etc

         (println "r # constraint ranges")
         (doseq [{lb :lower ub :upper :as c} constraints]
           ;; nlwrite.pdf, table 17

           ;; 0 l u  => l <= body <= u
           ;; 1 u => body <= u
           ;; 2 l => l <= body
           ;; 3 => free
           ;; 4 c => body = c
           ;; 5 k i => body complements vi-1 (what's k)
           (print-bound lb ub c))

         (println "b # variable bounds")
         (doseq [[var-name var] var-order]
           ;; this is also like table 17
           (let [{lower :lower upper :upper} var]
             (print-bound lower upper (assoc var :name var-name))))

         ;; jacobian sparsity information
         (printf "k%d # intermediate jacobian col lengths\n"
                 (dec (count var-order)))
         ;; number of constraints containing var 0
         ;; number of constraints containing var 1
         ;; and so on
         (let [acc (atom 0)]
           (doseq [[var-name _] (pop var-order)]
             (let [n (count-constraints var-name)]
               (println (swap! acc + n)))))

         ;; jacobian
         (doindexed [i constraint constraints]
                    (let [{body :body} constraint]
                      (printf "J%d %d\n" i (count-variables body))
                      (print-gradient body)
                      ))

         ;; objective gradients
         (let [objective (:objective lp)
               nvars     (count-variables objective)
               ]
           (printf "G0 %d\n" nvars)
           (print-gradient objective)
           ))
       }
      )))

(defn sol
  "Read a sol file into some vars
  Sol files are not well-documented

  https://github.com/Pyomo/pyomo/blob/0c13b88e6cb9d42ff415dfbc896dc3902dd7dc7d/pyomo/opt/plugins/sol.py as a reference.

  Sol file format seems to be

  Arbitrary junk

  Options                                  | A line that starts options
  n                                        | an integer; if > 4 something weird?
  o1                                       | option val 1
  o2                                       | ...
  o(n+1)                                   | [nopts]
  o(n+2)                                   | # constraints M
  o(n+3)                                   |
  o(n+4)                                   | # vars N
  M floats (constraint vals?)
  N floats (variable vals?)
  objno x y  (x and y are codes)
  "
  [text var-index]
  (try (let [lines              (remove s/blank? (s/split-lines text))
             [junk _ lines]     (partition-by #{"Options"} lines)
             [num-opts & lines] lines
             num-opts           (Integer/parseInt num-opts)
             weird              (> num-opts 4)                               ;; no idea what this means
             num-opts           (if weird (- num-opts 2) num-opts)
             [opt-vals lines]   (split-at (+ (if weird 5 4) num-opts) lines) ;; not sure why + 4

             ;; these are the only two values in here that we use
             n-vars (Integer/parseInt (nth opt-vals (+ num-opts 3)))
             n-cons (Integer/parseInt (nth opt-vals (+ num-opts 1)))

             [cons-vals lines] (split-at n-cons lines)
             [var-vals lines]  (split-at n-vars lines)

             read-double #(Double/parseDouble %)
             cons-vals   (map read-double cons-vals)
             var-vals    (map read-double var-vals)

             [objno & lines] lines

             [_ sc1 sc2] (s/split objno #" ")

             sc1 (Integer/parseInt sc1)
             sc2 (Integer/parseInt sc2)
             ]
         {:vars
          (->> (map-indexed
                (fn [i v]
                  [(get var-index i)
                   {:value v}])
                var-vals)
               (into {}))

          :solution
          {:exists  (<= 0 sc2 199)
           :reason  (cond
                      (<= 0 sc2 99)
                      :optimal
                      
                      (<= 200 sc2 299)
                      :infeasible

                      (<= 300 sc2 399)
                      :unbounded
                      
                      true
                      :unknown)
           }})

       (catch Exception e
         {:solution
          {:exists false
           :reason :error
           :error (.getMessage e)}}))
  )

(comment
  (println
   (:program
    (nl {:vars {:x {:type :non-negative}
                :y {:type :binary}
                :z {:type :non-negative}}
         :objective [:+ :x [:* 3 :y] [:- [:* 2 :z]]]
         :sense :maximize
         :constraints {:con1 [:<= :x [:* 2 :z]] 
                       :con2 [:and
                              [:= 1 :z]
                              [:>= :z :y]]
                       :con3 [:<= 3 [:- :x :y] 19]
                       }
         }))))
