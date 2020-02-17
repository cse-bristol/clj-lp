(ns lp.io
  (:require [lp.core :as lp]
            [clojure.java.io :as io]
            [clojure.set :refer [map-invert]])
  
  (:import [java.nio.file Path Files]))

(defn delete-files
  [& fs]
  (when-let [f (first fs)]
    (if-let [cs (seq (.listFiles (io/file f)))]
      (recur (concat cs fs))
      (do (io/delete-file f)
          (recur (rest fs))))))

(defmacro with-temp-dir [var & body]
  `(let [~var (.toFile
               (Files/createTempDirectory
                "temp"
                (into-array java.nio.file.attribute.FileAttribute [] )))]
     (try
       (do ~@body)
       (finally (delete-files ~var)))))

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
  [lp]
  (let [lp         (lp/normalize lp)
        _          (assert (lp/linear? lp) "Only linear programs for cplex")
        var-order  (map-indexed vector (keys (:vars lp)))
        var-rindex (into {} var-order)
        var-index  (map-invert var-rindex)

        print-sum
        (fn [sum & {:keys [constant-value]}]
          (let [obj (:terms sum)]
            (doseq [[term factor] obj]
              (when-not (or (lp/is-zero? factor)
                            (and (not constant-value)
                                 (lp/is-one? term))
                            )
                (if (>= factor 0)
                  (print "+ ")
                  (print "- "))
                
                (when-not (== 1 (Math/abs factor))
                  (print (Math/abs factor))
                  (print " "))
                
                (when-not (lp/is-one? term)
                  (let [var (-> (:factors term) first first)]
                    (print (str "x" (get var-index var))))
                  
                  (print " "))))))]

    {:index-to-var var-rindex
     :var-to-index var-index
     :program
     (with-out-str
       (print (name (:sense lp)) " ")
       (print-sum (:objective lp) :constant-value true)

       ;; constraints
       (when-let [cons (seq (lp/constraint-bodies lp))]
         (println)
         (println)
         (println "subject to")
         (doseq [{body :body ub :upper lb :lower} cons]
           (let [constant-term (or (lp/constant-value body) 0)
                 lb (and lb (- lb constant-term))
                 ub (and ub (- ub constant-term))
                 ]
             (print-sum body)
             (cond
               (and ub lb (== ub lb))
               (println "=" ub)

               (and ub (not lb))
               (println "<=" ub)

               (and lb (not ub))
               (println ">="lb)

               (and lb ub)
               (do (println "=<" ub)
                   (print-sum body)
                   (println ">=" lb))))))

       ;; variable bounds
       (println "bounds")
       (doseq [[i v] var-order]
         (let [{lb :lower ub :upper} (get (:vars lp) v)]
           (cond
             (and lb ub (== lb ub))
             (printf "x%d = %s\n" i lb)

             (and (not lb) (not ub))
             (printf "x%d free\n" i)

             :else
             (printf
              "%s <= x%d <= %s\n"
              (or lb "-inf")
              i
              (or ub "+inf")))))

       ;; variable types
       (let [{int-vars :integer
              bin-vars :binary}
             (group-by (comp :type second) (:vars lp))]
         (when (seq int-vars)
           (println "integer")
           (doseq [n (map first int-vars)]
             (printf "x%d\n" (get var-index n))))
         (when (seq bin-vars)
           (println "binary")
           (doseq [n (map first bin-vars)]
             (printf "x%d\n" (get var-index n)))))
       
       (println "end")
       )}))

(defn nl
  "Format an nl file for an lp.
  After https://cfwebprod.sandia.gov/cfdocs/CompResearch/docs/nlwrite20051130.pdf

  Return a map having {:program and :vars}

  :program is the nl file as a string
  :vars is a map from variable number to variable name in the input program.

  Currently requires that the input program be linear, although nl files do not.
  "
  [lp]
  ;; precondition - lp is linear?
  (let [lp (lp/normalize lp)
        _ (assert (lp/linear? lp) "NL output only for linear programs")

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

        constraints (lp/constraint-bodies lp)
        
        n-constraints       (count constraints)
        n-objectives        1
        n-range-constraints 0
        n-eq-constraints    (count (filter #{:=} (map first constraints)))

        
        n-linear-constraints n-constraints

        count-variables
        (fn count-variables [x]
          (cond
            (number? x) 0

            (instance? lp.core.Sum x)
            (reduce + 0 (map count-variables (keys (:terms x))))

            (instance? lp.core.Product x)
            (count (remove lp/is-zero? (vals (:factors x))))
            
            ;; our precondition should save us from trouble here
            :else 0))

        nz-jacobians  (reduce + 0 (for [c constraints] (count-variables (:body c))))
        gradients     (count-variables (:objective lp))
        
        factor-index
        (fn [x]
          (when-let [factors (:factors x)]
            (get var-to-index
                 (first
                  (for [[f e] factors
                        :when (lp/is-one? e)]
                    f)))))
        
        print-gradient
        (fn [x]
          (when (instance? lp.core.Sum x)
            (doseq [[t k] (:terms x)]
              ;; each thing in here should be a single Product
              ;; which should either be a linear term or a constant
              (when-let [i (factor-index t)]
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
                  (fn [{body :body}]
                    (and (instance? lp.core.Sum body)
                         (some (fn [x]
                                 (not (zero? (get (:factors x) var-name 0))))
                               (keys (:terms body)))))
                  constraints)))
        ]
    
    {:index-to-var index-to-var
     :var-to-index var-to-index
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
         (printf "n%s\n" (lp/constant-value (:body constraint)))
         )

       ;; objective sense
       (printf "O0 %d\n" (if (= :maximize (:sense lp)) 1 0))

       ;; objective body; again constant 0 because linear
       (printf "n%s\n" (lp/constant-value (:objective lp)))

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
           (print-bound lower upper var)))

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
    ))

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
            })))
  
  )
