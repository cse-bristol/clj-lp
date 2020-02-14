(ns lp.scip
  (:require [lp.core :as lp]
            [lp.io :as lpio]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as s]))

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

  (let [lines              (remove s/blank? (s/split-lines text))
        [junk _ lines]     (partition-by #{"Options"} lines)
        [num-opts & lines] lines
        num-opts           (Integer/parseInt num-opts)
        weird              (> num-opts 4) ;; no idea what this means
        num-opts           (if weird (- num-opts 2) num-opts)
        [opt-vals lines]   (split-at (+ (if weird 5 4) num-opts) lines) ;; not sure why + 4

        ;; these are the only two values in here that we use
        n-vars (Integer/parseInt (nth opt-vals (+ num-opts 3)))
        n-cons (Integer/parseInt (nth opt-vals (+ num-opts 1)))

        [cons-vals lines]  (split-at n-cons lines)
        [var-vals lines]   (split-at n-vars lines)

        read-double #(Double/parseDouble %)
        cons-vals (map read-double cons-vals)
        var-vals  (map read-double var-vals)

        [objno & lines] lines
        
        ]
    (->> (map-indexed
          (fn [i v]
            [(get var-index i)
             {:value v}])
          var-vals)
         (into {}))
    
    ;; {:vars var-vals
    ;;  :cons cons-vals
    ;;  :objno objno
    ;;  }

    ))

(defn solve [lp]
  (let [{problem-text :program var-index :index-to-var}
        (lpio/nl lp)
        
        result-text
        (lpio/with-temp-dir temp-dir
          (spit (io/file temp-dir "problem.nl") problem-text)
          (sh/sh
           ;; need right args here
           "/nix/store/ijhl69ka24hwg58hg3pmak0zdrkd49y9-run-solver-env/bin/scipampl"
           "problem.nl"
           :dir temp-dir)
          (slurp (io/file temp-dir "problem.sol")))

        ;; need solution format here
        result (sol result-text var-index)]
    (lp/merge-results lp result))
  )

