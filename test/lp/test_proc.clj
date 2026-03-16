(ns lp.test-proc
  (:require [clojure.test :as t]
            [clojure.java.io :as io]
            [lp.proc :refer [run]]))

(t/deftest interrupt-proc-test
  (let [f (future (run ["sh" "-c" "sleep 1; echo test > file-should-not-exist"]
                    ))]
    
    (println @f)
    (t/is (.exists (io/as-file "file-should-not-exist")))
    (io/delete-file "file-should-not-exist" true))

  (let [f (future (run ["sh" "-c" "sleep 1; echo test > file-should-not-exist"]))]
    (future-cancel f)
    (Thread/sleep 1200)
    (t/is (not (.exists (io/as-file "file-should-not-exist"))))
    (io/delete-file "file-should-not-exist" true)))

