(ns lp.proc
  "A version of clojure.java.shell that terminates the subprocess if interrupted"
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]))

(defn- ^"[Ljava.lang.String;" as-env-strings 
  "Helper so that callers can pass a Clojure map for the :env to sh."
  [arg]
  (cond
   (nil? arg) nil
   (map? arg) (into-array String (map (fn [[k v]] (str (name k) "=" v)) arg))
   true arg))

(defn- stream-to-string [in]
  (with-open [bout (java.io.StringWriter.)]
    (io/copy in bout :encoding (.name (java.nio.charset.Charset/defaultCharset)))
    (.toString bout)))

(defn run [cmd-args & {:keys [dir env]
            :or {env (into {} (System/getenv))
                 dir (.getCanonicalPath (io/as-file ""))}}]
  (let [proc (.exec (Runtime/getRuntime) 
                    ^"[Ljava.lang.String;" (into-array cmd-args)
                    (as-env-strings env)
                    (io/as-file dir))]
    (try
      (.close (.getOutputStream proc))
      (with-open [stdout (.getInputStream proc)
                  stderr (.getErrorStream proc)]
        (let [out (future (stream-to-string stdout))
              err (future (stream-to-string stderr))
              exit-code (.waitFor proc)]
          {:exit exit-code :out @out :err @err}))
      (catch InterruptedException e
        (.destroy proc)
        (throw e)))))

(comment
  
  
  )
