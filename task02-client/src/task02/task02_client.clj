(ns task02.task02-client
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]])
  (:import [java.net Socket InetAddress InetSocketAddress SocketTimeoutException])
  (:gen-class))

(defn query [command]
  (let [sock (Socket. "127.0.0.1" 9997)]
    (with-open [rdr (io/reader sock)
                wrt (io/writer sock)]
      (let [result (delay (.readLine rdr))]
        (doto wrt
          (.write command)
          (.write (str \newline))
          (.flush))
        (when-let [data @result]
          (clojure.edn/read-string data))))))



;; (pprint (query "select student"))

(defn -main []
  (println "Enter sql query bellow")
  (println "   Note only \"select\" queries are supported")
  (println "   type \":quit\" command to quit programm")
  (loop [command (read-line)]
    (when-not (= ":quit" command)
      (println (format "Your query \"%s\"" command))
      (println "Result:")
      (pprint (query command))
      (println \newline "Enter another query")
      (recur (read-line)))))
