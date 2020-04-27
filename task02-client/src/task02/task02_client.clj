(ns task02.task02-client
  (:require [clojure.java.io :as io])
  (:import [java.net Socket InetAddress InetSocketAddress SocketTimeoutException])
  (:gen-class))

(defn query [command]
  (let [sock (Socket. "127.0.0.1" 9999)]
    (with-open [rdr (io/reader sock)
                wrt (io/writer sock)]
      (let [result (delay (.readLine rdr))]
        (doto wrt
          (.write command)
          (.write (str \newline))
          (.flush))
        (when-let [data @result]
          (clojure.edn/read-string data))))))


(query "select student where surname = Ivanov")
