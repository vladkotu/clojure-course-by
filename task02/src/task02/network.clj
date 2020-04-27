(ns task02.network
  ;; (:use [task02 helpers query])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [task02.query :as q]
            [task02.db :as db])
  (:import [java.net Socket ServerSocket InetAddress InetSocketAddress SocketTimeoutException]))

;; Объявить переменную для синхронизации между потоками. Воспользуйтесь promise
(def ^:private should-be-finished (promise))

(set! *warn-on-reflection* true)

(pr-str [{:a 1}])

(defn handle-request [^Socket sock]
  (try
    (with-open [rdr (io/reader sock)
                wrt (io/writer sock)]
      (let [command (.readLine ^java.io.BufferedReader  rdr)]
        (doto wrt
          (.write (pr-str (q/perform-query command)))
          (.write (str \newline))
          (.flush))))
    (catch Throwable ex
      (println "Exception: " ex))
    (finally
      (.close sock))))


;; Hint: future, deliver
(defn- run-loop [^ServerSocket server-sock]
  (try
    (let [sock (.accept server-sock)]
      (future
        (handle-request sock)))
    (catch SocketTimeoutException ex)
    (catch Throwable ex
      (println "Got exception" ex)
      (deliver should-be-finished true))))

(defn run [^long port]
  (let [sock-addr     (InetSocketAddress. "127.0.0.1" port)
        server-socket (doto (ServerSocket.)
                        (.setReuseAddress true)
                        (.setSoTimeout 1000)
                        (.bind sock-addr))]
    (loop [_ (run-loop server-socket)]
      (when-not (realized? should-be-finished)
        (recur (run-loop server-socket))))
    (.close server-socket)))

;; (db/load-initial-data)
;; (run 9999)

;; (def ^:private should-be-finished (promise))
;; (realized? should-be-finished)
