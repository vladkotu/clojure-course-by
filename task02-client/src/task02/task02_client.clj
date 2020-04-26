(ns task02.task02-client
  (:refer-clojure :exclude [read-line])
  (:require [clojure.java.io :as io])
  (:import [java.net Socket InetAddress InetSocketAddress SocketTimeoutException])
  (:gen-class))

(defn create-socket [hostname port]
  (Socket. hostname port))

(defn close-socket [socket]
  (.close socket))

(defn write [socket message]
  (doto (io/writer socket)
    (.write (str message \newline))
    (.flush)))

(def get-reader
  (memoize (fn [socket] (io/reader socket))))

(defn read-line [socket]
  (doto (get-reader socket)
    (.readLine)))

(defn read-line [socket]
  (with-open [rdr (io/reader socket)]
    (.readLine rdr)))

(def socket1 (create-socket "127.0.0.1" 9999))
(def socket2 (create-socket "127.0.0.1" 9999))


;; (dotimes [n 100]
;;   (write socket1 "hola")
;;   (write socket2 "espanolo")
;;   )


(doseq [[name socket] [["socket1" socket1]
                       ["socket2" socket2]]]
  (future
    (with-open [rdr (io/reader socket)]
      (loop [line (.readLine rdr)]
        (println "in shutdown" (.isInputShutdown socket))
        (println "isClosed" (.isClosed socket))
        (if (= "quit" line)
          (do (println (str name " socket closed"))
              (close-socket socket))
          (do
            (println (str name " socket recieved: " line))
            (Thread/sleep (rand-int 1000))
            (recur (.readLine rdr))))))))

(close-socket socket1)
(close-socket socket2)

(.isClosed socket1)
(.isClosed socket2)
