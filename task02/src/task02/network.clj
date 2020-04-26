(ns task02.network
  (:use [task02 helpers query])
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.net Socket ServerSocket InetAddress InetSocketAddress SocketTimeoutException]))

;; Объявить переменную для синхронизации между потоками. Воспользуйтесь promise
(def ^:private should-be-finished (promise))

;; Hint: *in*, *out*, io/writer, io/reader, socket.getOutputStream(), socket.getInputStream(), socket.close(), binding
;;       deliver, prn
(defn handle-request [^Socket sock]
  (:implement-me ;; переопределить *in* & *out* чтобы они указывали на входной и выходной потоки сокета
   (try
     (let [s (read-line)] ;; считать данные из переопределенного *in*
       (if (= (str/lower-case s) "quit")
         (deliver should-be-finished true) ;;; 1) сообщить основному потоку что мы завершаем выполнение.
         :implement-me ;;; 2) выполнить запрос при помощи perform-query и записать
               ;;; результат в переопределенный *out*
         ))
     (catch Throwable ex
       (println "Exception: " ex))
     (finally
       (.close sock))))) ;;; закрыть сокет


;; Hint: future, deliver
(defn- run-loop [server-sock]
  (try
    (println "run-loop")
    (let [^Socket sock (.accept server-sock)]
      (future
        (println "new thread")
        (with-open [rdr (io/reader sock)
                    wr  (io/writer sock)]
          (loop [line (.readLine rdr) comms 3]
            (if (zero? comms)
              (do (println "goint out:")
                  (doto wr
                    (.write (str "quit" \newline))
                    (.flush)))
              (do
                (println (str "recieved: " line))
                (doto wr
                  (.write (str "processed: " line \newline))
                  (.flush))
                (recur (.readLine rdr) (dec comms))))))
        (println "close socket")
        (.close sock)))
    (catch SocketTimeoutException ex)
    (catch Throwable ex
      (println "Got exception" ex)
      (deliver should-be-finished true) ;; сообщить основному потоку что мы завершаем выполнение
          ;; для этого необходимо установить переменную should-be-finished в true
      )))

(defn run [port]
  (let [sock-addr (InetSocketAddress. "127.0.0.1" port)
        server-socket (doto (ServerSocket.)
                        (.setReuseAddress true)
                        (.setSoTimeout 1000)
                        (.bind sock-addr))]
    (loop [_ (run-loop server-socket)]
      (when-not (realized? should-be-finished) ;; следующий запрос если работа не завершается...
        (recur (run-loop server-socket))))
    (println "close server")
    (.close server-socket)))

(run 9999)

(def ^:private should-be-finished (promise))
(realized? should-be-finished)
