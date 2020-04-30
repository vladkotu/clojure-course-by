(ns async.async-clj-test
  (:require [clojure.core.async :as a :refer [<!! >!!]]
            [clojure.pprint :as pp]
            [clojure.data.json :as json]
            [clj-http.client :as client]))

(def c1
  (let [c (a/chan 3 (comp (map #(- % 12))
                          (filter #(> % 40))))]
    (a/go (a/>! c 52)
          (a/>! c 62)
          (a/>! c 72))
    (a/go-loop []
      (println "value" (a/<! c))
      (recur))))
;; => value 50
;; => value 60


(def queue (a/chan 10))
(a/go-loop [n 10] (when-not (zero? n)
                    (a/put! queue n)
                    (recur (dec n))))

(for [n (range 10)]
  (a/<!! queue))
;; => (10 9 8 7 6 5 4 3 2 1)


(defmacro pp-macroexpand
  ([form] `(pp-macroexpand {} ~form))
  ([opts form]
   `(let [qualificate# (get ~opts :qualificate false)
          expand#      (get ~opts :expand :-1)]
      (println)
      (clojure.pprint/write
       (case expand#
         :-1   (macroexpand-1 ~form)
         :-all (clojure.walk/macroexpand-all ~form)
         (macroexpand ~form))
       :dispatch clojure.pprint/code-dispatch
       :suppress-namespaces (not qualificate#))
      (println))))

(defmacro while-let [binding & body]
  (let [[form test] binding]
    `(loop [times# 1024]
       (if (zero? times#)
         (throw (ex-info "Potentially infinite loop detected." {:max-tries times#}))
         (let [tmp# ~test]
           (when tmp#
             (let [~form tmp#]
               ~@body)
             (recur (dec times#))))))))

(pp-macroexpand '(while-let [a true]
                            (println a)))
;; (loop [times__21423__auto__ 1024]
;;   (if (zero? times__21423__auto__)
;;     (throw
;;      (ex-info
;;       "Potentially infinite loop detected."
;;       {:max-tries times__21423__auto__}))
;;     (let [tmp__21424__auto__ true]
;;       (when tmp__21424__auto__
;;         (let [a tmp__21424__auto__] (println a))
;;         (recur (dec times__21423__auto__))))))

(a/go (while-let [res (a/<! queue)]
                 (println res)))

;; 10
;; 9
;; 8
;; 7
;; 6
;; 5
;; 4
;; 3
;; 2
;; 1


