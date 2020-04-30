(ns async.tbaldridge-tutor
  (:require [clojure.core.async :as a :refer [<!! >!!]]))


;; From TBaldridge -- its too hard to get for right now
(defn to-proc< [in]
  (let [out (a/chan 1)]
    (a/pipe in out)
    out))

(defn pipeline< [data c]
  (let [pip (partition 2 data)]
    (reduce
     (fn [prev-c [n f]]
       (a/merge
        (for [_ (range n)]
          (to-proc< (a/map< f prev-c)))))
     c
     pip)))

(let [c (a/chan 10)]
  (>!! c 42)
  (<!! (pipeline< [4 inc
                   1 dec
                   2 inc
                   3 #(* % %)
                   2 str] c)))

