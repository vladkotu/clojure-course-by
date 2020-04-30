(ns async.network
  (:require [clojure.core.async :as a :refer [<!! >!!]]
            [clojure.pprint :as pp]
            [clojure.data.json :as json]
            [cheshire.core :as che]
            [clj-http.client :as client]))


;;; networking
;; (def surl "https://serpapi.com/search.json?q=Coffee&location=Kharkiv&hl=en&gl=ua")


(def surl "https://api.cognitive.microsoft.com/bing/v7.0/search")

(def locations {:kh "Kharkiv"
                :zp "Zaporizhzhia"
                :ky "Kyiv"})

(defn search< [{:keys [q l]} c]
  (client/get
   surl {:async?  true
         :headers {"Ocp-Apim-Subscription-Key" "d6735678aef84ffd9b2518f94442d6cd"}
         :query-params
         {:q       (str q " near " (get locations l))
          :cc      "ua"
          :mkt     "ua_EN"
          :setLang "en"
          :count   3}}
   (fn [response]
     (let [data (che/parse-string (:body response) true)
           places (-> data :webPages :value)
           result (map #(select-keys % [:name :snippet :url]) places)]
       (a/put! c result)))
   (fn [err]
     (println "ERROR")
     (clojure.pprint/pprint err)))
  c)

(def queries
  (for [q ["Burger" "Coffee" "Fitness" "Hotel"]
        l (keys locations)]
    {:q q :l l}))

(let [c (a/chan 100)]
  (a/go-loop [queries queries]
    (when (seq queries)
      (println (a/<! (search< (first queries) c)))
      (println "Waiting")
      (a/<! (a/timeout 500))
      (recur (rest queries)))))
