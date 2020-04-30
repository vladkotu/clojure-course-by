(ns async.network
  (:require [clojure.core.async :as a :refer [<!! >!!]]
            [clojure.pprint :as pp]
            [clojure.data.json :as json]
            [clj-http.client :as client]))


;;; networking
;; (def surl "https://serpapi.com/search.json?q=Coffee&location=Kharkiv&hl=en&gl=ua")

(def surl "https://serpapi.com/search.json")

(client/get surl {
                  :async? true
                  ;; :as :json
                  :query-params {:q "Coffee"
                                 :location "Kharkiv"
                                 :hl "en"
                                 :gl "ua"}}
            (fn [response]

              ;; (println response)
              (println (json/read-str response))
              #_(let [res (json/read-str response)]
                  (println res)
                  res))
            (fn [err]
              (println err)))
