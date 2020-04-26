(ns task02.query
  (:require [task02.helpers :as utils]
            [task02.db :as db]
            [clojure.core.match :refer [match]]))

;; Функция выполняющая парсинг запроса переданного пользователем
;;
;; Синтаксис запроса:
;; SELECT table_name [WHERE column comp-op value] [ORDER BY column] [LIMIT N] [JOIN other_table ON left_column = right_column]
;;
;; - Имена колонок указываются в запросе как обычные имена, а не в виде keywords. В
;;   результате необходимо преобразовать в keywords
;; - Поддерживаемые операторы WHERE: =, !=, <, >, <=, >=
;; - Имя таблицы в JOIN указывается в виде строки - оно будет передано функции get-table для получения объекта
;; - Значение value может быть либо числом, либо строкой в одинарных кавычках ('test').
;;   Необходимо вернуть данные соответствующего типа, удалив одинарные кавычки для строки.
;;
;; - Ключевые слова --> case-insensitive
;;
;; Функция должна вернуть последовательность со следующей структурой:
;;  - имя таблицы в виде строки
;;  - остальные параметры которые будут переданы select
;;
;; Если запрос нельзя распарсить, то вернуть nil

;; Примеры вызова:
;;  (parse-select "select student")
;; ;; ("student")
;; (parse-select "select student where id = 10")
;; ;; ("student" :where #<function>)
;; (parse-select "select student where id = 10 limit 2")
;; ;; ("student" :where #<function> :limit 2)
;; (parse-select "select student where id = 10 order by id limit 2")
;; ;; ("student" :where #<function> :order-by :id :limit 2)
;; (parse-select "select student where id = 10 order by id limit 2 join subject on id = sid")
;; ;;(parse-select "select student join subject on id = sid where id = 10 order by id limit 2 ")
;; ;; ("student" :where #<function> :order-by :id :limit 2 :joins [[:id "subject" :sid]])
;; (parse-select "werfwefw")
;; nil


(defn make-where-function [x op y]
  (let [x  (keyword x)
        y  (or (re-matches #"(?i)[^0-9]+" y) (utils/parse-int y))
        op ({"=" = ">=" >= "<=" <= "<" < ">" >} op)]
    (fn [rec] (op (get rec x) y))))
;; ((make-where-function "a" "<=" "b") {:a 2 :b 2})

(defn parse-select
  ([sel] (parse-select (re-seq #"(?i)[a-z0-9=><]+" sel) []))
  ([sel res]
   (if (seq sel)
     (match [(vec sel)]
       [["select" tbl & rq]] (parse-select rq (conj res tbl))
       [["where" x op y & rq]] (parse-select rq (conj res :where (make-where-function x op y)))
       [["order" "by" x & rq]] (parse-select rq (conj res :order-by (keyword x)))
       [["limit" x & rq]] (parse-select rq (conj res :limit (utils/parse-int x)))
       [["join" tbl "on" x op y & rq]] (parse-select rq (conj res :joins [[(keyword x) tbl (keyword y)]]))
       :else (parse-select (rest sel) res))
     (seq res))))


;; Выполняет запрос переданный в строке.  Бросает исключение если не удалось распарсить запрос

;; Примеры вызова:
;; (perform-query "select student")
;; ({:id 1, :year 1998, :surname "Ivanov"} {:id 2, :year 1997, :surname "Petrov"} {:id 3, :year 1996, :surname "Sidorov"})
 ;; (perform-query "select student order by year")
;; ({:id 3, :year 1996, :surname "Sidorov"} {:id 2, :year 1997, :surname "Petrov"} {:id 1, :year 1998, :surname "Ivanov"})
;; (perform-query "select student where id > 1")
;; ({:id 2, :year 1997, :surname "Petrov"} {:id 3, :year 1996, :surname "Sidorov"})
;; (perform-query "not valid")
;; exception...


(defn perform-query [^String sel-string]
  (if-let [query (parse-select sel-string)]
    (apply db/select (db/get-table (first query)) (rest query))
    (throw (IllegalArgumentException. (str "Can't parse query: " sel-string)))))
