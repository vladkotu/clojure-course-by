(ns task02.db
  "Namespace for database-related data & functions..."
  (:refer-clojure :exclude [update])
  (:require [clojure.data.csv :as csv]
            [clojure.string :as str]
            [task02.helpers :as utils]))

(set! *warn-on-reflection* true)

;; helper functions
(defn table-keys [tbl]
  (mapv keyword (first tbl)))

(defn key-value-pairs [tbl-keys tbl-record]
  (interleave tbl-keys tbl-record))

(def data-record zipmap)

(defn data-table [tbl]
  (map (partial data-record (table-keys tbl)) (next tbl)))

(defn str-field-to-int [field rec]
  (update-in rec [field] utils/parse-int))

(defn str-all-fields-to-int [fields rec]
  (if (seq fields)
    (recur (rest fields)
           (str-field-to-int (first fields) rec))
    rec))

;; (str-all-fields-to-int [:a :b] {:a "1" :b "2" :c 'c})

;; Место для хранения данных - используйте atom/ref/agent/...
(def student (atom []))
(def subject (atom []))
(def student-subject (atom []))

;; функция должна вернуть мутабельный объект используя его имя
(defn get-table [^String tb-name]
  (case (str/lower-case tb-name)
    "student"         student
    "subject"         subject
    "student-subject" student-subject))

;; (get-table "subject")

;;; Данная функция загружает начальные данные из файлов .csv
;;; и сохраняет их в изменяемых переменных student, subject, student-subject

(defn load-tbl-data
  [tbl-name to-ints]
  (let [tbl-file (str (clojure.string/replace tbl-name #"-" "_") ".csv")
        tbl-data (data-table (csv/read-csv (slurp tbl-file)))]
    (vec
     (if-not (empty? to-ints)
       (map #(str-all-fields-to-int to-ints %) tbl-data)
       tbl-data))))
;; (load-tbl-data "student" [:id :year])

(defn load-initial-data []
  (doseq [[tbl-name to-ints] [["student" [:id :year]]
                              ["subject" [:id]]
                              ["student-subject" [:subject_id :student_id]]]]
    (reset! (get-table tbl-name)
            (load-tbl-data tbl-name to-ints))))

;; (load-initial-data)

;; select-related functions...
(defn where* [data condition-func]
  (if condition-func
    (filter condition-func data)
    data))

(defn where-not* [data condition-func]
  (if condition-func
    (remove condition-func data)
    []))

(defn limit* [data lim]
  (if lim
    (take lim data)
    data))

(defn order-by* [data column]
  (if column
    (sort-by column data)
    data))

(defn join* [data1 column1 data2 column2]
  (for [left data1
        right data2
        :when (= (column1 left) (column2 right))]
    (merge right left)))

;; Внимание! в отличии от task01, в этом задании 2-я таблица передается в виде строки с именем таблицы!
(defn perform-joins [data joins]
  (if (empty? joins)
    data
    (let [[col1 data2 col2] (first joins)]
      (recur (join* data col1 @(get-table data2) col2)
             (next joins)))))

;; Данная функция аналогична функции в первом задании за исключением параметра :joins как описано выше.
;; примеры использования с измененым :joins:
;;   (select student-subject :joins [[:student_id "student" :id] [:subject_id "subject" :id]])
;;   (select student-subject :limit 2 :joins [[:student_id "student" :id] [:subject_id "subject" :id]])
(defn select [data & {:keys [where limit order-by joins]}]
  (-> @data
      (perform-joins joins)
      (where* where)
      (order-by* order-by)
      (limit* limit)))

;; insert/update/delete...

;; Данная функция должна удалить все записи соответствующие указанному условию
;; :where. Если :where не указан, то удаляются все данные.
;;
;; Аргумент data передается как мутабельный объект (использование зависит от выбора ref/atom/agent/...)
;;
;; Примеры использования
;;   (delete student);; -> []
;;   (delete student :where #(= (:id %) 1)) ;; -> все кроме первой записи

;; (load-initial-data)

(defn delete [data & {:keys [where] :or {where identity}}]
  (reset! data (vec (where-not* @data where))))

;; Данная функция должна обновить данные в строках соответствующих указанному предикату
;; (или во всей таблице).
;;
;; - Аргумент data передается как мутабельный объект (использование зависит от выбора ref/atom/agent/...)
;; - Новые данные передаются в виде map который будет объединен с данными в таблице.
;;
;; Примеры использования:
;; (update student {:id 5})
;; (update student {:id 6} :where #(= (:year %) 1996))
;; (load-initial-data)

(defn update [data upd-map & {:keys [where]  :or {where identity}}]
  (reset! data (mapv #(if (where %) (merge % upd-map) %) @data)))


;; Вставляет новую строку в указанную таблицу
;;
;; - Аргумент data передается как мутабельный объект (использование зависит от выбора ref/atom/agent/...)
;; - Новые данные передаются в виде map
;;
;; Примеры использования:
;; (insert student {:id 10 :year 2000 :surname "test"})
;; (load-initial-data)
;; (deref student)

(defn insert [data new-entry]
  (swap! data conj new-entry))
