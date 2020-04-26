(ns csvdb.core
  (:require [clojure-csv.core :as csv]
            [clojure.edn :as edn]))

(def student-tbl (csv/parse-csv (slurp "student.csv")))
(def subject-tbl (csv/parse-csv (slurp "subject.csv")))
(def student-subject-tbl (csv/parse-csv (slurp "student_subject.csv")))

(defn table-keys [tbl]
  (->> (first tbl)
       (mapv keyword)))

(defn key-value-pairs [tbl-keys tbl-record]
  (mapcat vector tbl-keys tbl-record))

(defn data-record [tbl-keys tbl-record]
  (zipmap tbl-keys tbl-record))

(defn data-table [tbl]
  (->> (rest tbl)
       (map (partial data-record (table-keys tbl)))))

(defn str-field-to-int [field rec]
  (update rec field #(Integer/parseInt %)))

(def student (->> (data-table student-tbl)
                  (map #(str-field-to-int :id %))
                  (map #(str-field-to-int :year %))))

(def subject (->> (data-table subject-tbl)
                  (map #(str-field-to-int :id %))))

(def student-subject (->> (data-table student-subject-tbl)
                          (map #(str-field-to-int :subject_id %))
                          (map #(str-field-to-int :student_id %))))

(defmacro defop [nam args & body]
  `(defn ~nam ~args
     (if (last ~args)
       ~@body
       (first ~args))))

(defop where* [data condition-func]
  (filter condition-func data))

(defop limit* [data lim]
  (take lim data))

(defop order-by* [data column]
  (sort-by column data))

(defop join* [data1 column1 data2 column2]
  (let [vals1  (set (map column1 data1))
        index2 (into {} (keep #(let [f1 (column2 %)]
                                 (when (contains? vals1 f1)
                                   [f1 %])) data2))]
    (map #(merge (get index2 (column1 %) {}) %) data1)))

(defop perform-joins [data joins*]
  (loop [data1 data
         joins joins*]
    (if (empty? joins)
      data1
      (let [[col1 data2 col2] (first joins)]
        (recur (join* data1 col1 data2 col2)
               (next joins))))))

(defn select [data & {:keys [where limit order-by joins]}]
  (-> data
      (perform-joins joins)
      (where* where)
      (order-by* order-by)
      (limit* limit)))

(select student)

(select student :order-by :year)

(select student :where #(> (:id %) 1))

(select student :limit 2)

(select student :where #(> (:id %) 1))

(select student :where #(> (:id %) 1) :order-by :year :limit 2)

(select student-subject :joins [[:student_id student :id] [:subject_id subject :id]])

(select student-subject :limit 2 :joins [[:student_id student :id] [:subject_id subject :id]])

(set! *warn-on-reflection* true)
(defrecord Ma [^long a])

(class (Ma. 1))
(class {:a 1})
(= (Ma. 1) {:a 1})

(:a (Ma. 1))

(defn ln ^Integer [^String s]
  (.length s))

(.intValue (ln "atr"))

(prn-str #{1 2 3})
(read-string (prn-str #{1 2 3}))
(class (edn/read-string (prn-str '(1 2))))

(edn/read-string (prn-str (Ma. 3)))

(def x (with-meta {:data [1 2 3]} {:hola :spain}))
(meta x)
(meta #'x)

(def shread2
  (memoize
   (fn [o]
     (do
       (println "shread2" o)
       (:name o)))))

(defmulti hell #'shread2)
(defmethod hell :kit
  [_]
  (println (str "Hola Kit")))
(defmethod hell :blor
  [_]
  (println (str "Hola Blor")))

(hell {:name :kit})
(hell {:name :blor})
