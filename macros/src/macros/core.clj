(ns macros.core
  (:gen-class))

;; ** how to delay calculation to runtime?
;; wrong way
(defmacro defsum* [name x y]
  (let [res (+ x y)]
    `(def ~name ~res)))

(def a 2)
(def b 8)
;; (defsum* x1 a b) ;; clojure.lang.Symbol cannot be cast to class java.lang.Number

;; right way
(defmacro defsum [name x y]
  (let [res `(+ ~x ~y)]
    `(def ~name ~res)))

(def a 2)
(def b 8)
(defsum x1 a b)
x1 ;; => 10

;;;;;;;;;;;;;;;;;;; if all let macro ;;;;;;;;;;;;;;;;;
(alter-var-root #'clojure.pprint/*print-suppress-namespaces* (constantly true))

(defmacro if-all-let [bindings then else]
  (let [parties   (partition 2 bindings)
        positive  (keep (fn [[_ tst]] `(when ~tst) tst) parties)
        parties'  (count parties)
        positive' (count positive)]
    `(let ~bindings
       (if (= ~parties' ~positive')
         ~then
         ~else))))

(with-out-str
  (clojure.pprint/pprint
   (clojure.walk/macroexpand-all
    '(if-all-let [zu 1
                  eu 2
                  xx nil]
                 :than
                 :else))))
;; => "(let* [zu 1 eu 2 xx nil] (if (= 3 2) :than :else))\n"

(if-all-let [z 1
             x 2
             y nil]
            :then
            :else)
;; => :else


;;;;;;;;;;;; Language Workbenches ;;;;;;;;;;;;;
(def sample-lines
  ["SVCLFOWLER         10101MS0120050313........................."
   "SVCLHOHPE          10201DX0320050315........................"
   "SVCLTWO           x10301MRP220050329.............................."
   "USGE10301TWO          x50214..7050329..........................."])

(defn line-type
  ([line] (keyword (.substring line 0 4)))
  ([line specs] :by-spec))

(defmulti parse #'line-type)

(defmethod parse :by-spec
  ([line specs]
   (mapv #(-> line
              (.substring (first %) (inc (second %)))
              clojure.string/trim)
         specs)))

(defmacro defmapping [name code & spec]
  (let [recname (symbol (str "->" name))
        fields  (partition 3 spec)
        fnames  (mapv #(symbol (last %)) fields)]
    `(do
       (defrecord ~name ~fnames)
       (defmethod parse ~code
         ([line#]
          (apply ~recname
                 (parse line# ~(mapv vec fields))))))))

(defmapping ServiceCall :SVCL
  4 18 "CustomerName"
  19 23 "CustomerID"
  24 27 "CallTypeCode"
  28 35 "DateOfCallString")

(defmapping Usage :USGE
  4 8 "CustomerID"
  9 22 "CustomerName"
  30 30 "Cycle"
  31 36 "ReadDate")

(mapv parse sample-lines)
;; => [{:CustomerName "FOWLER",
;;      :CustomerID "10101",
;;      :CallTypeCode "MS01",
;;      :DateOfCallString "20050313"}
;;     {:CustomerName "HOHPE",
;;      :CustomerID "10201",
;;      :CallTypeCode "DX03",
;;      :DateOfCallString "20050315"}
;;     {:CustomerName "TWO           x",
;;      :CustomerID "10301",
;;      :CallTypeCode "MRP2",
;;      :DateOfCallString "20050329"}
;;     {:CustomerID "10301",
;;      :CustomerName "TWO          x",
;;      :Cycle "7",
;;      :ReadDate "050329"}]

(alter-var-root #'clojure.pprint/*print-suppress-namespaces* (constantly true))
(clojure.pprint/pprint
 (macroexpand-1 '(defmapping ServiceCall :SVCL
                   4 18 "CustomerName"
                   19 23 "CustomerID"
                   24 27 "CallTypeCode"
                   28 35 "DateOfCallString")))
;; =>
;; (do
;; (defrecord
;;     ServiceCall
;;     [CustomerName CustomerID CallTypeCode DateOfCallString])
;; (defmethod
;;   parse
;;   :SVCL
;;   ([line__12064__auto__]
;;    (apply
;;     ->ServiceCall
;;     (parse
;;      line__12064__auto__
;;      [[4 18 "CustomerName"]
;;       [19 23 "CustomerID"]
;;       [24 27 "CallTypeCode"]
;;       [28 35 "DateOfCallString"]])))))
