(ns macros.core
  (:gen-class))

;; ** how to delay calculation to runtime?
;; wrong way
(defmacro defsum* [name x y]
  (let [res (+ x y)]
    `(def ~name ~res)))

(def a 2)
(def b 8)
(defsum x1 a b) ;; clojure.lang.Symbol cannot be cast to class java.lang.Number

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
