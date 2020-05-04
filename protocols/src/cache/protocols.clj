(ns cache.protocols)

(defprotocol PCache
  (lookup [cache e] "Retrieves item in the cache if it exists")
  (has?   [cache e] "Checks for a cached value")
  (hit    [cache e] "Called when item is found")
  (miss   [cache e result] "Called when no item is found"))

(comment
  ;; Primers: https://stackoverflow.com/questions/26622511/clojure-value-equality-and-sets
  ;;          https://stackoverflow.com/questions/9229434/maps-and-records-equality-in-clojure
  ;; Object hashCode implementation will give this:
  (into #{} [(BaseCache. {:a 1}) (BaseCache. {:a 1})])
  ;; => #{#object[cache.protocols.BaseCache 0x3d72637f "<{:a 1}>"]}

  ;; Object equals will give this:
  (= (BaseCache. {:a 1}) (BaseCache. {:a 1}))
  ;; => true

  ;; Object toString will give this:
  (str (BaseCache. {:a 1}))
  ;; => "<{:a 1}>"

  ;; check type inside Object equals implementation gives this:
  (.equals (BaseCache. {:a 1}) {:a 1}) ;; => true
  (.equals (BaseCache. {:a 1}) (hash-map :a 1)) ;; => true
  )

(deftype BaseCache [cache]
  Object
  (equals [this other]
    (condp contains? (type other)
      #{BaseCache}                      (= cache (.cache other))
      #{clojure.lang.PersistentArrayMap
        clojure.lang.PersistentHashMap} (= cache other)
      false))
  (hashCode [_]
    (hash cache))
  (toString [this]
    (str "<" cache ">"))

  PCache
  (lookup [_ item]
    (get cache item))
  (has? [_ item]
    (contains? cache item))
  (hit [this item]
    this)
  (miss [_ item result]
    (BaseCache. (assoc cache item result))))

(comment
  (.toString (BaseCache. {:k :v}))
  (def cache (BaseCache. {:k :v}))
  (lookup (BaseCache. {:k :v2}) :k))

(defn hit-or-miss [cache f item]
  (if (has? cache item)
    (hit cache item)
    (miss cache item (delay (apply f item)))))

(do
  (defn foo [& nums]
    (Thread/sleep 1000)
    (reduce + (filter number? nums)))
  (foo 1 32 nil 'a "s")
  (def args [1 2 3 'sym nil "a" 4])
  (def basecache (hit-or-miss (BaseCache. {}) foo args))
  (with-out-str (time (print (str (deref (lookup basecache args)) " => "))))
  ;; => "10 => \"Elapsed time: 1000.388802 msecs\"\n"
  (with-out-str (time (print (str @(lookup basecache args) " => "))))
  ;; => "10 => \"Elapsed time: 0.187338 msecs\"\n"
  )
(do
  (def args2 [9 8 7])
  (def cache (atom (BaseCache. {})))
  (swap! cache hit-or-miss foo args)
  (swap! cache hit-or-miss foo args2)
  @(lookup @cache args)
  ;; => 10
  @(lookup @cache args2)
  ;; => 24
  )

(defn memo
  ([f] (memo f (BaseCache. {})))
  ([f strategy & seed-args]
   {:pre [(satisfies? PCache strategy)]}
   (let [cache-state (atom strategy)]
     (fn [& args]
       (let [item   (concat seed-args args)
             result (swap! cache-state hit-or-miss f item)]
         (deref (lookup result item)))))))

(def c-sum (memo #(do (Thread/sleep 1000)
                      (+ %1 %2))))
(c-sum 2 2)
