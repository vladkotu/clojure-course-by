(ns cache.protocols)

(defprotocol PCache
  (lookup [cache e] "Retrieves item in the cache if it exists")
  (has?   [cache e] "Checks for a cached value")
  (hit    [cache e] "Called when item is found")
  (miss   [cache e result] "Called when no item is found"))

(comment
  ;; Object hashCoe implementation will give this:
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

(def cache (BaseCache. {:k :v}))
(lookup (miss cache :k2 :v2) :k2)
