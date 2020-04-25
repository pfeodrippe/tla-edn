(ns tla-edn.core
  (:import
   (tlc2.value.impl Value IntValue RecordValue BoolValue FcnRcdValue
                    StringValue TupleValue SetEnumValue BoolValue)
   (util UniqueString)))

(defprotocol TLAPlusEdn
  (to-edn [this]))

(extend-protocol TLAPlusEdn
  tlc2.value.impl.RecordValue
  (to-edn [v]
    (with-meta
      (zipmap (map str (.-names v))
              (map to-edn (.-values v)))
      {:tla-plus-type tlc2.value.impl.RecordValue}))

  tlc2.value.impl.FcnRcdValue
  (to-edn [v]
    (with-meta
      (zipmap (map #(str (.val %)) (.-domain v))
              (map to-edn (.-values v)))
      {:tla-plus-type tlc2.value.impl.FcnRcdValue}))

  tlc2.value.impl.TupleValue
  (to-edn [v]
    (with-meta
      (map to-edn (.getElems v))
      {:tla-plus-type tlc2.value.impl.TupleValue}))

  tlc2.value.impl.SetEnumValue
  (to-edn [v]
    (with-meta
      (set (map to-edn (.toArray (.-elems v))))
      {:tla-plus-type tlc2.value.impl.SetEnumValue}))

  tlc2.value.impl.IntValue
  (to-edn [v]
    (.val v))

  tlc2.value.impl.StringValue
  (to-edn [v]
    (str (.val v)))

  tlc2.value.impl.BoolValue
  (to-edn [v]
    (.getVal v)))

(defmulti to-tla-value
  (fn [v]
    (or (:tla-plus-type (meta v))
        (type v))))

(defmethod to-tla-value tlc2.value.impl.RecordValue
  [v]
  (->> (map (fn [[k val]]
              [(UniqueString/uniqueStringOf
                (if (keyword? k) (name k) k))
               (to-tla-value val)])
            v)
       (into {})
       (#(RecordValue.
          (into-array UniqueString (keys %))
          (into-array Value (vals %))
          false))))

(defmethod to-tla-value clojure.lang.PersistentArrayMap
  [v]
  (->> (map (fn [[k val]]
              [(UniqueString/uniqueStringOf
                (if (keyword? k) (name k) k))
               (to-tla-value val)])
            v)
       (into {})
       (#(RecordValue.
          (into-array UniqueString (keys %))
          (into-array Value (vals %))
          false))))

(defmethod to-tla-value tlc2.value.impl.FcnRcdValue
  [v]
  (->> (map (fn [[k val]]
              [(StringValue.
                (if (keyword? k) (name k) k))
               (to-tla-value val)])
            v)
       (into {})
       (#(FcnRcdValue.
          (into-array StringValue (keys %))
          (into-array Value (vals %))
          false))))

(defmethod to-tla-value tlc2.value.impl.TupleValue
  [v]
  (->> (map (fn [val]
              (to-tla-value val))
            v)
       (into-array Value)
       (#(TupleValue. %))))

(defmethod to-tla-value clojure.lang.PersistentVector
  [v]
  (->> (map (fn [val]
              (to-tla-value val))
            v)
       (into-array Value)
       (#(TupleValue. %))))

(defmethod to-tla-value clojure.lang.PersistentList
  [v]
  (->> (map (fn [val]
              (to-tla-value val))
            v)
       (into-array Value)
       (#(TupleValue. %))))

(defmethod to-tla-value tlc2.value.impl.SetEnumValue
  [v]
  (->> (map (fn [val]
              (to-tla-value val))
            v)
       (into-array Value)
       (#(SetEnumValue. % false))))

(defmethod to-tla-value clojure.lang.PersistentHashSet
  [v]
  (->> (map (fn [val]
              (to-tla-value val))
            v)
       (into-array Value)
       (#(SetEnumValue. % false))))

(defmethod to-tla-value Integer
  [v]
  (IntValue/gen v))

(defmethod to-tla-value Long
  [v]
  (IntValue/gen v))

(defmethod to-tla-value BigDecimal
  [v]
  (IntValue/gen v))

(defmethod to-tla-value String
  [v]
  (StringValue. v))

(defmethod to-tla-value Boolean
  [v]
  (BoolValue. v))

(defmethod to-tla-value :default
  [v]
  (throw
   (Exception.
    (str "type not supported: " (or (:tla-plus-type (meta v))
                                    (type v))
         "\n for value " (binding [*print-meta* true]
                           (pr-str v))))))
