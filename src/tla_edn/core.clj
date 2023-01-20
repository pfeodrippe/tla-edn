(ns tla-edn.core
  (:import
   (tlc2.value.impl Value IntValue RecordValue BoolValue FcnRcdValue
                    StringValue TupleValue SetEnumValue BoolValue)
   (util UniqueString))
  (:require [clojure.string :as str]))

(def ^:private ^:dynamic *string-to-keyword?* false)

(defprotocol TLAPlusEdn
  (-to-edn [this]))

(defn to-edn
  ([this]
   (to-edn this {}))
  ([this {:keys [:string-to-keyword?]
          :or   {string-to-keyword? false}}]
   (binding [*string-to-keyword?* string-to-keyword?]
     (-to-edn this))))

(extend-protocol TLAPlusEdn
  tlc2.value.impl.RecordValue
  (-to-edn [v]
    (let [name->value (zipmap (map -to-edn (.-names v))
                              (map -to-edn (.-values v)))]
      (with-meta
        ;; We add a default value for empty because TLA+ does not
        ;; have a literal for a empty map.
        (if (= name->value {:tla-edn.record/empty? true})
          {}
          name->value)
        {:tla-plus-type tlc2.value.impl.RecordValue})))

  tlc2.value.impl.FcnRcdValue
  (-to-edn [v]
    (with-meta
      (zipmap (map #(-to-edn (.val %)) (.-domain v))
              (map -to-edn (.-values v)))
      {:tla-plus-type tlc2.value.impl.FcnRcdValue}))

  tlc2.value.impl.TupleValue
  (-to-edn [v]
    (with-meta
      (mapv -to-edn (.getElems v))
      {:tla-plus-type tlc2.value.impl.TupleValue}))

  tlc2.value.impl.SetEnumValue
  (-to-edn [v]
    (with-meta
      (set (map -to-edn (.toArray (.-elems v))))
      {:tla-plus-type tlc2.value.impl.SetEnumValue}))

  tlc2.value.impl.IntValue
  (-to-edn [v]
    (.val v))

  tlc2.value.impl.StringValue
  (-to-edn [v]
    (let [s (str (.val v))]
      (if (str/includes? s "__")
        (let [[nmsp n] (str/split s #"__")]
          (keyword nmsp n))
        (if *string-to-keyword?*
          (keyword s)
          s))))

  UniqueString
  (-to-edn [v]
    (let [s (str v)]
      (if (str/includes? s "__")
        (let [[nmsp n] (str/split s #"__")]
          (keyword nmsp n))
        (if *string-to-keyword?*
          (keyword s)
          s))))

  tlc2.value.impl.BoolValue
  (-to-edn [v]
    (.getVal v)))

(defmulti to-tla-value
  (fn [v]
    (or (:tla-plus-type (meta v))
        (type v))))

(defmethod to-tla-value tlc2.value.impl.RecordValue
  [v]
  (if (empty? v)
    (to-tla-value {:tla-edn.record/empty? true})
    (->> (map (fn [[k val]]
                [(-> k to-tla-value .getVal)
                 (to-tla-value val)])
              v)
         (into {})
         (#(RecordValue.
            (into-array UniqueString (keys %))
            (into-array Value (vals %))
            false)))))

(defmethod to-tla-value clojure.lang.APersistentMap
  [v]
  (if (empty? v)
    (to-tla-value {:tla-edn.record/empty? true})
    (->> (map (fn [[k val]]
                [(-> k to-tla-value .getVal)
                 (to-tla-value val)])
              v)
         (into {})
         (#(RecordValue.
            (into-array UniqueString (keys %))
            (into-array Value (vals %))
            false)))))

(defmethod to-tla-value tlc2.value.impl.FcnRcdValue
  [v]
  (->> (map (fn [[k val]]
              [(to-tla-value k)
               (to-tla-value val)])
            v)
       (into {})
       (#(FcnRcdValue.
          (into-array Value (keys %))
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

(defmethod to-tla-value clojure.lang.Keyword
  [v]
  (StringValue. (if (namespace v)
                  (str (namespace v) "__" (name v))
                  (name v))))

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

(comment

  (let [result (tla-edn.spec/run "wire.tla" "wire.cfg" [] {:complete-response? true})]
    (with-open [rdr (clojure.java.io/reader (:out result))]
      (binding [*in* rdr]
        (loop []
          (when-let [line (read-line)]
            (println line)
            (recur))))))

  ())
