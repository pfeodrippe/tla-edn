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
    ;; Single-pass construction avoiding intermediate sequences
    (let [names (.-names v)
          values (.-values v)
          n (alength names)]
      (if (zero? n)
        (with-meta {} {:tla-plus-type tlc2.value.impl.RecordValue})
        (let [result (loop [i 0, acc (transient {})]
                       (if (< i n)
                         (recur (unchecked-inc i)
                                (assoc! acc
                                        (-to-edn (aget names i))
                                        (-to-edn (aget values i))))
                         (persistent! acc)))]
          (with-meta
            (if (= result {:tla-edn.record/empty? true})
              {}
              result)
            {:tla-plus-type tlc2.value.impl.RecordValue})))))

  tlc2.value.impl.FcnRcdValue
  (-to-edn [v]
    ;; Single-pass construction
    (let [domain (.-domain v)
          values (.-values v)
          n (alength domain)]
      (with-meta
        (loop [i 0, acc (transient {})]
          (if (< i n)
            (recur (unchecked-inc i)
                   (assoc! acc
                           (-to-edn (.val ^Value (aget domain i)))
                           (-to-edn (aget values i))))
            (persistent! acc)))
        {:tla-plus-type tlc2.value.impl.FcnRcdValue})))

  tlc2.value.impl.TupleValue
  (-to-edn [v]
    ;; Use reduce for single-pass
    (let [elems (.getElems v)
          n (alength elems)]
      (with-meta
        (loop [i 0, acc (transient [])]
          (if (< i n)
            (recur (unchecked-inc i) (conj! acc (-to-edn (aget elems i))))
            (persistent! acc)))
        {:tla-plus-type tlc2.value.impl.TupleValue})))

  tlc2.value.impl.SetEnumValue
  (-to-edn [v]
    ;; Single-pass set construction
    (let [arr (.toArray (.-elems v))
          n (alength arr)]
      (with-meta
        (loop [i 0, acc (transient #{})]
          (if (< i n)
            (recur (unchecked-inc i) (conj! acc (-to-edn (aget arr i))))
            (persistent! acc)))
        {:tla-plus-type tlc2.value.impl.SetEnumValue})))

  tlc2.value.impl.IntValue
  (-to-edn [v]
    (.val v))

  tlc2.value.impl.StringValue
  (-to-edn [v]
    (let [s (str (.val v))
          idx (.indexOf s "__")]
      (if (>= idx 0)
        (keyword (subs s 0 idx) (subs s (+ idx 2)))
        (if *string-to-keyword?*
          (keyword s)
          s))))

  UniqueString
  (-to-edn [v]
    (let [s (str v)
          idx (.indexOf s "__")]
      (if (>= idx 0)
        (keyword (subs s 0 idx) (subs s (+ idx 2)))
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
    ;; Single-pass array construction - avoid intermediate HashMap
    (let [entries (seq v)
          n (count entries)
          names (make-array UniqueString n)
          values (make-array Value n)]
      (loop [i 0, entries entries]
        (when-let [[k val] (first entries)]
          (aset names i (-> k to-tla-value .getVal))
          (aset values i (to-tla-value val))
          (recur (unchecked-inc i) (rest entries))))
      (RecordValue. names values false))))

(defmethod to-tla-value clojure.lang.APersistentMap
  [v]
  (if (empty? v)
    (to-tla-value {:tla-edn.record/empty? true})
    ;; Single-pass array construction - avoid intermediate HashMap
    (let [entries (seq v)
          n (count entries)
          names (make-array UniqueString n)
          values (make-array Value n)]
      (loop [i 0, entries entries]
        (when-let [[k val] (first entries)]
          (aset names i (-> k to-tla-value .getVal))
          (aset values i (to-tla-value val))
          (recur (unchecked-inc i) (rest entries))))
      (RecordValue. names values false))))

(defmethod to-tla-value tlc2.value.impl.FcnRcdValue
  [v]
  ;; Single-pass array construction
  (let [entries (seq v)
        n (count entries)
        keys-arr (make-array Value n)
        values-arr (make-array Value n)]
    (loop [i 0, entries entries]
      (when-let [[k val] (first entries)]
        (aset keys-arr i (to-tla-value k))
        (aset values-arr i (to-tla-value val))
        (recur (unchecked-inc i) (rest entries))))
    (FcnRcdValue. keys-arr values-arr false)))

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
