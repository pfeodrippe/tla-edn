(ns tla-edn.spec
  (:gen-class)
  (:require
   [clojure.java.shell :as sh]
   [clojure.pprint :as pp]
   [clojure.reflect :as reflect]
   [clojure.string :as str]
   [kaocha.classpath :as classpath])
  (:import
   (tlc2 TLC)
   (tlc2.overrides ITLCOverrides TLAPlusOperator)
   (java.lang.reflect Field Modifier)))

(def classes-to-be-loaded (atom {}))

(defn- class-exists?
  [c]
  (reflect/resolve-class (.getContextClassLoader (Thread/currentThread)) c))

(defn classes-loaded?
  []
  (every? class-exists? (keys @classes-to-be-loaded)))

(defn compile-operators
  "`ns` is a symbol (e.g 'my-ns.core).
  Does not compile if all classes are already loaded."
  [ns]
  (when-not (classes-loaded?)
    (sh/sh "mkdir" "-p" "classes")
    (compile ns)
    (compile 'tla-edn.spec)
    ;; Delete extra classes files.
    (sh/sh "find" "." "-type" "f"
           "-path" (str "./classes/" (str/replace (str (munge ns)) #"\." "/") "*")
           "-name" "*.class" "-delete")
    ;; Dynamically add `classes` folder to classpath after compilation
    ;; so we don't need to restart the JVM.
    (classpath/add-classpath "classes")))

(defmacro defop
  "Generates a class and a function which should be used to override
  a TLA+ operator."
  {:arglists '([name {:keys [:module :identifier :warn :prefix]
                      :or {warn true
                           prefix ""}
                      :as opts}
                doc-string? attr-map? [params*] prepost-map? body])}
  [name {:keys [:module :identifier :warn :prefix]
         :or {warn true
              prefix ""}
         :as opts}
   & [a b c :as fdecl]]
  (let [identifier (or identifier (str name))
        arg-list (some #(when (vector? %) %) [a b c])
        klass (symbol (str "tlc2.overrides.Operator_"
                           identifier "_"
                           (Math/abs (hash [identifier opts (count arg-list)]))))]
    `(do
       (swap! classes-to-be-loaded assoc '~klass *ns*)

       (gen-class
        :name ~klass
        :prefix ~prefix
        :main false
        :methods [~(with-meta
                     `[~(with-meta (symbol (str name))
                          `{tlc2.overrides.TLAPlusOperator
                            {identifier ~identifier
                             module ~module
                             warn ~warn}})
                       ~(vec (repeat (count arg-list) Object)) Object]
                     `{:static true})])

       (defn ~(with-meta (symbol (str prefix name))
                (merge (meta name)
                       (assoc opts :identifier identifier)))
         ~@fdecl))))

;; `tlc2.overrides.TLCOverrides` is loaded by TLC
(gen-class
 :name tlc2.overrides.TLCOverrides
 :implements [tlc2.overrides.ITLCOverrides]
 :prefix "tlc-"
 :main false)

(defn- tlc-get
  [_this]
  ;; we need to use `eval` to load the overrides classes dynamically as
  ;; the atom keeps the symbols, not the classes themselves
  (try
    (into-array Class (map resolve (or (keys @classes-to-be-loaded) [])))
    (catch Exception e (pp/pprint {::tlc-get {:exception e}}))))

(defn- get-class-non-final-static-fields
  [klass]
  {klass (->> (.getDeclaredFields klass)
              (filter #(and (Modifier/isStatic (.getModifiers %))
                            (Modifier/isFinal (.getModifiers %))))
              (mapv #(do (.setAccessible % true)
                         [% (.get % nil)]))
              (into {}))})

(defonce ^:private tlc-initial-values
  (get-class-non-final-static-fields tlc2.TLCGlobals))

(defn try-to-reset-tlc-state!
  "We try to reset TLC to a good state here, certainly it will not work for all
  (or even most) cases. If you run same things twice and they behaviour differently,
  you have to track down in TLC which static field is the corrupt."
  []
  ;; Reset `LiveWorker.errFoundByThread` static field.
  ;; This field is one of the corruptors for temporal properties violations.
  ;; There should be many others which cause this, but at least this makes some
  ;; examples behaviour like if there were run from a fresh JVM.
  (-> (->> (get-class-non-final-static-fields tlc2.tool.liveness.LiveWorker)
           vals
           first
           keys
           (some #(when (= (.getName %) "errFoundByThread")
                    %)))
      (.set nil (int -1)))

  ;; Reset `TLCGlobals`.
  (->> tlc-initial-values
       vals
       first
       (map #(.set (key %) nil (val %)))))

(defn run-spec
  ([model-path cfg-path]
   (run-spec model-path cfg-path []))
  ([model-path cfg-path cli-opts]
   (when-not (classes-loaded?)
     (print "Compiling tla-edn override operators ..." @classes-to-be-loaded)
     (doseq [ns (map ns-name (set (vals @classes-to-be-loaded)))]
       (compile-operators ns))
     (println " ... ok"))
   (try
     (doto (TLC.)
       (.handleParameters (into-array (concat ["-config" cfg-path]
                                              cli-opts
                                              [model-path])))
       .process)
     (finally
       (try-to-reset-tlc-state!)))))
