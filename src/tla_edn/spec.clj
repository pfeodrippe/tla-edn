(ns tla-edn.spec
  (:gen-class)
  (:require
   [babashka.process :as p]
   [clojure.java.classpath :as cp]
   [clojure.java.shell :as sh]
   [clojure.pprint :as pp]
   [clojure.reflect :as reflect]
   [clojure.string :as str]
   [kaocha.classpath :as classpath])
  (:import
   (tlc2 TLC)
   (tlc2.overrides ITLCOverrides TLAPlusOperator)
   (tlc2.value.impl Value)
   (java.lang.reflect Field Modifier)))

;; For `babashka.process`.
(prefer-method clojure.pprint/simple-dispatch clojure.lang.IPersistentMap clojure.lang.IDeref)

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
                       ~(vec (repeat (count arg-list) Value)) Value]
                     `{:static true})])

       (defn ~(with-meta (symbol (str prefix name))
                (merge (meta name)
                       (assoc opts
                              :identifier identifier
                              :klass (str klass)
                              :op-ns (str *ns*))))
         ~@fdecl))))

;; `tlc2.overrides.TLCOverrides` is loaded by TLC
(gen-class
 :name tlc2.overrides.TLCOverrides
 :implements [tlc2.overrides.ITLCOverrides]
 :prefix "tlc-"
 :main false)

(defn- tlc-get
  [_this]
  (try
    (when-let [tla-edn-namespaces (System/getProperty "TLA-EDN-Namespaces")]
      ;; Require namespaces.
      (->> (str/split tla-edn-namespaces #",")
           (remove empty?)
           (run! #(-> % symbol require))))
    (into-array Class (map resolve (or (keys @classes-to-be-loaded) [])))
    (catch Exception e (pp/pprint {::tlc-get {:exception e}}))))

(defn get-class-non-final-static-fields
  [klass]
  {klass (->> (concat (.getDeclaredFields klass)
                      (.getDeclaredFields (.getSuperclass klass)))
              (filter #(and (Modifier/isStatic (.getModifiers %))
                            (not (Modifier/isFinal (.getModifiers %)))))
              (mapv #(do (.setAccessible % true)
                         [% (.get % nil)]))
              (into {}))})

(defonce ^:private tlc-initial-values
  (merge (get-class-non-final-static-fields tlc2.TLCGlobals)
         (get-class-non-final-static-fields tlc2.tool.TLCStateMutExt)))

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

  ;; Reset some static fields to their initial values.
  (->> tlc-initial-values
       vals
       (apply concat)
       (map #(.set (key %) nil (val %)))))

(defn run-spec
  ([model-path cfg-path]
   (run-spec model-path cfg-path []))
  ([model-path cfg-path cli-opts]
   (run-spec model-path cfg-path cli-opts {}))
  ([model-path cfg-path cli-opts {:keys [:run?]
                                  :or {run? true}}]
   (when-not (classes-loaded?)
     (print "Compiling tla-edn override operators ..." @classes-to-be-loaded)
     (doseq [ns (map ns-name (set (vals @classes-to-be-loaded)))]
       (compile-operators ns))
     (println " ... ok"))
   (try
     (let [tlc (doto (TLC.) (.handleParameters (into-array (concat ["-config" cfg-path]
                                                                   cli-opts
                                                                   [model-path]))))]
       (when run? (.process tlc))
       tlc)
     (finally
       (try-to-reset-tlc-state!)))))

(defn -main
  [model-path cfg-path tlc-result-handler-str namespaces-str cli-opts-str]
  (try
    (let [cli-opts (->> (str/split cli-opts-str #" ")
                        (remove empty?))]
      ;; Require namespaces so classes are loaded.
      (->> (str/split namespaces-str #" ")
           (remove empty?)
           (run! #(-> % symbol require)))
      ;; Now it's time to run the spec.
      (if (not= tlc-result-handler-str "0")
        (-> (if (seq cli-opts)
              #(run-spec model-path cfg-path cli-opts {:run? false})
              #(run-spec model-path cfg-path [] {:run? false}))
            ((resolve (symbol tlc-result-handler-str))))
        (if (seq cli-opts)
          (run-spec model-path cfg-path cli-opts)
          (run-spec model-path cfg-path))))
    (System/exit 0)
    (catch Exception _
      (System/exit 1))))

(defn run
  "Like `run-spec`, but starts a new JVM and runs TLC from there.
  If you use it, `:tlc-result-handler` should be a var of a function which
  receives one argument."
  ([model-path cfg-path]
   (run model-path cfg-path []))
  ([model-path cfg-path cli-opts]
   (run model-path cfg-path cli-opts {}))
  ([model-path cfg-path cli-opts {:keys [:tlc-result-handler :complete-response? :loaded-classes
                                         :raw-args]
                                  :or {loaded-classes (vals @classes-to-be-loaded)}}]
   (cond-> (p/$ java
                "-Djava.awt.headless=true"
                ~(or (some->> raw-args
                              seq
                              (mapv str)
                              (str/join " "))
                     ;; Some bogus property just to make this work with `p/$`.
                     "-DXxXx=true")
                -cp ~(->> (mapv str (cp/classpath))
                          (str/join ":"))
                clojure.main -m tla-edn.spec
                ~model-path ~cfg-path
                ~(if tlc-result-handler (str (symbol tlc-result-handler)) "0")
                ~(if tlc-result-handler
                   (->> loaded-classes
                        (cons (namespace (symbol tlc-result-handler)))
                        (mapv str)
                        distinct
                        (str/join " "))
                   (->> loaded-classes
                        (mapv str)
                        distinct
                        (str/join " ")))
                ~(str/join " " cli-opts))
     (not complete-response?)
     (-> deref
         :out
         slurp
         str/split-lines))))
