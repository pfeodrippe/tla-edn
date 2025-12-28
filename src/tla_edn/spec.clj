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
   (tlc2 TLC TLCGlobals)
   (tlc2.tool TLCIsolated)
   (tlc2.overrides ITLCOverrides TLAPlusOperator)
   (tlc2.value.impl Value)
   (tla2sany.semantic Context)
   (util ToolIO SimpleFilenameToStream)
   (java.lang.reflect Field Modifier)
   (java.io ByteArrayOutputStream PrintStream)))

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
      ;; Require namespaces so operator override classes are available.
      (->> (str/split tla-edn-namespaces #",")
           (remove empty?)
           (run! #(-> % symbol require))))
    (into-array Class (map resolve (or (keys @classes-to-be-loaded) [])))
    (catch Exception e
      (pp/pprint {::tlc-get {:exception e}}))))

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

(defn- reset-liveness-state!
  "Reset LiveWorker.errFoundByThread which tracks if a liveness error was found.
  This field is not reset by TLCGlobals.deepReset() and causes subsequent liveness
  checks to fail."
  []
  (try
    (let [live-worker-class (Class/forName "tlc2.tool.liveness.LiveWorker")
          err-field (.getDeclaredField live-worker-class "errFoundByThread")]
      (.setAccessible err-field true)
      (.set err-field nil (int -1)))
    (catch Exception e
      ;; LiveWorker may not be loaded yet, which is fine
      nil)))

(defn- reset-sany-state!
  "Reset SANY's internal caches which are not cleared by TLCGlobals.deepReset().
  This is essential for running different specs sequentially in the same JVM."
  []
  ;; Reset the global context where module symbols are cached.
  ;; Without this, SANY reuses cached modules by name (e.g. 'spec')
  ;; causing errors when switching between different specs.
  ;; Context.reInit() creates a fresh context and re-initializes built-in operators.
  (Context/reInit)
  ;; Clear the default file resolver to force fresh resolution of spec files.
  ;; This ensures that changed spec files are re-read from disk.
  (ToolIO/setDefaultResolver nil)
  ;; NOTE: Do NOT call UniqueString.initialize() - it clears the intern table
  ;; which contains built-in TLA+ operator names (=, TRUE, /\, etc.)
  ;; that are essential for parsing.
  )

(defn- reset-unique-string-locations!
  "Reset all UniqueString.loc fields to -1 to allow clean Defns lookups.

  ROOT CAUSE FIX: UniqueString objects are stored globally in internTbl and persist
  across TLC runs. Each UniqueString has a 'loc' field that stores the index into
  the Defns.table array where that operator's definition lives.

  When running a second spec, a NEW Defns table is created, but the UniqueString
  objects still have 'loc' values pointing to the OLD table's indices. This causes
  lookups to return wrong definitions (e.g., 'invariant' returning 'Integers.Times').

  The fix is to reset all loc fields to -1 before each TLC run, so Defns.put()
  will assign fresh indices for the new table."
  []
  (try
    ;; Get the static internTbl field from UniqueString
    (let [unique-string-class (Class/forName "util.UniqueString")
          intern-tbl-field (.getDeclaredField unique-string-class "internTbl")]
      (.setAccessible intern-tbl-field true)
      (let [intern-tbl (.get intern-tbl-field nil)
            ;; Get the table array from InternTable
            table-field (.getDeclaredField (class intern-tbl) "table")]
        (.setAccessible table-field true)
        (let [table (.get table-field intern-tbl)]
          ;; Reset loc to -1 on every UniqueString in the table
          (dotimes [i (alength table)]
            (when-let [us (aget table i)]
              (.setLoc us (int -1))))))
      ;; Also reset varCount to 0 so variable/definition distinction is clean
      (let [var-count-field (.getDeclaredField unique-string-class "varCount")]
        (.setAccessible var-count-field true)
        (.set var-count-field nil (int 0))))
    (catch Exception e
      ;; Log but don't fail - this is a best-effort reset
      (println "Warning: Failed to reset UniqueString locations:" (.getMessage e)))))

(defn reset-tlc-state!
  "Reset TLC to clean state using TLC's native comprehensive reset mechanism.
  This handles all global/static state including operator caches, parser state,
  model values, thread-local RNG, and more."
  []
  (TLCGlobals/deepReset)
  (reset-liveness-state!)
  (reset-sany-state!)
  ;; CRITICAL: Reset UniqueString.loc fields to allow clean Defns lookups.
  ;; This is the fix for running different specs sequentially.
  (reset-unique-string-locations!))

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
   ;; Reset BEFORE running to clear any cached state from previous runs.
   ;; This is essential for running different specs sequentially.
   (reset-tlc-state!)
   (try
     (let [tlc (doto (TLC.) (.handleParameters (into-array (concat ["-config" cfg-path]
                                                                   cli-opts
                                                                   [model-path]))))]
       (when run? (.process tlc))
       tlc)
     (finally
       (reset-tlc-state!)))))

(defn run-spec-isolated
  "Run TLC in a fully isolated class loader.
   This creates a fresh environment for each run, avoiding all module caching issues.
   Returns {:exit-code int :output string :error string}

   Options:
   - :loaded-classes - seq of namespace strings for operator overrides"
  ([model-path cfg-path]
   (run-spec-isolated model-path cfg-path []))
  ([model-path cfg-path cli-opts]
   (run-spec-isolated model-path cfg-path cli-opts {}))
  ([model-path cfg-path cli-opts {:keys [loaded-classes]
                                  :or {loaded-classes (vals @classes-to-be-loaded)}}]
   (when-not (classes-loaded?)
     (print "Compiling tla-edn override operators ..." @classes-to-be-loaded)
     (doseq [ns (map ns-name (set (vals @classes-to-be-loaded)))]
       (compile-operators ns))
     (println " ... ok"))
   (let [args (into-array String
                          (concat ["-config" cfg-path]
                                  cli-opts
                                  [model-path]))
         ;; Set namespaces property so TLCOverrides can find operator overrides
         namespaces-str (->> loaded-classes
                             (mapv str)
                             distinct
                             (str/join ","))
         old-namespaces-prop (System/getProperty "TLA-EDN-Namespaces")
         out-stream (ByteArrayOutputStream.)
         err-stream (ByteArrayOutputStream.)
         old-out System/out
         old-err System/err]
     (try
       (System/setProperty "TLA-EDN-Namespaces" namespaces-str)
       (System/setOut (PrintStream. out-stream))
       (System/setErr (PrintStream. err-stream))
       (let [exit-code (TLCIsolated/run args)]
         {:exit-code exit-code
          :output (.toString out-stream)
          :error (.toString err-stream)})
       (finally
         (System/setOut old-out)
         (System/setErr old-err)
         (if old-namespaces-prop
           (System/setProperty "TLA-EDN-Namespaces" old-namespaces-prop)
           (System/clearProperty "TLA-EDN-Namespaces")))))))

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
