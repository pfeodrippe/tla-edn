(ns tla-edn.spec
  (:require
   [clojure.java.shell :as sh]
   [clojure.pprint :as pp]
   [clojure.reflect :as reflect]
   [clojure.string :as str])
  (:import
   (tlc2.overrides ITLCOverrides TLAPlusOperator)
   (tlc2 TLC)))

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
    ;; delete extra classes files
    (sh/sh "find" "." "-type" "f"
           "-path" (str "./classes/" (str/replace (str (munge ns)) #"\." "/") "*")
           "-name" "*.class" "-delete")))

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

(defn run-spec
  "This should be run from a fresh JVM as the TLA+ model checker is
  not stable after first run (there is no known way to reset it without
  to restart the JVM)."
  ([model-path cfg-path]
   (run-spec model-path cfg-path []))
  ([model-path cfg-path cli-opts]
   (when-not (classes-loaded?)
     (print "Compiling tla-edn override operators ..." @classes-to-be-loaded)
     (doseq [ns (map ns-name (set (vals @classes-to-be-loaded)))]
       (compile-operators ns))
     (println " ... ok"))
   (doto (TLC.)
     (.handleParameters (into-array (concat ["-config" cfg-path]
                                            cli-opts
                                            [model-path])))
     .process)))
