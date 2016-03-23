(ns plumbing.graph.positional
  "A compilation method for graphs that avoids maps for speed."
  (:use plumbing.core)
  (:require
   [schema.core :as s]
   [plumbing.fnk.schema :as schema]
   [plumbing.fnk.pfnk :as pfnk]
   [plumbing.fnk.impl :as fnk-impl])
  (:import
   clojure.lang.IFn))

(defn def-graph-record
  "Define a record for the output of a graph. It is usable as a function to be
  as close to a map as possible. Return the typename."
  ([g] (def-graph-record g (gensym "graph-record")))
  ([g record-type-name]
     ;; NOTE: This eval is needed because we want to define a record based on
     ;; information (a graph) that's only available at runtime.
     (eval `(defrecord ~record-type-name ~(->> g
                                               pfnk/output-schema
                                               keys
                                               (mapv (comp symbol name)))
              IFn
              (invoke [this# k#]
                (get this# k#))
              (invoke [this# k# not-found#]
                (get this# k# not-found#))
              (applyTo [this# args#]
                (apply get this# args#))))
     record-type-name))

(defn graph-let-bindings
  "Compute the bindings for functions and intermediates needed to form the body
  of a positional graph, E.g.
    [`[[f-3 ~some-function]] `[[intermediate-3 (f-3 intermediate-1 intermediate-2)]]]"
  [g g-value-syms]
  (->> g
       (map (fn [[kw f]]
              (let [f-sym (-> kw name (str "-fn") gensym)
                    arg-forms (map-from-keys g-value-syms (pfnk/input-schema-keys f))
                    [f arg-forms] (fnk-impl/efficient-call-forms f arg-forms)]
                [[f-sym f] [(g-value-syms kw) (cons f-sym arg-forms)]])))
       (apply map vector)))

(defn eval-bound
  "Evaluate a form with some symbols bound to some values."
  [form bindings]
  ((eval `(fn [~(mapv first bindings)] ~form))
   (map second bindings)))

(defn graph-form
  "Construct [body-form bindings-needed-for-eval] for a positional graph."
  [g arg-keywords]
  (let [value-syms (->> g
                        pfnk/io-schemata
                        (mapcat schema/explicit-schema-key-map)
                        (map key)
                        (map-from-keys (comp gensym name)))
        [needed-bindings value-bindings] (graph-let-bindings g value-syms)
        record-type (def-graph-record g)]
    [`(fn
        positional-graph#  ;; Name it just for kicks.
        ~(mapv value-syms arg-keywords)
        (let ~(vec (apply concat value-bindings))
          (new ~record-type ~@(->> g pfnk/output-schema keys (mapv value-syms)))))
     needed-bindings]))

(defn positional-flat-compile
  "Positional compile for a flat (non-nested) graph."
  [g]
  (let [arg-ks (->> g pfnk/input-schema-keys)
        [positional-fn-form eval-bindings] (graph-form g arg-ks)
        input-schema (pfnk/input-schema g)
        pos-fn-sym (gensym "pos")
        input-schema-sym (gensym "input-schema")
        output-schema-sym (gensym "output-schema")]
    (vary-meta ;; workaround evaluation quirks
     (eval-bound
      `(let [~pos-fn-sym ~positional-fn-form]
         ~(fnk-impl/positional-fnk-form
           (fnk-impl/schema-override 'graph-positional output-schema-sym)
           input-schema-sym
           (vec (schema/explicit-schema-key-map input-schema))
           (into {} (for [k (keys (schema/explicit-schema-key-map input-schema))] [k (symbol (name k))]))
           (list `(~pos-fn-sym ~@(mapv (comp symbol name) arg-ks)))
           nil))
      (into eval-bindings
            [[input-schema-sym input-schema]
             [output-schema-sym (pfnk/output-schema g)]]))
     assoc :schema (let [[is os] (pfnk/io-schemata g)] (s/=> os is)))))
