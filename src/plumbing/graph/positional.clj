(ns plumbing.graph.positional
  "A compilation method for graphs that avoids maps for speed."
  (:use plumbing.core)
  (:require
   [plumbing.fnk.schema :as schema]
   [plumbing.fnk.pfnk :as pfnk]
   [plumbing.fnk.impl :as fnk-impl])
  (:import
    clojure.lang.IFn))

;; TODO: allow qualified keyword binding in graph?
;; https://github.com/Prismatic/plumbing/issues/6
;; TODO: generate .invokePrim for primitive hinted fns.

(defn def-graph-record
  "Define a record for the output of a graph. It is usable as a function to be
  as close to a map as possible. Return the typename."
  [g]
  (let [record-type-name (gensym "graph-record")]
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

(defn fn-binding-and-call
  "Compute both the binding needed to inject a function into a form and a
  let-binding form that will call the function and store the value."
  [g-value-syms [kw f]]
  (let [sym (-> kw name (str "-fn") gensym)
        arg-forms (map-from-keys g-value-syms (keys (pfnk/input-schema f)))
        [f arg-forms] (fnk-impl/efficient-call-forms f arg-forms)]
    [[sym f] [(g-value-syms kw) (cons sym arg-forms)]]))

(defn graph-let-bindings
  "Compute the bindings for functions and intermediates needed to form the body
  of a positional graph, E.g.
    [`[[f-3 ~some-function]] `[[intermediate-3 (f-3 intermediate-1 intermediate-2)]]]"
  [g g-value-syms]
  (->> g
       (map (partial fn-binding-and-call g-value-syms))
       (apply map vector)))

(defn missing-positional-args
  "Get unused positional arguments."
  [input-schema arg-ks]
  (->> arg-ks
       (reduce dissoc input-schema)
       keys
       set))

(defn validate-positional-args
  "Given a graph input schema and provided seq of arg ks, validate that it
  contains all required keys and only valid input keys."
  [input-schema arg-ks]
  (when arg-ks
    ;; Args are sane.
    (schema/assert-iae (apply distinct? ::dummy arg-ks)
                       "Invalid positional args %s contain duplicates"
                       arg-ks)
    (let [missing-args (remove (comp false? input-schema)
                               (missing-positional-args input-schema arg-ks))
          extra-args (remove (partial contains? input-schema) arg-ks)]
      (schema/assert-iae (and (empty? missing-args) (empty? extra-args))
        "Invalid positional args %s missing %s, with extra %s"
        arg-ks missing-args extra-args))
    arg-ks))

(defn positional-fn->keyword-fn
  "Construct a keyword function that calls a positional function."
  [positional-fn positional-args]
  (fn [args-map]
    (into {}
          (apply positional-fn (for [arg positional-args]
                                 (get args-map arg fnk-impl/+none+))))))

(defn eval-bound
  "Evaluate a form with some symbols bound into some values."
  [form bindings]
  ((eval `(fn [~(->> bindings (mapv first))] ~form))
     (map second bindings)))

(defn graph-form
  "Construct the form (and bindings needed for it to eval) for a positional
  graph body."
  [g arg-keywords]
  (let [value-syms (->> g pfnk/io-schemata (apply merge) keys
                        (map-from-keys (comp gensym name)))
        missing-arg-bindings (-> g
                                 pfnk/input-schema
                                 (missing-positional-args arg-keywords)
                                 (->> (map value-syms))
                                 (interleave (repeat fnk-impl/+none+))
                                 vec)
        output-values (->> g pfnk/output-schema keys (mapv value-syms))
        [needed-bindings value-bindings] (graph-let-bindings g value-syms)
        record-type (def-graph-record g)]
    [`(fn
       positional-graph#  ;; Name it just for kicks.
       ~(mapv value-syms arg-keywords)
       (let ~(into missing-arg-bindings (apply concat value-bindings))
         (new ~record-type ~@output-values)))
     needed-bindings]))

(defn positional-flat-compile
  "Positional compile for a flat (non-nested) graph."
  [g arg-keywords]
  (let [arg-keywords (or (validate-positional-args (pfnk/input-schema g)
                                                   arg-keywords)
                         (-> g pfnk/input-schema keys))
        ;; NOTE: This eval is needed because we want to make a let structure
        ;; based on information (a graph) that's only available at runtime.
        positional-fn (apply eval-bound (graph-form g arg-keywords))]
    (fnk-impl/fn->positional-fnk
      (positional-fn->keyword-fn positional-fn arg-keywords)
      (pfnk/io-schemata g)
      positional-fn
      arg-keywords)))
