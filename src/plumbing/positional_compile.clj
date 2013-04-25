(ns plumbing.positional-compile
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

(def positional? pfnk/positional-fn)

(defn positional-call-form
  [f f-sym arg-form-map]
  (let [input-schema (pfnk/input-schema f)]
    `(~f-sym
       ~@(map (comp arg-form-map) (pfnk/positional-args f)))))

(defn keyword-call-form
  [f f-sym arg-form-map]
  `(~f-sym (into {} (remove #(identical? fnk-impl/+none+ (second %))
                            ~arg-form-map))))

(defn efficient-call-form
  "Generate the most efficient available call for a function."
  [f f-sym arg-form-map]
  (if (positional? f)
    (positional-call-form f f-sym arg-form-map)
    (keyword-call-form f f-sym arg-form-map)))

(defn efficient-fn
  "Get the most efficient available version of a function."
  [f]
  (if (positional? f)
    (pfnk/positional-fn f)
    f))

(defn def-graph-record
  "Define a record for the output of a graph."
  [g]
  (let [record-type-name (gensym "graph-record")]
    (eval `(defrecord ~record-type-name ~(->> g
                                           pfnk/output-schema
                                           keys
                                           (map (comp symbol name))
                                           vec)
             ;; NOTE(leon): I wanted this record to be usable as a map, and one
             ;; of the main use cases is as a function, so I implemented IFn.
             IFn
             (invoke [this# k#]
               (get this# k#))
             (invoke [this# k# not-found#]
               (get this# k# not-found#))
             (applyTo [this# args#]
               (apply get this# args#))))
    record-type-name))

(defn generate-positional-body-expr
  "Generate the body expression for a graph with positional calls to node functions"
  [g g-fn-syms g-value-syms]
  (let [record-type (def-graph-record g)]
    `(let ~(->> (for [[k f] g]
                  [(get g-value-syms k)
                   (efficient-call-form f (get g-fn-syms k)
                                        (for-map [[arg-kw _] (pfnk/input-schema f)]
                                                 arg-kw (get g-value-syms arg-kw)))])
             (apply concat)
             vec)
       (new ~record-type ~@(->> g pfnk/output-schema keys (map g-value-syms) vec)))))

(defn validate-positional-args
  "Take a graph input schema and provided seq of arg ks, validate that it
  contains all required keys and only valid input keys."
  [input-schema arg-ks]
  ;; Args are sane.
  (assert (apply distinct? ::dummy arg-ks))  ;; Dummy in case arg-ks is empty.
  (let [arg-ks (set arg-ks)]
    ;; No missing required args.
    (doseq [[k required] input-schema]
      (schema/assert-iae (or (false? required) (contains? arg-ks k))
                         "Missing required arg key %s" k))
    ;; All args valid.
    (doseq [arg-k arg-ks]
      (schema/assert-iae (contains? input-schema arg-k)))))

(defn missing-positional-args
  "Get unused positional arguments."
  [input-schema arg-ks]
  (->> arg-ks
    (reduce dissoc input-schema)
    keys
    set))

(defn positional-fn->keyword-fn
  "Construct a keyword function that calls a positional function appropriately."
  [positional-fn positional-args]
  (fn [args-map]
    (into {}
          (apply positional-fn (for [arg positional-args]
                                 (get args-map arg fnk-impl/+none+))))))

(defn eval-bound
  "Evaluate a form with some symbols bound into some values."
  [form bindings]
  ((eval `(fn [~(->> bindings (map first) vec)] ~form))
     (map second bindings)))

(defn generate-graph-form
  [g arg-keywords fn-syms]
  (let [record-type-name (def-graph-record g)
        missing-args (missing-positional-args (pfnk/input-schema g) arg-keywords)
        value-syms (into {} (for [[kw _] (apply merge (pfnk/io-schemata g))]
                              [kw (-> kw name gensym)]))]
    `(fn
       positional-graph#
       ~(mapv value-syms arg-keywords)
       (let ~(vec (interleave (map value-syms missing-args)
                              (repeat fnk-impl/+none+)))
         ~(generate-positional-body-expr g fn-syms value-syms)))))

(defn positional-flat-compile
  "Positional compile for a flat (non-nested) graph."
  [g arg-keywords]
  (let [arg-keywords (or arg-keywords (-> g pfnk/input-schema keys))
        _ (validate-positional-args (pfnk/input-schema g) arg-keywords)
        fn-syms (into {} (for [[kw _] g] [kw (-> kw name gensym)]))
        graph-form (generate-graph-form g arg-keywords fn-syms)
        positional-fn (eval-bound graph-form
                                  (for [[kw f] g] [(fn-syms kw)
                                                   (efficient-fn f)]))]
    (pfnk/fn->fnk
      (positional-fn->keyword-fn positional-fn arg-keywords)
      (pfnk/io-schemata g)
      positional-fn
      arg-keywords)))
