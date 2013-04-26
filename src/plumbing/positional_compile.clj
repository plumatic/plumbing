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

(def positional? (comp boolean pfnk/positional-fn))

(defn positional-args-form
  [f arg-form-map]
  (map arg-form-map (pfnk/positional-args f)))

(defn keyword-args-form
  [arg-form-map]
  [`(into {} (remove #(identical? fnk-impl/+none+ (second %))
                     ~arg-form-map))])

(defn efficient-fn-info
  [g]
  (for-map [[kw f] g]
           kw
           (let [sym (-> kw name gensym)
                 args-form (if (positional? f)
                             (partial positional-args-form f)
                             keyword-args-form)]
             {:sym sym
              :efficient-call (fn [arg-form-map]
                                `(~sym ~@(args-form arg-form-map)))
              :efficient-fn (if (positional? f)
                              (pfnk/positional-fn f)
                              f)})))

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
  [g g-fn-info g-value-syms]
  (let [record-type (def-graph-record g)]
    `(let ~(->>
             (for [[k f] g
                   :let [call-fn (-> g-fn-info (get k) :efficient-call)
                         arg-forms (for-map [[arg-kw _] (pfnk/input-schema f)]
                                            arg-kw (get g-value-syms arg-kw))]]
                  [(get g-value-syms k)
                   (call-fn arg-forms)])
             (apply concat)
             vec)
       (new ~record-type
            ~@(->> g pfnk/output-schema keys (map g-value-syms) vec)))))

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

(defn eval-bound-fn-info
  [form fn-info]
  (eval-bound form
              (for [[_ {:keys [sym efficient-fn]} info] fn-info]
                          [sym efficient-fn])))

(defn generate-graph-form
  [g arg-keywords fn-info]
  (let [record-type-name (def-graph-record g)
        missing-args (missing-positional-args (pfnk/input-schema g) arg-keywords)
        value-syms (into {} (for [[kw _] (apply merge (pfnk/io-schemata g))]
                              [kw (-> kw name gensym)]))]
    `(fn
       positional-graph#
       ~(mapv value-syms arg-keywords)
       (let ~(vec (interleave (map value-syms missing-args)
                              (repeat fnk-impl/+none+)))
         ~(generate-positional-body-expr g fn-info value-syms)))))

(defn positional-flat-compile
  "Positional compile for a flat (non-nested) graph."
  [g arg-keywords]
  (let [arg-keywords (or arg-keywords (-> g pfnk/input-schema keys))
        _ (validate-positional-args (pfnk/input-schema g) arg-keywords)
        fn-info (efficient-fn-info g)
        graph-form (generate-graph-form g arg-keywords fn-info)
        positional-fn (eval-bound-fn-info graph-form fn-info)]
    (pfnk/fn->fnk
      (positional-fn->keyword-fn positional-fn arg-keywords)
      (pfnk/io-schemata g)
      positional-fn
      arg-keywords)))
