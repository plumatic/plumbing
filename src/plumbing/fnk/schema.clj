(ns plumbing.fnk.schema
  "A very simple type system for a subset of schemas consisting of nested
   maps with optional or required keyword keys; used by fnk and kin.

   Since schemas are turing-complete and not really designed for type inference,
   (and for simplicity) we err on the side of completeness (allowing all legal programs)
   at the cost of soundness.

   These operations also bake in some logic specific to reasoning about Graphs,
   namely that all input keys to a node must be explicitly mentioned as optional or
   required, or provided via `instance`, and will thus deliberately drop extra key
   schemas on inputs as appropriate.  Output schemas may not have optional keys."
  (:require
   [schema.core :as s]
   [schema.macros :as macros]))

(def Schema (s/protocol s/Schema))
(def InputSchema {(s/either (s/eq s/Keyword) schema.core.OptionalKey s/Keyword) Schema})
(def OutputSchema Schema)
(def IOSchemata [(s/one InputSchema 'input) (s/one OutputSchema 'output)])

(def GraphInputSchema {(s/either schema.core.OptionalKey s/Keyword) Schema})
(def MapOutputSchema {s/Keyword Schema})
(def GraphIOSchemata [(s/one GraphInputSchema 'input) (s/one MapOutputSchema 'output)])

;;; Helper

(defmacro assert-iae
  "Like assert, but throws an IllegalArgumentException not an Error (and also takes args to format)"
  [form & format-args]
  `(when-not ~form (throw (IllegalArgumentException. (format ~@format-args)))))


;;; Punt on non-maps.

(defn non-map-union [s1 s2]
  (cond (= s1 s2) s1
        (= s1 s/Any) s2
        (= s2 s/Any) s1
        :else (s/both s1 s2)))

(defn non-map-diff
  "Return a difference of schmas s1 and s2, where one is not a map.
   Punt for now, assuming s2 always satisfies s1."
  [s1 s2]
  nil)

(defn map-schema? [m]
  (instance? clojure.lang.APersistentMap m))

;;; Input schemata

(macros/defn explicit-schema-key-map
  "Given a possibly-unevaluated map schema, return a map from bare keyword to true
   (for required) or false (for optional)"
  [s] :- {s/Keyword boolean}
  (->> s
       (keep (fn [[k v]]
               (when (s/specific-key? k)
                 [(s/explicit-schema-key k) (s/required-key? k)])))
       (into {})))

(macros/defn split-schema-keys
  "Given output of explicit-schema-key-map, split into seq [req opt]."
  [s :- {s/Keyword boolean}] :- [(s/one [s/Keyword] 'required) (s/one [s/Keyword] 'optional)]
  (->> s
       ((juxt filter remove) val)
       (mapv (partial mapv key))))

(defn- merge-on-with
  "Like merge-with, but also projects keys to a smaller space and merges them similar to the
   values."
  [key-project key-combine val-combine & maps]
  (->> (apply concat maps)
       (reduce
        (fn [m [k v]]
          (let [pk (key-project k)]
            (if-let [[ok ov] (get m pk)]
              (assoc m pk [(key-combine ok k) (val-combine ov v)])
              (assoc m pk [k v]))))
        {})
       vals
       (into {})))

(macros/defn union-input-schemata :- InputSchema
  "Returns a minimal input schema schema that entails satisfaction of both s1 and s2"
  [i1 :- InputSchema i2 :- InputSchema]
  (merge-on-with
   #(if (s/specific-key? %) (s/explicit-schema-key %) :extra)
   (fn [k1 k2]
     (cond (s/required-key? k1) k1
           (s/required-key? k2) k2
           (s/optional-key? k1) (do (assert-iae (= k1 k2)) k1)
           (= k1 k2) k1
           :else (assert-iae false "Only one extra schema allowed")))
   (fn [s1 s2]
     (if (and (map-schema? s1) (map-schema? s2))
       (union-input-schemata s1 s2)
       (non-map-union s1 s2)))
   i1 i2))

(macros/defn required-toplevel-keys :- [s/Keyword]
  "Which top-level keys are required (i.e., non-false) by this input schema."
  [input-schema :- InputSchema]
  (keep
   (fn [k]
     (when (s/required-key? k)
       (s/explicit-schema-key k)))
   (keys input-schema)))



;;; Output schemata

(defn guess-expr-output-schema
  "Guess an output schema for an expr.  Currently just looks for literal map structure and
   all keyword keys."
  [expr]
  (if (and (map? expr) (every? keyword? (keys expr)))
    (into {} (for [[k v] expr] [k (guess-expr-output-schema v)]))
    `s/Any))

;;; Combining inputs and outputs.

(macros/defn schema-diff ;; don't validate since it returns better errors.
  "Subtract output-schema from input-schema, returning nil if it's possible that an object
   satisfying the output-schema satisfies the input-schema, or otherwise a description
   of the part(s) of input-schema not met by output-schema.  Strict about the map structure
   of output-schema matching input-schema, but loose about everything else (only looks at
   required keys of output-schema."
  [input-schema output-schema] ;; not schematized since it returns more helpful errors
  (cond (not (map-schema? input-schema))
        (non-map-diff input-schema output-schema)

        (not (map-schema? output-schema))
        (macros/validation-error input-schema output-schema (list 'map? (s/explain output-schema)))

        :else
        (->> (for [[k v] input-schema
                   :when (s/specific-key? k)
                   :let [required? (s/required-key? k)
                         raw-k (s/explicit-schema-key k)
                         present? (contains? output-schema raw-k)]
                   :when (or required? present?)
                   :let [fail (if-not present?
                                'missing-required-key
                                (schema-diff v (get output-schema raw-k)))]
                   :when fail]
               [k fail])
             (into {})
             not-empty)))

(defn assert-satisfies-schema [input-schema output-schema]
  (let [fails (schema-diff input-schema output-schema)]
    (when fails (throw (ex-info (str fails) {:error    :does-not-satisfy-schema
                                             :failures fails})))))


(macros/defn ^:always-validate compose-schemata
  "Given pairs of input and output schemata for fnks f1 and f2,
   return a pair of input and output schemata for #(f2 (merge % (f1 %))).
   f1's output schema must not contain any optional keys."
  [[i2 o2] :- IOSchemata
   [i1 o1] :- [(s/one InputSchema 'input) (s/one MapOutputSchema 'output)]]
  (assert-satisfies-schema (select-keys i2 (keys o1)) o1)
  [(union-input-schemata (apply dissoc i2 (concat (keys o1) (map s/optional-key (keys o1)))) i1)
   o2])

(defn schema-key [m k]
  (cond (contains? m k)
        k

        (contains? m (s/optional-key k))
        (s/optional-key k)

        :else nil))

(defn possibly-contains? [m k]
  (boolean (schema-key m k)))

(macros/defn split-schema
  "Return a pair [ks-part non-ks-part], with any extra schema removed."
  [s :- InputSchema ks :- [s/Keyword]]
  (let [ks (set ks)]
    (for [in? [true false]]
      (into {} (for [[k v] s
                     :when (and (s/specific-key? k)
                                (= in? (contains? ks (s/explicit-schema-key k))))]
                 [k v])))))

(macros/defn sequence-schemata :- GraphIOSchemata
  "Given pairs of input and output schemata for fnks f1 and f2, and a keyword k,
   return a pair of input and output schemata for #(let [v1 (f1 %)] (assoc v1 k (f2 (merge-disjoint % v1))))"
  [[i1 o1] :- GraphIOSchemata
   [k [i2 o2]] :- [(s/one s/Keyword "key") (s/one IOSchemata "inner-schemas")]]
  (assert-iae (not (possibly-contains? i1 k)) "Duplicate key output (possibly due to a misordered graph) %s for input %s from input %s" k (s/explain i2) (s/explain i1))
  (assert-iae (not (possibly-contains? i2 k)) "Node outputs a key %s in its inputs %s" k (s/explain i2))
  (assert-iae (not (possibly-contains? o1 k)) "Node outputs a duplicate key %s given inputs %s" k (s/explain i1))
  (let [[used unused] (split-schema i2 (keys o1))]
    (assert-satisfies-schema used o1)
    [(union-input-schemata unused i1)
     (assoc o1 k o2)]))
