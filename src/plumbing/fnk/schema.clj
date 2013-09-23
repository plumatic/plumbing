(ns plumbing.fnk.schema
  "A very simple type system for nested maps with keyword keys, used by fnk and kin.

   Input schemata specify required and optional arguments to a fnk using a nested
   map structure that parallels the desired input shape.  The leaves of an input schema
   are true (for a required key) or false (for an optional key).  Non-leaf keys are always
   required.  See 'satisfies-schema?' below for examples.

   Similarly, output schemata specify the keys outputted by a fnk (if the fnk outputs a map)
   using a similar nested map structure.  The leaves of an output schema are always true
   (for keys guaranteed to be in the output).  If the output schema is just 'true', then
   no claims are made about the output of the function (i.e., it may not even be a map).")


;;; Helper

(defmacro assert-iae
  "Like assert, but throws an IllegalArgumentException not an Error (and also takes args to format)"
  [form & format-args]
  `(when-not ~form (throw (IllegalArgumentException. (format ~@format-args)))))


;;; Input schemata

(defn union-input-schemata
  "Returns a minimal input schema schema that entails satisfaction of both s1 and s2"
  [i1 i2]
  (cond (and (map? i1) (map? i2)) (merge-with union-input-schemata i1 i2)
        (map? i1) i1
        (map? i2) i2
        :else (or i1 i2)))

(defn required-toplevel-keys
  "Which top-level keys are required (i.e., non-false) by this input schema."
  [input-schema]
  (keep
   (fn [[k v]]
     (when v k))
   input-schema))



;;; Output schemata

(defn guess-expr-output-schema
  "Guess an output schema for an expr.  Currently just looks for literal map structure and
   all keyword keys."
  [expr]
  (if (and (map? expr) (every? keyword? (keys expr)))
    (into {} (for [[k v] expr] [k (guess-expr-output-schema v)]))
    true))

;;; Combining inputs and outputs.

(defn assert-satisfies-schema
  "Does this value satisfy this schema?
   schema may be an input or output schema, and value may be an output schema or actual value.

   For example:
   (satisfies-schema? {:a {:a1 true :b1 false} :b false} {:a {:a1 9 :a3 17}})
   (not (satisfies-schema? {:a {:a1 true :a2 false} :b false} {:b 999}))
   (not (satisfies-schema? {:a {:a1 true :a2 false} :b false} {:a {:a2 4242})))"
  [schema value & [keyseq]]
  (when-not (map? schema) ;; sanity check
    (assert-iae (or (true? schema) (false? schema)) "Schema leaf is not true or false: %s" schema))
  (when (map? schema)
    (assert-iae (map? value) "Not a map %s keyseq: %s" value keyseq)
    (doseq [[ik iv] schema]
      (assert-iae (keyword? ik) "Schema has non-keyword: %s" ik)
      (when-not (false? iv)
        (if-let [[_ ov] (find value ik)]
          (assert-satisfies-schema iv ov (conj (or keyseq []) ik))
          (assert-iae false "Failed on keyseq: %s. Value is missing. \n%s \n%s" (conj (or keyseq []) ik) schema value))))))


(defn filter-and-match-schemata
  "Remove all keys provided by output schema os from input schema is, and throw
   if any matching sub-schema from os cannot satisfy the corresponding input
   schema in is."
  [is os]

  (reduce
   (fn [res [k ov]]
     (if-let [[_ iv] (find res k)]
       (do (assert-satisfies-schema iv ov [k])
           (dissoc res k))
       res))
   is
   os))


(defn compose-schemata
  "Given pairs of input and output schemata for fnks f1 and f2,
   return a pair of input and output schemata for #(f2 (merge % (f1 %)))"
  [[i2 o2] [i1 o1]]
  (assert-iae (map? o1) "Schema is not a map: %s" o1)
  [(union-input-schemata (filter-and-match-schemata i2 o1) i1)
   o2])

(defn sequence-schemata
  "Given pairs of input and output schemata for fnks f1 and f2, and a keyword k,
   return a pair of input and output schemata for #(let [v1 (f1 %)] (assoc v1 k (f2 (merge-disjoint % v1))))"
  [[i1 o1] [k [i2 o2]]]
  (let [o2 {k o2}]
    (assert-iae (not (contains? i1 k)) "Duplicate key output (possibly due to a misordered graph) %s for input %s from input %s" k i2 i1)
    (assert-iae (not (contains? i2 k)) "Node outputs a key %s in its inputs %s" k i2)
    (assert-iae (not (contains? o1 k)) "Node outputs a duplicate key %s given inputs %s" k i1)
    [(reduce
      (fn [in [new-in-k new-in-v]]
        (if (contains? o1 new-in-k)
          (do
            (assert-satisfies-schema new-in-v (o1 new-in-k))
            in)
          (assoc in new-in-k (union-input-schemata (in new-in-k) new-in-v))))
      i1
      i2)
     (merge o1 o2)]))
