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

(defn- parse-explicit-output-schema-spec
  "Convert an output schema spec (where leaf meaps can be given as seqs of keys) to 
   an ordinary output schema.  

   For example, (= (parse-output-schema {:a [:b :c]}) {:a {:b true :c true}})"
  [s]
  (cond (map? s)
        (do (assert-iae (every? keyword? (keys s)) "Output schema has non-keyword keys: %s" s)
            (into {} (for [[k v] s] [k (parse-explicit-output-schema-spec v)])))

        (coll? s)
        (do (assert-iae (every? keyword? s) "Output schema has non-keyword keys: %s" s)
            (into {} (for [k s] [k true])))

        :else
        (do (assert-iae (true? s) "Output schema has non-true leaf: %s" s)
            true)))

(defn guess-expr-output-schema 
  "Guess an output schema for an expr.  Currently just looks for literal map structure."
  [expr]
  (if (map? expr)
    (into {} (for [[k v] expr] [k (guess-expr-output-schema v)]))
    true))

(defn- intersect-output-schemata 
  "Combine information from an explicit output schema and guess from examining a body expr."
  [expr-schema explicit-schema]
  (cond (true? expr-schema) explicit-schema
        (true? explicit-schema) expr-schema
        :else
        (do (assert-iae (and (map? expr-schema) (map? explicit-schema))
                        "Non-map schema in %s or %s" expr-schema explicit-schema)
            (assert-iae (every? expr-schema (keys explicit-schema))
                        "Explicit schema %s claims impossible keys given expr-schema %s" explicit-schema  expr-schema)
            (into {}                  
                  (for [[k v] expr-schema]
                    [k (intersect-output-schemata v (explicit-schema k true))])))))

(defn make-output-schema 
  "Take an expr and an (possibly partial or absent) explicit specification of the output schema of the expr,
   and return the most specific output schema inferrable from these specifications."
  [body-expr explicit-spec]
  (intersect-output-schemata 
   (guess-expr-output-schema body-expr)
   (parse-explicit-output-schema-spec explicit-spec)))



;;; Combining inputs and outputs.

(defn satisfies-schema?
  "Does this value satisfy this schema?
   schema may be an input or output schema, and value may be an output schema or actual value.

   For example:
   (satisfies-schema? {:a {:a1 true :b1 false} :b false} {:a {:a1 9 :a3 17}})
   (not (satisfies-schema? {:a {:a1 true :a2 false} :b false} {:b 999}))
   (not (satisfies-schema? {:a {:a1 true :a2 false} :b false} {:a {:a2 4242})))"
  [schema value]
  (when-not (map? schema)
    (assert-iae (or (true? schema) (false? schema)) "Schema leaf is not true or false: %s" schema))
  (or (not (map? schema))
      (and (map? value)
           (every? (fn [[ik iv]]                
                     (assert-iae (keyword? ik) "Schema has non-keyword: %s" ik)
                     (or (false? iv)
                         (when-let [[_ ov] (find value ik)]
                           (satisfies-schema? iv ov))))
                   schema))))


(defn filter-and-match-schemata
  "Remove all keys provided by output schema os from input schema is, and throw
   if any matching sub-schema from os cannot satisfy the corresponding input
   schema in is."
  [is os]
  (reduce
   (fn [res [k ov]]
     (if-let [iv (res k)]
       (do (assert-iae (satisfies-schema? iv ov)                       
                       "Output schema %s under key %s does not satisfy input schema %s"
                       ov k iv)
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
    (assert-iae (not (contains? i1 k)) "Duplicate key output %s for input %s from input %s" k i2 i1)
    (assert-iae (not (contains? i2 k)) "Node outputs a key %s in its inputs %s" k i2)
    (assert-iae (not (contains? o1 k)) "Node outputs a duplicate key %s given inputs %s" k i1)
    [(reduce
      (fn [in [new-in-k new-in-v]]
        (if (contains? o1 new-in-k)
          (do (assert-iae (satisfies-schema? new-in-v (o1 new-in-k))
                          "Schema %s not satisfied by %s" new-in-v (o1 new-in-k))
              in)
          (assoc in new-in-k (union-input-schemata (in new-in-k) new-in-v))))
      i1
      i2)
     (merge o1 o2)]))

