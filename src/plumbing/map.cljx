(ns plumbing.map
  "Common operations on maps (both Clojure immutable and mutable Java stuff)"
  (:refer-clojure :exclude [flatten])
  (:require
   [plumbing.core :as plumbing :include-macros true]
   [plumbing.fnk.schema :as schema :include-macros true]
   #+cljs [clojure.set :as set]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Clojure immutable maps

(defn safe-select-keys
  "Like select-keys, but asserts that all keys are present."
  [m ks]
  (let [missing (remove (partial contains? m) ks)]
    (schema/assert-iae (empty? missing) "Keys %s not found in %s" (vec missing)
                       (binding [*print-length* 200]
                         (print-str (mapv key m)))))
  (select-keys m ks))

(defn merge-disjoint
  "Like merge, but throws with any key overlap between maps"
  ([] {})
  ([m] m)
  ([m1 m2]
   (let [duplicates (filter (partial contains? m2) (keys m1))]
     (schema/assert-iae (empty? duplicates) "Duplicate keys %s"
                        (vec duplicates)))
   (into (or m2 {}) m1))
  ([m1 m2 & maps]
     (reduce merge-disjoint m1 (cons m2 maps))))

(defn merge-with-key
  "Like merge-with, but the merging function takes the key being merged
   as the first argument"
  [f & maps]
  (when (some identity maps)
    (let [merge-entry (fn [m e]
                        (let [k (key e) v (val e)]
                          (if (contains? m k)
                            (assoc m k (f k (get m k) v))
                            (assoc m k v))))
          merge2 (fn [m1 m2]
                   (reduce merge-entry (or m1 {}) (seq m2)))]
      (reduce merge2 maps))))

(defn flatten
  "Transform a nested map into a seq of [keyseq leaf-val] pairs"
  [m]
  (when m
    ((fn flatten-helper [keyseq m]
       (when m
         (if (map? m)
           (mapcat (fn [[k v]] (flatten-helper (conj keyseq k) v)) m)
           [[keyseq m]])))
     [] m)))

(defn unflatten
  "Transform a seq of [keyseq leaf-val] pairs into a nested map.
   If one keyseq is a prefix of another, you're on your own."
  [s]
  (reduce (fn [m [ks v]] (if (seq ks) (assoc-in m ks v) v)) {} s))


;; TODO: make sure we're safe with false here -- pretty sure we're not.  Same for nil.
(defn map-leaves-and-path
  "Takes a nested map and returns a nested map with the same shape, where each
   (non-map) leaf v is transformed to (f key-seq v).
   key-seq is the sequence of keys to reach this leaf, starting at the root."
  ([f m] (when m (map-leaves-and-path f [] m)))
  ([f ks m]
     (if-not (map? m)
       (f ks m)
       (plumbing/for-map [[k v] m]
         k
         (map-leaves-and-path f (conj ks k) v)))))

(defn keep-leaves-and-path
  "Takes a nested map and returns a nested map with the same shape, where each
   (non-map) leaf v is transformed to (f key-seq v), or removed if it returns nil.
   key-seq is the sequence of keys to reach this leaf, starting at the root.
   Empty maps produced by this pruning are themselves pruned from the output."
  ([f m] (keep-leaves-and-path f [] m))
  ([f ks m]
     (if-not (map? m)
       (f ks m)
       (plumbing/for-map [[k ov] m
                          :let [nv (keep-leaves-and-path f (conj ks k) ov)]
                          :when (not (or (nil? nv) (and (map? nv) (empty? nv))))]
         k nv))))

(defn map-leaves
  "Takes a nested map and returns a nested map with the same shape, where each
   (non-map) leaf v is transformed to (f v)."
  ([f m] (map-leaves-and-path (fn [_ l] (f l)) m)))

(defn keep-leaves
  "Takes a nested map and returns a nested map with the same shape, where each
   (non-map) leaf v is transformed to (f v), or removed if it returns nil.
   Empty maps produced by this pruning are themselves pruned from the output."
  ([f m] (keep-leaves-and-path (fn [_ l] (f l)) m)))

(defmacro keyword-map
  "Expands to a map whose keys are keywords with the same name as the given
  symbols, e.g.:

    (let [x 41, y (inc x)]
      (keyword-map x y))

    ;; => {:x 41, :y 42}"
  [& syms]
  (when-not (every? symbol? syms)
    (throw (ex-info "Arguments to keyword-map must be symbols!" {:args syms})))
  (zipmap (map #(keyword (name %)) syms) syms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Java mutable Maps

#+clj
(do
  (defn update-key!
    "Transform value in java.util.Map m under key k with fn f."
    ([^java.util.Map m k f]
       (.put m k (f (.get m k))))
    ([^java.util.Map m k f & args]
       (.put m k (apply f (.get m k) args))))

  (defmacro get!
    "Get the value in java.util.Map m under key k.  If the key is not present,
   set the value to the result of default-expr and return it.  Useful for
   constructing mutable nested structures on the fly.

   (.add ^List (get! m :k (java.util.ArrayList.)) :foo)"
    [m k default-expr]
    `(let [^java.util.Map m# ~m k# ~k]
       (or (.get m# k#)
           (let [nv# ~default-expr]
             (.put m# k# nv#)
             nv#))))

  (defn inc-key!
    "Increment the value in java.util.Map m under key k by double d."
    [^java.util.Map m k ^double d]
    (.put m k (if-let [v (.get m k)]
                (+ (double v) d)
                d)))

  (defn inc-key-in!
    "Increment the value in java.util.Map m under key-seq ks by double d,
   creating and storing HashMaps under missing keys on the path to this leaf."
    [^java.util.Map m ks ^double d]
    (if-let [mk (next ks)]
      (recur (get! m (first ks) (java.util.HashMap.)) mk d)
      (inc-key! m (first ks) d)))


  (defn ^java.util.HashMap collate
    "Take a seq of [k v] counts and sum them up into a HashMap on k."
    [flat-counts]
    (let [m (java.util.HashMap.)]
      (doseq [[k v] flat-counts]
        (inc-key! m k v))
      m))

  (defn ^java.util.HashMap deep-collate
    "Take a seq of [kseq v] counts and sum them up into nested HashMaps"
    [nested-counts]
    (let [m (java.util.HashMap.)]
      (doseq [[ks v] nested-counts]
        (inc-key-in! m ks v))
      m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ops on graphs represented as maps.

#+clj
(defn topological-sort
  "Take an adjacency list representation of a graph (a map from node names to
   sequences of child node names), and return a topological ordering of the node
   names in linear time, or throw an error if the graph is cyclic.
   If include-leaves? is false the ordering will only include keys from child-map,
   and if true it will also include nodes only named as children in child-map."
  [child-map & [include-leaves?]]
  (let [e  (java.util.HashMap. ^java.util.Map child-map)
        re (java.util.HashMap.)
        s (java.util.Stack.)]
    (doseq [[p children] child-map
            c children]
      (when include-leaves? (when-not (.containsKey e c) (.put e c nil)))
      (update-key! re c #(cons p %)))
    (while (not (.isEmpty e))
      ((fn dfs1 [n]
         (when (.containsKey e n)
           (let [nns (.get e n)]
             (.remove e n)
             (doseq [nn nns] (dfs1 nn)))
           (.push s n)))
       (first (keys e))))
    (let [candidate (reverse (seq s))]
      (doseq [c candidate
              r (.remove re c)]
        (when (.containsKey re r)
          (throw (IllegalArgumentException. (format "Graph contains a cycle containing %s and %s" c r)))))
      candidate)))

#+cljs
(defn topological-sort
  [child-map & [include-leaves?]]
  (let [e (atom child-map)
        re (atom {})
        s (atom [])]
    (doseq [[p children] child-map
            c children]
      (when include-leaves? (when-not (find @e c) (swap! e assoc c nil)))
      (swap! re update c #(cons p %)))
    (while (seq @e)
      ((fn dfs1 [n]
         (when-let [[_ nns] (find @e n)]
           (swap! e dissoc n)
           (doseq [nn nns] (dfs1 nn))
           (swap! s conj n)))
       (first (keys @e))))
    (let [candidate (reverse @s)]
      (doseq [c candidate
              :let [rs (@re c)
                    _ (swap! re dissoc c)]
              r rs]
        (when (find @re r)
          (throw (ex-info (str "Graph contains a cycle containing " c " and " r) {:nodes [c r]}))))
      candidate)))
