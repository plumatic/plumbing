(ns plumbing.graph
  "A Graph is a simple, declarative way to define a composition of functions that is
   easy to define, modify, execute, test, and monitor.

   This blog post provides a high-level overview of Graph and its benefits:
   http://plumatic.github.io/prismatics-graph-at-strange-loop

   Concretely, a Graph specification is just a Clojure (nested) map with keyword keys
   and keyword functions at the leaves.

   A Graph is defined recursively as either:
     1. a keyword function (i.e., fn satisfying PFnk), or
     2. a Clojure map from keywords to (sub)graphs.

   A Graph is a declarative specification of a single keyword function that
   produces a map output, where each value in the output is produced by executing
   the corresponding keyword function in the Graph.  The inputs to the keyword
   function are given by the outputs of other nodes in the graph with matching
   keywords (mimicking lexical scope in the case of nested maps), or failing that,
   from keywords in the input map.

   For more details and examples of Graphs, see test/plumbing/graph_examples_test.cljx."
  (:refer-clojure :exclude [compile])
  (:require
   #+clj [lazymap.core :as lazymap]
   [schema.core :as s]
   #+clj [schema.macros :as schema-macros]
   [plumbing.fnk.schema :as schema :include-macros true]
   [plumbing.fnk.pfnk :as pfnk]
   #+clj [plumbing.fnk.impl :as fnk-impl]
   #+clj [plumbing.graph.positional :as graph-positional]
   [plumbing.core :as plumbing :include-macros true]
   [plumbing.map :as map])
  #+cljs (:require-macros [schema.macros :as schema-macros]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constructing graphs

(defn working-array-map
  "array-map in cljs no longer preserves ordering, replicate the old functionality."
  [& args]
  (schema-macros/if-cljs
   (.fromArray cljs.core/PersistentArrayMap (apply array args) true true)
   (apply array-map args)))

(defn ->graph
  "Convert a graph specification into a canonical well-formed 'graph', which
   is an array-map with nodes in a correct topological order that will respond
   to 'io-schemata' with a specification of the graph inputs and outputs.

   The graph specification can be a Clojure map, in which case the topological
   order will be computed (an error will be thrown for cyclic specifications),
   or a sequence of key-value pairs that are already in a valid topological order
   (an error will be thrown if the order is not valid).  Values in the input
   sequence are also converted to canonical graphs via recursive calls to ->graph."
  [graph-nodes]
  (if (or (fn? graph-nodes) (= graph-nodes (::self (meta graph-nodes))))
    graph-nodes
    (let [canonical-nodes (plumbing/map-vals ->graph graph-nodes)
          graph (->> (if-not (map? graph-nodes)
                       (map first graph-nodes)
                       (->> canonical-nodes
                            (plumbing/map-vals pfnk/input-schema-keys)
                            map/topological-sort
                            reverse))
                     (mapcat #(find canonical-nodes %))
                     (apply working-array-map))]
      (assert (every? keyword? (keys graph)))
      (with-meta graph
        {::io-schemata (update-in (reduce schema/sequence-schemata
                                          [{} {}]
                                          (for [[k node] graph]
                                            [k (pfnk/io-schemata node)]))
                                  [0] assoc s/Keyword s/Any)
         ::self graph}))))

;; Any Clojure map can be treated as a graph directly, without calling ->graph

(defn io-schemata* [g]
  (plumbing/safe-get (meta (->graph g)) ::io-schemata))

(extend-protocol pfnk/PFnk
  #+clj clojure.lang.IPersistentMap
  #+cljs cljs.core.PersistentArrayMap
  (io-schemata [g] (io-schemata* g))
  #+cljs cljs.core.PersistentHashMap
  (io-schemata [g] (io-schemata* g)))

(defn- split-nodes [s]
  (loop [in s out []]
    (if-let [[f & r] (seq in)]
      (cond (keyword? f) ;; key then value
            (recur (next r) (conj out [f (first r)]))

            (fn? f)
            (do (schema/assert-iae (pfnk/fnk-name f) "Inline fnks must have a name (to be used as a key)")
                (recur r (conj out [(keyword (pfnk/fnk-name f)) f])))

            :else ;; inline graph
            (recur r (into out f)))
      out)))

(defn graph
  "An ordered constructor for graphs, which enforces that the Graph is provided
   in a valid topological ordering.  This is a sanity check, and also enforces
   defining graphs in a readable way.  Most explicit graphs should be created
   with this constructor.

   (graph
     :x-plus-1   (fnk [x] (inc x))
     :2-x-plus-2 (fnk [x-plus-1] (* 2 x-plus-1)))

   in addition, an 'inline' graph can be provided in place of a key-value
   sequence, which will be merged into the graph at this position.

   a named fnk can also be provided in place of a key-value pair,
   where the fnk's name (as a keyword) is the implicit key."
  [& nodes]
  (let [partitioned (split-nodes nodes)]
    (schema/assert-distinct (map first partitioned))
    (->graph partitioned)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compiling and running graphs

#+clj
(defn eager-compile
  "Compile graph specification g to a corresponding fnk that is optimized for
   speed. Wherever possible, fnks are called positionally, to reduce the
   overhead of creating and destructuring maps, and the return value is a
   record, which is much faster to create and access than a map.  Compilation
   is relatively slow, however, due to internal calls to 'eval'."
  [g]
  (if (fn? g)
    g
    (let [g (for [[k sub-g] (->graph g)]
              [k (eager-compile sub-g)])]
      (graph-positional/positional-flat-compile (->graph g)))))

#+clj
(defn positional-eager-compile
  "Like eager-compile, but produce a non-keyword function that can be called
   with args in the order provided by arg-ks, avoiding the overhead of creating
   and destructuring a top-level map.  This can yield a substantially faster
   fn for Graphs with very computationally inexpensive node fnks."
  [g arg-ks]
  (fnk-impl/positional-fn (eager-compile g) arg-ks))

(defn simple-flat-compile
  "Helper method for simple (non-nested) graph compilations that convert a graph
   specification to a fnk that returns a Clojure map of the graph node values.
   (make-map m) converts an initial Clojure map m to the return type of the fnk,
   and (assoc-f m k f) associates the value given by (f) under key k to map m."
  [g check-input? make-map assoc-f]
  (let [g (->graph g)
        req-ks (schema/required-toplevel-keys (pfnk/input-schema g))]
    (pfnk/fn->fnk
     (fn [m]
       (when check-input?
         (let [missing-keys (seq (remove #(contains? m %) req-ks))]
           (schema/assert-iae (empty? missing-keys)
                              "Missing top-level keys in graph input: %s"
                              (set missing-keys))))
       (apply
        dissoc
        (reduce
         (fn [inner [k node-f]]
           (schema/assert-iae (not (contains? inner k))
                              "Inner graph key %s duplicated" k)
           (assoc-f inner k node-f))
         (make-map m)
         g)
        (keys m)))
     (pfnk/io-schemata g))))

(defn simple-hierarchical-compile
  "Hierarchical extension of simple-nonhierarchical-compile."
  [g check-input? make-map assoc-f]
  (if (fn? g)
    g
    (simple-flat-compile
     (for [[k sub-g] (->graph g)]
       [k (simple-hierarchical-compile sub-g check-input? make-map assoc-f)])
     check-input? make-map assoc-f)))

(defn restricted-call
  "Call fnk f on the subset of keys its input schema explicitly asks for"
  [f in-m]
  (f (select-keys in-m (pfnk/input-schema-keys f))))

(defn interpreted-eager-compile
  "Compile graph specification g to a corresponding fnk that returns an
   ordinary Clojure map of the node result fns on a given input.  The
   compilation is much faster than 'eager-compile', but the compiled fn
   will typically be much slower."
  [g]
  (simple-hierarchical-compile
   g
   true
   (fn [m] m)
   (fn [m k f] (assoc m k (restricted-call f m)))))

#+clj
(defn lazy-compile
  "Compile graph specification g to a corresponding fnk that returns a
   lazymap of the node result fns on a given input.  This fnk returns
   the lazymap immediately, and node values are computed and cached as needed
   as values are extracted from the lazymap.  Besides this lazy behavior,
   the lazymap can be used interchangeably with an ordinary Clojure map.
   Required inputs to the graph are checked lazily, so you can omit input
   keys not required by unneeded output keys."
  [g]
  (simple-hierarchical-compile
   g
   false
   (fn [m] (reduce-kv assoc (lazymap/lazy-hash-map) m)) ;; into is extremely slow on lazymaps.
   (fn [m k f] (lazymap/delay-assoc m k (delay (restricted-call f m))))))

#+clj ;; TODO: move out.
(defn par-compile
  "Experimental.  Launches one future per node at startup; we probably woudln't
   use this in production, and will release more sophisticated parallel
   compilations later.

   Compile graph specification g to a corresponding fnk that returns a
   lazymap of the node result fns on a given input.  This fnk returns
   the lazymap immediately, and node values are computed and cached in parallel
   starting immediately (and attempts to extract values from the lazymap will
   block until each value is computed).  Besides this lazy behavior,
   the lazymap can be used interchangeably with an ordinary Clojure map."
  [g]
  (simple-hierarchical-compile
   g
   true
   (fn [m] (into (lazymap/lazy-hash-map) m))
   (fn [m k f] (lazymap/delay-assoc m k (future (restricted-call f m))))))

(defn compile
  "Compile graph specification g to a corresponding fnk using the a default
   compile strategy for host.
   Clojure: eager-compile
   ClojureScript: interpreted-eager-compile"
  [g]
  #+clj  (eager-compile g)
  #+cljs (interpreted-eager-compile g))

(defn run
  "Eagerly run a graph on an input by compiling and then executing on this input."
  [g input]
  ((interpreted-eager-compile g) input))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Higher-order functions on graphs

(defn check-comp-partial!
  "Check that instance-fn is a valid fn to comp-partial with graph g."
  [g instance-fn]
  (let [is (pfnk/input-schema g)
        os (pfnk/output-schema instance-fn)]
    (schema/assert-iae (map? os) "instance-fn must have output metadata")
    (let [extra-ks (remove #(schema/possibly-contains? is %) (keys os))]
      (schema/assert-iae (empty? extra-ks) "instance-fn provides unused keys: %s" (vec extra-ks)))
    (doseq [[k s] os]
      (schema/assert-satisfies-schema (or (get is k) (get is (s/optional-key k))) s))))

(defn comp-partial-fn
  "Return a new pfnk representing the composition #(f (merge % (other %)))"
  [f other]
  (pfnk/fn->fnk
   (fn [m] (f (merge m (other m))))
   (schema/compose-schemata (pfnk/io-schemata f) (pfnk/io-schemata other))))

(defn comp-partial
  "Experimental.

   An extension of pfnk/comp-partial that supplies new parameters to a subgraph,
   useful in composing hierarchical graphs.

   g is a graph, and instance-fn is a fnk that takes arguments from the surrounding
   context and produces new parameters that are fed into g.  Works by comp-partialing
   all leafs that expects any parameter produced by instance-fn with instance-fn,
   so beware of expensive instance-fns, or those that expect caching of some sort
   (i.e., attempt to generate shared state).

   Throws an error if any parameter supplied by instance-fn is not used by at least
   one node in g."
  [g instance-fn]
  (if (fn? g)
    (comp-partial-fn g instance-fn)
    (let [os (pfnk/output-schema instance-fn)]
      (check-comp-partial! g instance-fn)
      (->graph
       (map/map-leaves
        (fn [node-fn]
          (if (some os (pfnk/input-schema-keys node-fn))
            (comp-partial-fn node-fn instance-fn)
            node-fn))
        g)))))

(defmacro instance
  "Experimental.

   Convenience macro for comp-partial, used to supply inline parameters to a
   subgraph (or fnk).

   Example:
   (= {:x 21}
      (run (instance {:x (fnk [a] (inc a))} [z] {:a (* z 2)})
           {:z 10}))"
  ([g m] `(instance ~g [] ~m))
  ([g bind m]
     `(comp-partial ~g (plumbing/fnk ~bind ~m))))

(defn profiled
  "Modify graph spec g, producing a new graph spec with a new top-level key
   'profile-key'.  After each node value is computed, the number of milliseconds
   taken to compute its value will be stored under an atom at 'profile-key'."
  [profile-key g]
  (assert (and (keyword? profile-key) (not (get g profile-key))))
  (->graph
   (assoc (map/map-leaves-and-path
           (fn [ks f]
             (pfnk/fn->fnk
              (fn [m]
                (let [pm (plumbing/safe-get m profile-key)
                      start #+clj (System/nanoTime) #+cljs (plumbing/millis)
                      res (f (dissoc m profile-key))]
                  (swap! pm assoc-in ks
                         #+clj  (/ (- (System/nanoTime) start) 1000000.0)
                         #+cljs (- (plumbing/millis) start))
                  res))
              [(assoc (pfnk/input-schema f)
                 profile-key s/Any)
               (pfnk/output-schema f)]))
           (->graph g))
     profile-key (plumbing/fnk [] (atom {})))))
