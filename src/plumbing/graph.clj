(ns plumbing.graph
  "A Graph is a simple, declarative way to define a composition of functions that is
   easy to define, modify, execute, test, and monitor.

   This blog post provides a high-level overview of Graph and its benefits:
   http://blog.getprismatic.com/blog/2012/10/1/prismatics-graph-at-strange-loop.html

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

   For more details and examples of Graphs, see test/plumbing/graph_examples_test.clj."
  (:require
   [lazymap.core :as lazymap]
   [plumbing.fnk.schema :as schema]
   [plumbing.fnk.pfnk :as pfnk]
   [plumbing.core :as plumbing]
   [plumbing.map :as map]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constructing graphs

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
                            (plumbing/map-vals 
                             (comp schema/required-toplevel-keys pfnk/input-schema))
                            map/topological-sort
                            reverse))
                     (mapcat #(find canonical-nodes %))
                     (apply array-map))]
      (assert (every? keyword? (keys graph)))
      (with-meta graph
        {::io-schemata (reduce schema/sequence-schemata
                               [{} {}]
                               (for [[k node] graph] 
                                 [k (pfnk/io-schemata node)]))
         ::self graph}))))

;; Any Clojure map can be treated as a graph directly, without calling ->graph
(extend-type clojure.lang.IPersistentMap
  pfnk/PFnk
  (io-schemata [g]
    (plumbing/safe-get (meta (->graph g)) ::io-schemata)))

(defn graph
  "An ordered constructor for graphs, which enforces that the Graph is provided
   in a vaid topological ordering.  This is a sanity check, and also enforces
   defining graphs in a readable way.  Most explicit graphs should be created
   with this constructor.

   (graph
     :x-plus-1   (fnk [x] (inc x))
     :2-x-plus-2 (fnk [x-plus-1] (* 2 x-plus-1)))"
  [& nodes]
  (assert (even? (count nodes)))
  (->graph (partition 2 nodes)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compiling and running graphs

(defn abstract-compile 
  "Helper method for simple graph compilations that convert a graph specification
   to a fnk that returns a Clojure map of the graph node values.  (make-map m)
   converts an initial Clojure map m to the return type of the fnk, and 
   (assoc-f m k f) associates the value given by (f) under key k to map m."
  [g make-map assoc-f]
  (if (fn? g)
    g
    (let [g (->graph g)
          req-ks (schema/required-toplevel-keys (pfnk/input-schema g))]
      (pfnk/fn->fnk
       (fn [m]
         (let [missing-keys (seq (remove #(contains? m %) req-ks))]
           (schema/assert-iae (empty? missing-keys)
                           "Missing top-level keys in graph input: %s"
                           (set missing-keys)))
         (apply
          dissoc
          (reduce
           (fn [inner [k v]]
             (let [f (abstract-compile v make-map assoc-f)]
               (schema/assert-iae (not (contains? inner k))                                  
                                  "Inner graph key %s duplicated" k)
               (assoc-f inner k #(f (select-keys inner (keys (pfnk/input-schema f)))))))
           (make-map m)
           g)
          (keys m)))
       (pfnk/io-schemata g)))))

(defn eager-compile 
  "Compile graph specification g to a corresponding fnk that returns an
   ordinary Clojure map of the node result fns on a given input."
  [g]
  (abstract-compile
   g
   (fn [m] m)
   (fn [m k f] (assoc m k (f)))))

(defn lazy-compile 
  "Compile graph specification g to a corresponding fnk that returns a
   plumbing.lazymap of the node result fns on a given input.  This fnk returns
   the lazymap immediately, and node values are computed and cached as needed
   as values are extracted from the lazymap.  Besides this lazy behavior,
   the lazymap can be used interchangeably with an ordinary Clojure map."
  [g]
  (abstract-compile
   g
   (fn [m] (into (lazymap/lazy-hash-map) m))
   (fn [m k f] (lazymap/delay-assoc m k (delay (f))))))

;; TODO: move out.
(defn par-compile [g]
  "Experimental.  Launches one future per node at startup; we probably woudln't
   use this in production, and will release more sophisticated parallel 
   compilations later.

   Compile graph specification g to a corresponding fnk that returns a
   plumbing.lazymap of the node result fns on a given input.  This fnk returns
   the lazymap immediately, and node values are computed and cached in parallel
   starting immediately (and attempts to extract values from the lazymap will
   block until each value is computed).  Besides this lazy behavior,
   the lazymap can be used interchangeably with an ordinary Clojure map."
  (abstract-compile
   g
   (fn [m] (into (lazymap/lazy-hash-map) m))
   (fn [m k f] (lazymap/delay-assoc m k (future (f))))))


(defn run 
  "Eagerly run a graph on an input by compiling and then executing on this input."
  [g input] 
  ((eager-compile g) input))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Operations on graphs

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
                      start (System/nanoTime)
                      res (f (dissoc m profile-key))]
                  (swap! pm assoc-in ks (/ (- (System/nanoTime) start) 1000000.0))
                  res))
              [(assoc (pfnk/input-schema f)
                 profile-key true)
               (pfnk/output-schema f)]))
           (->graph g))
     profile-key (plumbing/fnk [] (atom {})))))

