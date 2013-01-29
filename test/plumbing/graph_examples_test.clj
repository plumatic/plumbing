(ns plumbing.graph-examples-test
  (:use plumbing.core clojure.test)
  (:require
   [plumbing.fnk.schema :as schema]
   [plumbing.fnk.pfnk :as pfnk]
   [plumbing.graph :as graph]
   [plumbing.map :as map]
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Motivation

;; Functional programming works by composing smaller functions into 
;; bigger ones.  These compositions are typically opaque: once you've made
;; them, you can't programmaticaly look inside to reason about them, 
;; modify and extend them, observe the operation of the pieces, etc.  

;; Graph provides a very simple way to specify *transparent*  compositions
;; with all of these properties.

;; This file will start by illustrating the basic idea behind Graph
;; with a simple example, and then move on to precisely define Graphs
;; and show what you can do with them via lots more examples.


;; As a first example, consider this function:

(defn stats 
  "Take a map {:xs xs} and return a map of simple statistics on xs"
  [{:keys [xs] :as m}]
  (assert (contains? m :xs))
  (let [n  (count xs)
        m  (/ (sum identity xs) n)
        m2 (/ (sum #(* % %) xs) n) 
        v  (- m2 (* m m))]
    {:n n   ; count   
     :m m   ; mean 
     :m2 m2 ; mean square
     :v v   ; variance
     }))

;; 'stats' effectively composes four different functions: count, mean,
;; mean-square, and variance, each of which can depend on previous outputs.

;; What's wrong with this method of composition?  Well, maybe sometimes
;; we only need to know the mean of our sample, other times we just want
;; the mean and mean-square, and sometimes we need everything.
;; But by calling 'stats' in all but the final case, we waste effort by 
;; computing unneeded statistics. On the other hand, if we attempted to 
;; break stats apart into separate functions for each statistic, we'd waste 
;; effort re-computing :m and :m2 when we want it all.

;; Or, suppose that we want to monitor the individual sub-computations in 
;; this function to see how much time each takes in production. We would 
;; have to individually instrument each computation with a time measurement, 
;; which is both verbose and error-prone.

;; (These criticisms may seem silly in the context of this simple example. 
;; Check out our blog post [1] for more about our real systems, which have 
;; similar issues but include dozens of components, polymorphism, and other 
;; complicating requirements.

;; [1] http://blog.getprismatic.com/blog/2012/10/1/prismatics-graph-at-strange-loop.html

;; The core issue is that while as programmers we can see the individual
;; components of 'stats' and their relationships, the rest of our code and 
;; tooling does not have access to this information because it is locked 
;; up inside an *opaque* function.

;; In contrast, here is a graph specifying the same computation:

(def stats-graph
  "A graph specifying the same computation as 'stats'"
  {:n  (fnk [xs]   (count xs))
   :m  (fnk [xs n] (/ (sum identity xs) n))
   :m2 (fnk [xs n] (/ (sum #(* % %) xs) n))
   :v  (fnk [m m2] (- m2 (* m m)))})

;; We can "compile" this graph to produce a function equivalent to the
;; opaque example above:

(def stats2 (graph/eager-compile stats-graph))

(defn test-stats-fn [f]
  (is (= {:n 4
          :m 3
          :m2 (/ 25 2)
          :v (/ 7 2)}
         (f {:xs [1 2 3 6]})))
  
  (is (thrown? Throwable (f {:ys [1 2 3]}))))

(deftest test-stats-and-stats-graph
  (test-stats-fn stats)
  (test-stats-fn stats2))

;; So, stats-graph can do everything that 'stats' can.  But, what *else*
;; can it do?  Here are a few simple examples.

;; 1.  We can modify and extend stats-graph using ordinary operations on maps.

(def extended-stats-graph
  (assoc stats-graph
    :sd (fnk [^double v] (Math/sqrt v))))

(def extended-stats (graph/eager-compile extended-stats-graph))

(deftest extended-stats-test
  (is (= {:n 4
          :m 3
          :m2 (/ 25 2)
          :v (/ 7 2)
          :sd (Math/sqrt 3.5)}
         (extended-stats {:xs [1 2 3 6]}))))


;; 2.  We can lazily compile stats-graph, so only needed values are computed,
;;     or parallel-compile it so functions that don't depend on one-another
;;     are done in separate threads.

(def lazy-stats (graph/lazy-compile stats-graph))

(deftest lazy-stats-test
  (let [output (lazy-stats {:xs [1 2 3 6]})]
    ;; Nothing has actually been computed yet
    (is (= (/ 25 2) (:m2 output)))
    ;; Now :n, :m, and :m2 have been computed, but :v is still behind a delay        
    ))

;; In cases where only some results from a set of related calculations are 
;; needed, this lazy compilation can be very convenient and powerful.

(def par-stats (graph/par-compile stats-graph))

(deftest par-stats-test
  (let [output (par-stats {:xs [1 2 3 6]})]
    ;; Nodes are being computed in futures, with :m and :m2 going in parallel
    (is (= (/ 7 2) (:v output)))))

;; Similarly, auto-parallelization allows us to focus on the structure of our
;; problem, and lets the compiler do the work of figuring out what can be 
;; done in parallel.


;; 3.  We can ask stats-graph for information about its inputs and outputs
;;     (automatically computed from the definition)

(deftest stats-schema-test
  ;; stats-graph takes a map with one required key, :xs
  (is (= {:xs true}
         (pfnk/input-schema stats-graph)))
  
  ;; stats-graph outputs a map with four keys, :n, :m, :m2, and :v
  (is (= {:n true :m true :m2 true :v true}
         (pfnk/output-schema stats-graph))))


;; 4.  We can automatically profile each sub-function in 'stats' to see how
;;     long it takes to execute.

(def profiled-stats (graph/eager-compile (graph/profiled ::profile-data stats-graph)))

(deftest profiled-stats-test
  (is (= (/ 7 2) (:v (profiled-stats {:xs [1 2 3 6]})))))

;;; times in milliseconds for each step:
;; (= {:n 1.001, :m 0.728, :m2 0.996, :v 0.069}
;;    @(::profile-data (profiled-stats {:xs (range 10000)}))


;; These examples just scratch the surface of what we can do with Graph.
;; After explaining more about what Graphs are and how they work, we show 
;; many more examples at the end of this document.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; What is a Graph?

;; As we saw above, Graph is a simple way to specify function compositions
;; using Clojure maps.  A graph implicitly specifies a function from an 
;; input map to an output map, where each key in the output map is 
;; computed by the corresponding function in the Graph.  These node functions
;; can take their inputs from the input to the parent function, or the
;; outputs of other nodes.  For example, the mean function :m in 
;; 'stats-graph' takes 'xs' from the input to 'stats' and 'n' from the 
;; output of the previous :n function.

;; For this to work, we need to know the names of the arguments to each of
;; the functions in the Graph.  This is where 'keyword functions' come
;; into play.  A keyword function is just a Clojure fn that takes a single
;; argument, a map with keyword keys, and also responds to the
;; pfnk/io-schemata call with information about which keys it expects
;; or accepts in its input (and output, if applicable).

;; For example, we can manually define a keyword function that takes 
;; keys :a and :b from the input map and outputs {:x (+ a b)}:

(def a-manual-keyword-function
  (pfnk/fn->fnk
   (fn [{:keys [a b] :as m}]
     (assert (every? #(contains? m %) [:a :b]))
     {:x (+ a b)})
   [{:a true :b true}
    {:x true}]))

(defn test-simple-keyword-function [f]
  (is (= {:x 3}
         (f {:a 1 :b 2})))

  ;; a keyword function knows its io-schemata
  (is (= [{:a true :b true}
          {:x true}]
         (pfnk/io-schemata f)))
  
  ;; a keyword function should throw if required keys not given.
  (is (thrown? Throwable (f {:a 3}))))

(deftest a-manual-keyword-function-test
  (test-simple-keyword-function a-manual-keyword-function))

;; Because this is rather tedious, we've defined new macros 'fnk' and
;; 'defnk' that define keyword functions that automatically compute 
;; their own input and output schemata, and also include a new 
;; destructuring syntax that we find convenient for working with Graphs
;; (and elsewhere). 

(defnk a-simple-fnk
  "This fnk is equivalent to a-manual-keyword-function."
  [a b]
  {:x (+ a b)})

(deftest a-simple-fnk-test
  (test-simple-keyword-function a-simple-fnk))


;; In this simple case, fnks are similar to Clojure's {:keys []} 
;; destructuring.  Functionally, the main differences are slightly
;; cleaner syntax for optional arguments and nested maps, assertions
;; by default that all required keys are present, and the automatic
;; assignment of input and output schema metadata.

;; (You do not have to use 'fnk' to use Graph, however -- see 
;; plumbing.fnk.fnk-examples-test for more motivation and details about 
;; fnk, as well as a description of how to define keyword functions
;; that can be used with Graph without the 'fnk' macro.)

;; With this addition, we can now formally define a Graph: a Graph is just 
;; a (possibly-nested) map from keywords to fnks. The required keys of each
;; fnk specify the node relationships: each required key refers to the output
;; of a previous node function under the same name, or if no such node is 
;; present, the value associated with this keyword in the input map. 

(defn graph? [g]
  (or (and (fn? g) (try (pfnk/io-schemata g) true (catch Throwable t false)))
      (and (map? g)
       (every? (fn [[k sub-g]]
                 (and (keyword? k)
                      (graph? sub-g)))
               g))))

(deftest graph?-test
  (is (graph? stats-graph))
  (is (not (graph? {:a (fn [x] (inc x))})))
  (is (not (graph? {:a 42})))
  (is (not (graph? {"a" (fnk [x] (inc x))}))))


;; The entire Graph itself specifies a fnk from input parameters to a map
;; of results, just like the example 'stats' fn written explicitly with 
;; defn and let above. Note, however, that the graph itself just describes the 
;; compositional structure of the computation, but says nothing about 
;; the actual execution strategy.

;; That said, to be well-defined, a Graph must specify an *acyclic* 
;; computation.  That is, there must exist a valid topological ordering
;; of the node functions, so that each node function only depends on the
;; outputs of previous node functions.

;; When you ask for the io-schemata of a Graph, a valid topological 
;; ordering is automatically computed, and an error will be thrown if the
;; Graph has a cycle.  You can also call graph/->graph on a map to 
;; check that it represents a valid Graph, and return an array-map 
;; version of the Graph where the nodes are in a valid topological order.

(deftest topological-graph-ordering-test  
  ;; ->graph finds the only valid topological order
  (is (= [:x :y] 
         (keys (graph/->graph
                {:y (fnk [x] (* 2 x))
                 :x (fnk [a] (inc a))}))))
  
  ;; ->graph throws if no valid topological order exists
  (is (thrown? Exception 
               (graph/->graph
                {:y (fnk [x] (* 2 x))
                 :x (fnk [a x] (inc (+ a x)))}))))

;; If you're defining a graph explicitly in code, it's rather bad form
;; to put the nodes out of topological order (like the first example
;; above).  To enforce that the nodes are actually provided in a valid 
;; order (for readability, and to catch bugs), you can use the 'graph'
;; constructor:

(deftest ordered-graph-constructor-test
  ;; nodes in of topological order.
  (is (= [:x :y] 
         (keys (graph/graph
                :x (fnk [a] (inc a))
                :y (fnk [x] (* 2 x))))))

  ;; nodes out of topological order
  (is (thrown? Exception
               (graph/graph
                :y (fnk [x] (* 2 x))
                :x (fnk [a] (inc a))))))


;; Finally, while all of the Graphs we've seen thus far have a single
;; level of nesting, we can also create deeper graphs where each node
;; is itself a graph, and use fnk's nested binding syntax to pull 
;; keys out of subgraphs.  Like in the flat case, to the extent possible,
;; everything is checked at compile-time to ensure the composition is
;; well-formed.

(def a-nested-graph
  {:x (fnk [a] (inc a))
   :y {:y1 (fnk [a x] (* a x))
       :y2 (fnk [b y1] (* y1 (dec b)))}
   :z (fnk [x [:y y1 y2]] ;; nested binding!
        (- y1 y2))})

(deftest a-nested-graph-test
  (let [f (graph/eager-compile a-nested-graph)]
    (is (= [{:a true :b true}
            {:x true
             :y {:y1 true :y2 true}
             :z true}]
           (pfnk/io-schemata f)))
    (is (= -6
           (- (* 1 (inc 1)) (* 1 (inc 1) (dec 5)))
           (:z (f {:a 1 :b 5})))))
  
  ;; The presence of the correct nested keys is checked when a graph 
  ;; is verified -- so here, we know that :x does not produce sub-key
  ;; :x-out2 required by the node function of :y.
  (is (thrown? Exception
               (graph/->graph
                {:x (fnk [a] {:x-out (inc a)})
                 :y (fnk [[:x x-out2]] (inc x-out2))}))))

;; For more about the nested binding syntax of fnk, check out the
;; docstring or plumbing.fnk.fnk-examples-test.clj.

;; Nested graphs essentially model lexical scope, so a subgraph
;; can refer to any node or input at a previous ancestor, but 
;; it can not refer directly to later nodes or arbitrary descencents
;; of ancestors.  

;; We use nested graphs extensively when composing production services,
;; an application we explore briefly at the end of this file.

;; Finally, there is one more thing to know about the semantics of 
;; Graph nodes -- they get only what they ask for.  So while 'fnk' 
;; and friends allow binding a symbol to the entire top-level map
;; (or additional keys) with :as (or &), in the context of a Graph,
;; only explicitly named required or optional keys will be provided.

(deftest graph-as-test
  (let [x-fn (fnk [a {b 2} :as m] ;; b is optional with default 2, m bound to whole input
               [a b m])]
    (is (= [1 2 {:a 1}]
           (x-fn {:a 1})))
    ;; When called directly, the :as binding gets the whole map
    (is (= [1 10 {:a 1 :b 10 :c 100}]
           (x-fn {:a 1 :b 10 :c 100})))
    
    ;; When called in a graph, it only gets what it asked for -- :a and :b
    (is (= [1 10 {:a 1 :b 10}]
           (:x ((graph/eager-compile {:x x-fn}) {:a 1 :b 10 :c 100}))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compiling Graphs

;; Compiling a graph produces a single fnk that can be called with an
;; input map to produce an output map.  We've already illustrated the
;; basic compilation strategies above -- eager, lazy, and parallel, 
;; so we'll just say a bit more about them here.

;; For example, here's a graph with the same shape as 'stats' but 
;; where the nodes are slow to compute:

(def slow-graph
  (graph/graph
   :a (fnk [x] (Thread/sleep 100) (inc x))
   :b1 (fnk [a] (Thread/sleep 200) (* a 2))
   :b2 (fnk [a] (Thread/sleep 300) (- a 3))
   :c (fnk [b1 b2] (Thread/sleep 50) (+ b1 b2))))

;; Then, we can example the properties of the different compilations
;; on this graph.  First, some scaffolding:

(defmacro timed-is
  "Test that (pred form) and also that form takes about
   expected-ms milliseconds to run, and return the output of form"
  [expected-ms pred form]
  `(let [start# (millis)
         v# ~form
         pv# (~pred v#)
         end# (millis)]
     (is (identity pv#))
     (is (< (Math/abs (- ~expected-ms (- end# start#))) 20))
     v#))

(deftest ^:slow timed-compilation-tests
  (let [in {:x 3}
        out {:a 4 :b1 8 :b2 1 :c 9}]
    ;; eager computes everything before returning
    (let [eager (graph/eager-compile slow-graph)
          eager-out (timed-is 650 identity (eager in))]
      (timed-is 0 true? (= out eager-out)))

    ;; lazy computes stuff as needed
    (let [lazy (graph/lazy-compile slow-graph)
          lazy-out (timed-is 0 keys (lazy in))]
      (timed-is 400 true? (= (:b2 out) (:b2 lazy-out))) ;; 100 + 300
      (timed-is 250 true? (= (:c out) (:c lazy-out))) ;; 200 + 50
      (timed-is 0 true? (= (:b1 out) (:b1 lazy-out)))) ;; already computed by :c
      
        ;; lazy computes stuff as needed
    (let [par (graph/par-compile slow-graph)
          par-out (timed-is 0 keys (par in))]
      (timed-is 450 true? (= out (into {} par-out)))))) ;; :b1 and :b2 are done in parallel




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; More fun stuff with Graphs

;; In the above examples, we've already seen a number of interesting 
;; things we can do with Graphs that we could not do with ordinary
;; (opaque) function compositions, including:
;;  - automatic computation of input and output schemas
;;  - automatic lazy and parallel compilations
;;  - easy extension with normal map operations (assoc, etc.)
;;  - automatic profiling of individual node execution times

;; Here we explore a few more interesting ideas centered around
;; defining, executing, monitoring, and testing production services.
;; These are techniques we already use in our real production services,
;; and we plan to release our real implementations soon (we wouldn't
;; recommend using the simple test implementations here for that purpose).

;; While the basic machinery in this setting is the same as above, its 
;; use is rather different.  In particular, each service is defined by a 
;; Graph that executes once on service startup, with each node building 
;; a resource (such as a storage system, cache, thread pool, web server, etc)
;; that can be in turn used by other resources.  The output of this
;; graph is a map of resources that can be introspected for debugging
;; while the service runs, and on shutdown can be used to cleanly close
;; out all components in the correct order (the inverse of startup).

;; In this case, a very simple approach would be to have each node function
;; return a map with two possible keys:
;;   - :resource is the actual resource that should be passed into other 
;;     nodes,
;;   - :shutdown is a shutdown hook to cleanly shut down this resource.

;; Under this simple scheme, we can define functions to start up and 
;; shutdown a service in just 20 lines of code.

(defn resource-transform [g]
  (assoc (map/map-leaves 
          (fn [node-fn]
            (pfnk/fn->fnk
             (fn [m]
               (let [r (node-fn m)]
                 (when-let [shutdown (:shutdown r)]
                   (swap! (::shutdown-hooks m) conj shutdown))
                 (assert (contains? r :resource))
                 (:resource r)))
             [(assoc (pfnk/input-schema node-fn) ::shutdown-hooks true)
              (pfnk/output-schema node-fn)]))                         
          g)
    ::shutdown-hooks (fnk [] (atom nil))))

(defn start-service [graph params]  
  ((graph/eager-compile (resource-transform graph)) params))

(defn shutdown-service [m]
  (doseq [f @(::shutdown-hooks m)]
    (f)))

;; Now we'll have to get a bit imaginative, since we don't have much 
;; in the way of interesting resources available on our classpath
;; at the moment.  We can start with simple resources like this one:

(defnk schedule-work 
  "Cron for clojure fns. Schedule a single fn with a pool to run every rate seconds."
  [work-fn rate]
  (let [pool (java.util.concurrent.Executors/newSingleThreadScheduledExecutor)]
    (.scheduleAtFixedRate pool work-fn (long 0) (long rate) java.util.concurrent.TimeUnit/SECONDS)
    {:resource nil
     :shutdown #(.shutdown pool)}))

;; and then we can build up more complex resources as Graphs from these
;; components (getting a bit silly for the sake of brevity):

(def expiring-cache
  (graph/graph 
   :atom (fnk [] {:resource (atom nil)})
   :prune (fnk [atom max-age {prune-rate 1}]
               (schedule-work 
                {:work-fn (fn [] (swap! atom (fn [m]
                                               (let [cutoff (- (millis) (* 1000 max-age))]
                                                 (for-map [[k [v ts]] m
                                                           :when (> ts cutoff)]
                                                   k [v ts])))))
                 :rate prune-rate}))))

(defn ec-get [ec k f]
  (letk [[atom] ec]
    (if-let [[_ [v]] (find @atom k)]
      v
      (let [v (f k)]
        (swap! atom assoc k [v (millis)])
        v))))
  

;; and finally we can build up services from these components

(def pointless-service
  (graph/graph
   :cache expiring-cache
   :sql-query (fnk []
                ;; pretend this gives a database query fn
                (throw (RuntimeException.)))
   :web-server (fnk [cache sql-query]
                 ;; pretend this is a real webserver, not just a fn.
                 {:resource (fn [q] (ec-get cache q sql-query))})))

;; and we can start and stop these services, and mock out components

(defn test-pointless-service [graph params]
  (let [svc-map (start-service 
                 (assoc graph
                   :sql-query (fnk mock-sql [] {:resource inc}))
                 params)
        web-server (:web-server svc-map)]
    
    (is (= 3 (web-server 2)))
    (is (= 11 (web-server 10)))
    (is (= #{2 10} (-> svc-map :cache :atom deref keys set)))
    (Thread/sleep 3000)
    (is (= #{} (-> svc-map :cache :atom deref keys set)))
    
    (shutdown-service svc-map)))

(deftest ^:slow pointless-service-test
  (test-pointless-service pointless-service {:max-age 1}))

;; To make this sort of composition useful on a large scale, we also
;; need a way to provide contextual arguments to subgraphs, 
;; via a version of comp-partial.  We do this extensively in our 
;; real services, but are still working on cleaning this mechanism
;; up and preparing it for release.  

;; Stay tuned for future releases that will include our library
;; of useful resources and subgraphs, utilities for starting, stopping,
;; and managing services based on Graph, and more.



