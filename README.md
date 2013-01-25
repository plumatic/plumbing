# Plumbing and Graph: The Clojure Utility Belt

Key functions and abstractions for building awesome things in Clojure. 

## Graph: The Functional Swiss-Army Knife

Graph is a simple and *declarative* way to describe how functions compose in a larger computation. Here's a simple example:

```clojure
(def stats-graph
  "A graph specifying the computation of univariate statistics"
  {:n  (fnk [xs]   (count xs))
   :m  (fnk [xs n] (/ (sum identity xs) n))
   :m2 (fnk [xs n] (/ (sum #(* % %) xs) n))
   :v  (fnk [m m2] (- m2 (* m m)))})   
```


A graph is just a map from keyword to annoymous keyword functions `fnk` ([learn more](#fnk)). This graph represents the steps in taking a sequence of numbers (`xs`) and  producing univariate statistics on those numbers (e.g., the mean `m` and the variance `v`).  The names of arguments to each `fnk` represent other graph steps that must happen before the step executes. For instance, in the above, to execute `:v`, you must execute the `:m` and `:m2` steps (mean and second moment respectively).

We can "compile" this graph to produce a single function, which also checks the map represents a valid graph:

```clojure
(require '[plumbing.graph :as graph])
(def stats-eager (graph/eager-compile stats-graph))

(= {:n 4
	:m 3
	:m2 (/ 25 2)
	:v (/ 7 2)}
   (stats-eager {:xs [1 2 3 6]}))

  
;; Missing :xs key exception
(thrown? Throwable (stats-eager {:ys [1 2 3]}))
```

If we want to add another step to `stats-graph` computation, we can do so simply using ordinary operations on maps:

```clojure
(def extended-stats-graph
  (assoc stats-graph
    :sd (fnk [^double v] (Math/sqrt v))))
	
(= {:n 4
    :m 3
    :m2 (/ 25 2)
    :v (/ 7 2)
    :sd (Math/sqrt 3.5)}
   (extended-stats-graph {:xs [1 2 3 6]}))	
```

A graph only encodes the structure of computation and there are many compilation strategies. We can do a lazy compilation so only values  which are requested are computed. Since a graph encodes dependency, we can also do parallel-compile so functions that don't depend on one-another are done in separate threads.

```clojure
(def lazy-stats (graph/lazy-compile stats-graph))

(deftest lazy-stats-test
  (let [output (lazy-stats {:xs [1 2 3 6]})]
    ;; Nothing has actually be computed yet
    (is (= (/ 25 2) (:m2 output)))
    ;; Now :n, :m, and :m2 have been computed, but :v is still behind a delay        
    ))

(def par-stats (graph/par-compile stats-graph))

(deftest par-stats-test
  (let [output (lazy-stats {:xs [1 2 3 6]})]
    ;; Nodes are being computed in futures, with :m and :m2 going in parallel
    (is (= (/ 7 2) (:v output)))))
```	

We can ask stats-graph for information about its inputs and outputs (automatically computed from the definition):


```clojure
(require '[plumbing.fnk.pfnk :as pfnk])

(deftest stats-schema-test
  ;; stats-graph takes a map with one required key, :xs
  (is (= {:xs true}
         (pfnk/input-schema stats-graph)))
  
  ;; stats-graph outputs a map with four keys, :n, :m, :m2, and :v
  (is (= {:n true :m true :m2 true :v true}
         (pfnk/output-schema stats-graph))))
```

We can automatically profile each sub-function in 'stats' to see how long it takes to execute.

```clojure
(def profiled-stats (graph/eager-compile (graph/profiled ::profile-data stats-graph)))

(deftest profiled-stats-test
  (test-stats-fn profiled-stats))
  
;;; times in milliseconds for each step:
(= {:n 1.001, :m 0.728, :m2 0.996, :v 0.069}
   (::profile-data (profiled-stats {:xs (range 10000)})))
```


<h2 id="fnk">Bring on (de)fnk</h2>

Many of the functions we write take a single map argument and we have expectations about which keys must be present and which can are optional. We developed a new style of binding (read more here) to make this a lot easier and to check that input data has the right 'shape'. We call these keyword functions (`defnk`) and here's what one looks like:

```clojure
(use 'plumbing.core)
(defnk simple-fnk [a b c] 
  (+ a b c))
  
(= 6  (simple-fnk {:a 1 :b 2 :c 3}))
;; Below throws: Key :c not found in (:a :b)
(thrown? Throwable (simple-fnk {:a 1 :b 2})) 
```

You can declare a key as optional and provide a default like this:
```clojure
(defnk simple-opt-fnk [a b {c 1}] 
  (+ a b c))
  
(= 4  (simple-opt-fnk {:a 1 :b 2}))   
```

You can do nested map bindings like this:
```clojure
(defnk simple-nested-fnk [a [:b b1] c] 
  (+ a b1 c))
  
(= 6  (simple-nested-fnk {:a 1 :b {:b1 2} :c 3}))   
;; Below throws: Expected a map at key-path [:b], got type class java.lang.Long
(thrown? Throwable (simple-nested-fnk {:a 1 :b 1 :c 3})) 
```

Of course, you can bind multiple variables from an inner map and do multiple levels of nesting:
```clojure
(defnk simple-nested-fnk [a [:b b1 [:c {d 3}]]] 
  (+ a b1 d))
  
(= 4  (simple-nested-fnk {:a 1  :b {:b1 2 :c {:d 1}}}))   
(= 5  (simple-nested-fnk {:a 1 :b {:b1 1 :c {}}}))
```

You can use this binding style in a `let` statement using `letk` 
or within an annoymous function by using `fnk`. 


## Working with Maps

The most useful tool for working with maps is `for-map`, which is `for` for building maps:

```clojure
(use 'plumbing.core)
(= (for-map [i (range 4) j (range 4) 
	        :let [s (+ i j)]
			:when (< s 6)] s [i j])
	{0 [0 0], 1 [1 0], 2 [2 0], 3 [3 0], 4 [3 1], 5 [3 2]})
```

Another useful map function we use a lot is `map-vals`:

```clojure
;; return k -> (f v) for [k, v] in map
(= (map-vals inc {:a 0 :b 0})
   {:a 1 :b 1})
```

`safe-get` is `get` but throws when the key doesn't exist:

```clojure
;; IllegalArgumentException Key :c not found in {:a 1, :b 2} 
(thrown? Exception (safe-get {:a 1 :b 2}} :c)
```

Check out [`plumbing.core`](https://github.com/Prismatic/plumbing/blob/master/src/plumbing/core.clj) for many other useful functions.

## Pipe Control

Ever wanted to conditionally do steps in a `->>` or `->`, now you can with 
penguin operators. Here's a single-arrow example:

```clojure
(use 'plumbing.core)
(=  (let [add-b? false]
	   (-> {:a 1}
		   (merge {:c 2})
		   (?> add-b? assoc :b 2)))
	{:a 1 :c 2})

(=  (let [inc-all? true]
	   (-> (range 10)
		   (filter even?)
		   (?>> inc-all? map inc)))
	{:a 1 :c 2})
```

You can also make a function from a `->` or `->>` expression:

```clojure
  (= ((fn-> (assoc :a 1)) {:b 1})
     {:a 1 :b 1})
```

