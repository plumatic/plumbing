# Plumbing and Graph: The Clojure utility belt

This first release includes our ['Graph' library](http://blog.getprismatic.com/blog/2012/10/1/prismatics-graph-at-strange-loop.html), our plumbing.core library of very commonly used functions (the only thing we :use across our codebase), and a few other supporting namespaces.  

Check back here often, because we'll keep adding more useful namespaces and functions as we work through cleaning up and open-sourcing our stack of Clojure libraries.

## Graph: The Functional Swiss-Army Knife

Functional programming works by composing smaller functions into bigger ones. Graph is a simple and *declarative* way to represent these compositions, which allows greater freedom to analyze, change, compose, and monitor them. Here's a simple example:

```clojure
(defn stats 
  "Take a map {:xs xs} and return a 
   map of simple statistics on xs"
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

(def stats-graph
  "A graph specifying the same computation as 'stats'"
  {:n  (fnk [xs]   (count xs))
   :m  (fnk [xs n] (/ (sum identity xs) n))
   :m2 (fnk [xs n] (/ (sum #(* % %) xs) n))
   :v  (fnk [m m2] (- m2 (* m m)))})   
```

A graph is just a map from keyword to keyword functions ([read more](#fnk)). We can "compile" this `stats-graph` graph to produce a function equivalent to the opaque `stats` fn  above:

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

We can also modify and extend `stats-graph` using ordinary operations on maps.

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

We can lazily compile stats-graph, so only needed values are computed, or parallel-compile it so functions that don't depend on one-another are done in separate threads.

```clojure
(def lazy-stats (graph/lazy-compile stats-graph))

(deftest lazy-stats-test
  (let [output (lazy-stats {:xs [1 2 3 6]})]
    ;; Nothing has actually been computed yet
    (is (= (/ 25 2) (:m2 output)))
    ;; Now :n, :m, and :m2 have been computed, but :v is still behind a delay        
    ))

(def par-stats (graph/par-compile stats-graph))

(deftest par-stats-test
  (let [output (lazy-stats {:xs [1 2 3 6]})]
    ;; Nodes are being computed in futures, with :m and :m2 going in parallel
    (is (= (/ 7 2) (:v output)))))
```	

We can ask stats-graph for information about its inputs and outputs (automatically computed from its definition):


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

And so on.  For more examples and details, check out test/plumbing/graph_examples_test.clj.


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
  
(= 4  (simple-nested-fnk {:a 1 :b {:b1 2 :c {:d 1}}}))   
(= 5  (simple-nested-fnk {:a 1 :b {:b1 1 :c {}}}))
```

You can use this binding style in a `let` statement using `letk` 
or within an anonymous function by using `fnk`. 


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

Ever wanted to conditionally do steps in a `->>` or `->`? Now you can with our
'penguin' operators. Here's a single-arrow example:

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

## License

Copyright (C) 2013 Prismatic.  Distributed under the Eclipse Public License, the same as Clojure.

This project also includes a modified version of de.kotka/lazymap, which is Copyright 2008-2011 (c) Meikel Brandmeyer and distributed under a MIT license.
