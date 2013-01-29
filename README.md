<img src="https://raw.github.com/wiki/prismatic/plumbing/images/prismatic-swiss-army-knife.png" alt="prismatic/plumbing logo" title="prismatic/plumbing logo" align="right" width="350" />

# Plumbing and Graph: the Clojure utility belt

This first release includes our '[Graph](http://blog.getprismatic.com/blog/2012/10/1/prismatics-graph-at-strange-loop.html)' library, our `plumbing.core` library of very commonly used functions (the only thing we `:use` across our codebase), and a few other supporting namespaces.  

Leiningen dependency (Clojars): [prismatic/plumbing "0.0.1"]  
** This is an alpha release.  We  are using it internally in production, but the API and organizational structure are subject to change.  Comments and suggestions are much appreciated. **

Check back often, because we'll keep adding more useful namespaces and functions as we work through cleaning up and open-sourcing our stack of Clojure libraries.  

## Graph: the Functional Swiss-Army Knife

Graph is a simple and *declarative* way to specify a structured computation, which is easy to analyze, change, compose, and monitor. Here's a simple example of an ordinary function definition, and its Graph equivalent:

```clojure
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
     :m2 m2 ; mean-square
     :v v   ; variance
     }))

(use 'plumbing.core)
(def stats-graph
  "A graph specifying the same computation as 'stats'"
  {:n  (fnk [xs]   (count xs))
   :m  (fnk [xs n] (/ (sum identity xs) n))
   :m2 (fnk [xs n] (/ (sum #(* % %) xs) n))
   :v  (fnk [m m2] (- m2 (* m m)))})   
```

A Graph is just a map from keywords to keyword functions ([learn more](#fnk)).  In this case, `stats-graph` represents the steps in taking a sequence of numbers (`xs`) and producing univariate statistics on those numbers (i.e., the mean `m` and the variance `v`).  The names of arguments to each `fnk` can refer to other steps that must happen before the step executes. For instance, in the above, to execute `:v`, you must first execute the `:m` and `:m2` steps (mean and mean-square respectively).

We can "compile" this Graph to produce a single function (equivalent to `stats`), which also checks that the map represents a valid Graph:

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

Unlike the opaque `stats` fn, however, we can modify and extend `stats-graph` using ordinary operations on maps:

```clojure
(def extended-stats  
  (graph/eager-compile 
    (assoc stats-graph
      :sd (fnk [^double v] (Math/sqrt v)))))
	
(= {:n 4
    :m 3
    :m2 (/ 25 2)
    :v (/ 7 2)
    :sd (Math/sqrt 3.5)}
   (extended-stats-graph {:xs [1 2 3 6]}))	
```

A Graph encodes the structure of a compuation, but not how it happens, allowing for many execution strategies. For example, we can do a lazy compilation of a Graph so step values are computed as needed. Or, we can parallel-compile the Graph so that independent step functions are run in separate threads:

```clojure
(def lazy-stats (graph/lazy-compile stats-graph))

(def output (lazy-stats {:xs [1 2 3 6]}))
;; Nothing has actually been computed yet
(= (/ 25 2) (:m2 output))
;; Now :n, :m, and :m2 have been computed, but :v is still behind a delay        


(def par-stats (graph/par-compile stats-graph))

(def output (par-stats {:xs [1 2 3 6]}))
;; Nodes are being computed in futures, with :m and :m2 going in parallel
(= (/ 7 2) (:v output)) 
```	

We can also ask a Graph for information about its inputs and outputs (automatically computed from its definition):

```clojure
(require '[plumbing.fnk.pfnk :as pfnk])

;; stats-graph takes a map with one required key, :xs
(= {:xs true}
   (pfnk/input-schema stats-graph))
  
;; stats-graph outputs a map with four keys, :n, :m, :m2, and :v
(= {:n true :m true :m2 true :v true}
   (pfnk/output-schema stats-graph))
```

We can also have higher-order functions on Graphs to wrap the behavior on each step. For instance, we can automatically profile each sub-function in 'stats' to see how long it takes to execute:

```clojure
(def profiled-stats (graph/eager-compile (graph/profiled ::profile-data stats-graph)))
  
;;; times in milliseconds for each step:
(= {:n 1.001, :m 0.728, :m2 0.996, :v 0.069}
   (::profile-data (profiled-stats {:xs (range 10000)})))
```

… and so on.  For more examples and details about Graph, check out the [graph examples test](https://github.com/Prismatic/plumbing/blob/master/test/plumbing/graph_examples_test.clj).  Also, stay tuned for a ClojureScript version of Graph, coming soon.

<a name="fnk"/>
## Bring on (de)fnk 

Many of the functions we write (in Graph and elsewhere) take a single map argument and we have expectations about which keys must be present and which can are optional. We developed a new style of binding ([read more here](https://github.com/Prismatic/plumbing/tree/master/src/plumbing/fnk)) to make this a lot easier and to check that input data has the right 'shape'. We call these 'keyword functions' (defined by `defnk`) and here's what one looks like:

```clojure
(use 'plumbing.core)
(defnk simple-fnk [a b c] 
  (+ a b c))
  
(= 6  (simple-fnk {:a 1 :b 2 :c 3}))
;; Below throws: Key :c not found in (:a :b)
(thrown? Throwable (simple-fnk {:a 1 :b 2})) 
```

You can declare a key as optional and provide a default:
```clojure
(defnk simple-opt-fnk [a b {c 1}] 
  (+ a b c))
  
(= 4  (simple-opt-fnk {:a 1 :b 2}))   
```

You can do nested map bindings:
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


## More good stuff

There's a whole lot of functions in `plumbing.core` which we can't live without. Here are a few of our favorites.

When we build maps, we often use `for-map`, which works like `for` but for maps:

```clojure
(use 'plumbing.core)
(= (for-map [i (range 4) j (range 4) 
	        :let [s (+ i j)]
			:when (< s 6)] s [i j])
	{0 [0 0], 1 [1 0], 2 [2 0], 3 [3 0], 4 [3 1], 5 [3 2]})
```

`safe-get` is like `get` but throws when the key doesn't exist:

```clojure
;; IllegalArgumentException Key :c not found in {:a 1, :b 2} 
(thrown? Exception (safe-get {:a 1 :b 2}} :c)
```

Another frequently used map function is `map-vals`:

```clojure
;; return k -> (f v) for [k, v] in map
(= (map-vals inc {:a 0 :b 0})
   {:a 1 :b 1})
```

Ever wanted to conditionally do steps in a `->>` or `->`? Now you can with our
'penguin' operators. Here's a few examples:

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

Check out [`plumbing.core`](https://github.com/Prismatic/plumbing/blob/master/src/plumbing/core.clj) for many other useful functions.


## License

Copyright (C) 2013 Prismatic.  Distributed under the Eclipse Public License, the same as Clojure.

This project also includes a modified version of de.kotka/lazymap, which is Copyright 2008-2011 (c) Meikel Brandmeyer and distributed under a MIT license.
