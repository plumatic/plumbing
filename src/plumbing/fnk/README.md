## Motivation

As part of our first open source release, we're contemplating introducing `fnk` and `defnk` macros with different destructuring syntax than the rest of Clojure.  

Below, we've collected some background on the rational behind introducing `fnk`, together with a proposed syntax and several alternatives.  Any and all input on these ideas would be much appreciated.

For more documentation and examples of graph and fnk, we encourage checking out `test/plumbing/fnk/fnk_examples_test.clj` and `test/plumbing/graph_examples_test.clj`.

### Background

We're very excited to begin sharing the Clojure infrastructure that powers Prismatic.  Our goals for 2013 include releasing open-source libraries for storage, machine learning, deployment, production services, and more for the Clojure community to (hopefully) use, contribute to, and build upon. 

Our first release is plumbing.[Graph], a library for declaratively specifying the composition structure of complex functions, along with other portions of our low-level "plumbing" library that support it. 

```clojure
{:n  (fnk [xs] (count xs))
 :m  (fnk [xs n] (/ (sum xs) n))
 :m2 (fnk [xs n] (/ (sum #(* % %) xs) n))
 :v  (fnk [m m2] (- m2 (* m m)))}
```

<img src="https://raw.github.com/wiki/plumatic/plumbing/images/graph_stats_graph.png" alt="A graphical depiction of this example graph" style="display:block; margin-left:auto; margin-right: auto; "align="center">
     
This example shows a simple Graph that expresses the computation of univariate statistics of a sequence of input numbers `xs` in four steps.  Dependencies between steps are expressed by argument and keyword names  (e.g., the variance `v` is computed from the mean `m` and mean-square `m2`).   The details of Graph are not vital for this discussion (see the [blog post](Graph) if you're interested), except for the following two high-level constraints on the implementation of `fnk`:

  1.  To express dependency structure simply (without repeating ourselves), we must be be able to interrogate a `fnk`  to ask for the *names* of its arguments.
  2.  The inputs, outputs, and intermediate values of a Graph are (nested) maps with keyword keys.  Thus, the arguments to a `fnk` are equivalent to keyword destructuring.

We cannot simply implement `(fnk [xs n] …)` with `(fn [{:keys [xs n]}])` for two reasons. First, arglist metadata is not supported by Clojure's current function-defining macros (`defn` puts it on the var, but neither `defn` nor `fn` puts it on the fn itself).  Second, while Clojure does offer excellent destructuring support (including for maps) out of the box, it turns out to be somewhat verbose for the cases commonly encountered in Graph.  

Thus, we are exploring the definition of a new family keyword functions (`fnk` and `defnk`) that use a new destructuring syntax focused around (nested) maps with keyword keys, and also provide explicit metadata about a function's input and output *schemata*. 

Our `fnk` experiment has been running internally for more than a year now, and we've found `fnk` to be quite useful for not only for defining Graphs, but also for many other situations involving maps with keyword keys.  Across our current codebase, about 5% of function definitions use a variant of `fnk` over `fn`.


## Fnk syntax

### (Why not) Clojure's destructuring syntax?

While Clojure's built-in destructuring is generally great, it leaves some things to be desired when we're only concerned with destructuring nested maps with keyword keys, and want to make heavy use of extra features like required keys or default values:

 * If we're only interested in top-level map inputs, we'd prefer to be able to say just `(fnk [a b c])` over `(fn [{:keys [a b c]}])` or `(fn [{a :a b :b c :c}])`. 
 * To require keys, I have to say (`fn [{:keys [a b c] :as m}] (assert (every? (partial contains? m) [:a :b :c]))) …)`.  This means I have to mention every argument twice.
 * Similarly, for default values, `(fn [{:keys [a] :or {a 2}}])` requires repeating argument names.
 * For nested map bindings, I must either repeat myself or mix :keys with direct map destructuring: `(fnk [{{b :b} :a}])` or `(fnk [{{:keys [b]} :a}])`
 
 This, while one option for Graph would be to just add arglist metadata to Clojure's `fn`, we have instead explored alternative syntax possibilities.

### Our fnk syntax proposal

Our primary design goal was to make keyword map destructuring, including nested and optional bindings, as straightforward and clear as possible.  Other forms of destructuring (i.e., for sequences) will not be supported.  We will introduce the syntax by example:

 * Functions take a single map argument, and bare symbols in the top-level binding represent required keys of this map: 

    ```clojure
    (defnk foo [x y] 
      (+ x y))

    (= (foo {:x 1 :y 2}) 3)

    (thrown? Exception (foo {:x 1})) ;; y is required
    ```

 * Optional keys with defaults are given as maps:

    ```clojure
    (defnk foo [x y {z 10}] 
      (+ x y z))

    (= (foo {:x 1 :y 2)) 13)

    (= (foo {:x 1 :y 2 :z 3)) 6)
    ```

 * Nested bindings are introduced with a vector (to match top-level bindings), but begin with the keyword to bind from:

    ```clojure
    (defnk foo [x [:sub c {d 10}]] (+ x c d))

    (= (foo {:x 1 :sub {:c 2}}) 13)
    ```
 
 * `:as` and `&` are allowed in terminal binding positions, with same meaning as ordinary Clojure destructuring:
 
    ```clojure
    (defnk foo [x & y :as z] [x y z])

    (= (foo {:x 10 :y 20}) [10 {:y 20} {:x 10 :y 20}])
    ````
   
  
Advantages:

 * Common case of flat required keys with no defaults is as simple as can be
 * Nested bindings are as minimal as possible
 * Notation is internally consistent: `[]` always indicates map binding, `{}` optional args
 * Key name repetition is eliminated for required keys and default values.

Known disadvantages: 

 * Different from existing Clojure destructuring
 * No sequence binding
 * Disparity between outer binding and nested bindings (which begin with keyword)
 * Renaming a key is a bit verbose -- `[:a :as b]`
  

### Alternatives

Let's take a simple example that includes most features of the above proposal, and compare with several alternative possibilities: 

```clojure
(defnk foo [x {y 1} [:z :as zalt] [:sub c]] ;; above proposal
   [x y zalt c]) 

(= (foo {:x 5 :z 10 :sub {:c 20}}) [5 1 10 20])
```

**Potential alternative 1:** exising Clojure syntax (see above).

```clojure
(defn foo [{x :x y :y zalt :z {c :c} :sub :or {y 1} :as m}]  ;; existing syntax
   (assert (and (contains? m :x) (contains? (:sub m) :c)))
   [x y zalt c])
```

  * Advantages: already exists, known to everyone, consistent
  * Disadvantages: verbose if you only care about map bindings, especially if you want required keys, default values, or nested bindings, all of which we use quite frequently.
  * Neutral: for Graph, we also have to modify `fn` (or create our own version) to record metadata about arglists and extract required or optional keys.

**Potential alternative 2:** an earlier version of fnk used `[]` for map bindings, and within a binding, `{}` to introduce sub-bindings and renamings, and `[]` for default values.  

```clojure
(defnk-2 foo [x [y 1] {zalt :z [c] :sub}] ;; alternative 2
   [x y zalt c]) 

(= (foo {:x 5 :z 10 :sub {:c 20}}) [5 1 10 20])
```
  
 * Advantage: Nested binding completely uniform with top-level (due to extra level of syntax)
 * Disadvantage: Each nested binding requires two levels of syntax
 * Disadvantage: [] used for two things (map binding and default values)
     
**Potential alternative 3:** like primary proposal, but use `#{}` literals for map binding (rather than `[]`) because map bindings are unordered (and to differentiate from existing syntax).  It's not clear how to best support nested binding keys, `:as:` and `&` in this unordered setting, but something like this might work:

```clojure
(defnk-3 foo #{x {y 1} #{:z ^:as zalt} #{:sub c}}  ;; alternative 3
   [x y zalt c])
```
  
 * Advantage: Set literal for binding conveys un-ordered nature of bindings
 * Advangage: Set literal also avoids any possibility for confusion with existing destructuring/`defn`
 * Disadvantage: `#{}` is not so pretty
 * Disadvantage: No obvious clean way to support `:as` and `&`, or enforce that the keyword comes first in nested binding, unless we change these to use metadata or add additional syntax.
 
## Addendum: underlying metadata representation for `fnk`

For the purposes of Graph, a `fnk` is just a fn of a single map argument, which also responds to protocol fn `(io-schemata f)` that returns a pair of an input schema and an output schema.  

An input schema is a nested map where keys are keywords and leaves are true or false, to indicate optional or required keys.  (Ultimately, it might be useful to put more sophisticated type information at the leaves).  Similarly, an output schema is a nested map where all the leaves are true (representing guaranteed elements of the return value).

For example, `(satisfies-schema? {:x true :y false :z true} {:x 2 :z 1})`.

Because Graph only depends on these protocol and schema definitions, you can use it without our `fnk` by definining schema metadata directly, or designing your own syntax.  Of course, we'd still like to get `fnk` right in our release, which is why we really need your input.  
 

[Graph]: http://plumatic.github.io/prismatics-graph-at-strange-loop
