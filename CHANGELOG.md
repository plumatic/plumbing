## 0.5.6
 * Adds support for `map-from-coll` that converts a collection to a map.

## 0.5.5
 * Bump schema dependency to avoid issues with Clojure 1.9 out of the box.

## 0.5.4
 * Allow redefining keys in an inner scope, and clarify the semantics.
 * Nicer error messages for `safe-get`, `safe-select-keys`, `merge-disjoint`.

## 0.5.3
 * **Deprecate** `keywordize-map` in favor of `clojure.walk/keywordize-keys`
 * Fix dependent optional bindings (e.g. (fnk [a {b a}])) broken in 0.5.1
 * Fnks remember their name, and named fnks can be used without a key in `graph/graph` forms (with an implicit key generated from `(keyword (name f))`).

## 0.5.2
 * Fix broken cycle check in Clojurescript topological sort.

## 0.5.1
 * (Experimental) include default values as metadata on fnk schemas.

## 0.5.0
 * **BREAKING**: Bump to Schema 1.0.1, breaking compatibility with pre-1.0.0 Schema.

## 0.4.4
 * Bump to latest Schema version, which should fix AOT compilation when used with Clojure 1.7-RC1 and later. 

## 0.4.3
 * Actually fix *update* warnings under Clojure 1.7 (commit missed the 0.4.2 release).

## 0.4.2
 * Letk now supports simple symbol bindings as well as map destructuring bindings.
 * Fix *update* warnings under Clojure 1.7.

## 0.4.1 
 * Fix concurrency issue recently introduced in distinct-by in Clojure (sequence had to be realized in creator thread due to transient restrictions)

## 0.4.0
 * **Breaking** Bump dependencies, potemkin no longer included transitively through schema.  

## 0.3.7
 * Add support for destructuring namespaced keywords, i.e.
   `(= 1 (letk [[a/b] {:a/b 1}] b))` and `(= 1 ((fnk [a/b] b) {:a/b 1}))`
 * Fix warnings about `*clojurescript-version*` when compiling ClojureScript

## 0.3.6
 * **BREAKING**: Define `update` only if `clojure.core/update` does not exist (ie. legacy clojure(script) versions)

## 0.3.5
 * Fix bug in `safe-get` in ClojureScript due to missing `:include-macros true` in plumbing.core

## 0.3.4
 * Add `plumbing.map/keyword-map`, `plumbing.core/if-letk`, `plumbing.core/when-letk`
 * Bump schema version to 0.3.1, fixing cljs warnings from that project, and move schema.macros calls over to schema.core.
 * Minimum required schema version is now 0.3.0

## 0.3.3
 * Properly generate cross-platform assertions, fixing ClojureScript errors that tried to throw Java errors.

## 0.3.2
 * Fix cljs compilation issue appearing in some circumstances (No such namespace: js)

## 0.3.1
 * Fix cljs issue where plumbing.fnk.schema was missing from dependency tree

## 0.3.0
 * **BREAKING**: `?>` and `?>>` require a body expression in parens, and take an arbitrary number of body expressions. 
 * Add ClojureScript support via cljx
 * Add plumbing.graph-async namespace to define asynchronous graphs using core.async channels. A core.async dependency has *not* been added to project.clj and must be supplied by user if this namespace is used.
 * Add `update` and `mapply` to plumbing.core

## 0.2.2
 * Don't depend on a specific Clojure version, and add support for Clojure 1.6.x

## 0.2.1
 * Fix for issues with AOT compilation after introducing schema

## 0.2.0
 * Replace fnk/graph's internal schema format with `prismatic/schema`.  This is a breaking change if (and only if) you've explicitly written old-style fnk/graph schemas like `{:x true :y false}`, or code for manipulating such schemas.
 * Drop support for Clojure 1.4.x

## 0.1.1
 * Fix bug when aliasing optional values with arg names, i.e. `(let [a 1] ((fnk [{a a}] a) {}))`
 * Implement well-defined semantics for optional values that reference other symbols bound within a (let/(de)fnk) form, matching Clojure: symbols are bound in the order given, so that an optional value can reference a symbol bound within the same destructuring form iff that symbol appears earlier in the form.
 * Add update-in-when, grouped-map, conk-when, cons-when, rsort-by, as->> to plumbing.core

## 0.1.0
 * Minor bugfixes and improved tests
 * Perf improvements for `map-keys` and `map-vals` (thanks [bendlas](https://github.com/bendlas)!)
 * Pulled out [lazymap](https://bitbucket.org/kotarak/lazymap) as a dependency.  `plumbing.lazymap` is no more -- it's now included indirectly as `lazymap.core`.  Thanks to Meikel Brandmeyer for a great library, and working with us to extend it to accommodate Graph's use case.
 * Lazily compiled graphs are now lazy about checking for required inputs, so a lazily compiled graph fn can be called without inputs not needed for computing the subset of outputs that will be extracted.
 * Explicit output-schema metadata on a fnk is taken as gold, rather than being merged with explicit data by analyzing the fnk body, and must be explicit rather than a spec.
 * Moved `comp-partial` from pfnk to graph, and added `instance` for fnks/graphs
 * Automatic efficient positional forms for fnks that take no rest args.
 * A new `eager-compile` that can produce graphs that are almost as fast as hand-coded replacements, by avoiding maps internally where possible using positional fns, and using Records when maps are necessary.  The old `eager-compile` is still available as `interpreted-eager-compile`.

## 0.0.1
 * Initial release
