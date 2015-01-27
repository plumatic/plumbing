## 0.3.7
 * Add support for destructuring namespaced keywords, i.e.
   `(= 1 (letk [[a/b] {:a/b 1}] b))` and `(= 1 ((fnk [a/b] b) {:a/b
   1}))`
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
