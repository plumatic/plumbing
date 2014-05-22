## 0.3.0
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
