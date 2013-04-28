## 0.0.2
 * Minor bugfixes and improved tests
 * Perf improvements for `map-keys` and `map-vals` (thanks [bendlas](https://github.com/bendlas)!)
 * Pulled out [lazymap](https://bitbucket.org/kotarak/lazymap) as a dependency.  `plumbing.lazymap` is no more -- it's now included indirectly as `lazymap.core`.  Thanks to Meikel Brandmeyer for a great library, and working with us to extend it to accommodate Graph's use case.
 * Moved `comp-partial` from pfnk to graph, and added `instance` for fnks/graphs
 * Automatic efficient positional forms for fnks that take no rest args.
 * A new `eager-compile` that can produce graphs that are almost as fast as hand-coded replacements, by avoiding maps internally where possible using positional fns, and using Records when maps are necessary.  The old `eager-compile` is still available as `interpreted-eager-compile`.

## 0.0.1
 * Initial release
