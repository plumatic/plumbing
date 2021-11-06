(ns plumbing.test-runner
  (:require [doo.runner :refer-macros [doo-tests]]
            plumbing.core-test
            plumbing.fnk.fnk-examples-test
            plumbing.fnk.pfnk-test
            plumbing.fnk.schema-test
            plumbing.graph-async-test
            plumbing.graph-examples-test
            plumbing.graph-test
            plumbing.map-test))

(doo-tests
  'plumbing.core-test
  'plumbing.fnk.fnk-examples-test
  'plumbing.fnk.pfnk-test
  'plumbing.fnk.schema-test
  'plumbing.graph-async-test
  'plumbing.graph-examples-test
  'plumbing.graph-test
  'plumbing.map-test)
