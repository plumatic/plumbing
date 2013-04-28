(ns plumbing.fnk.pfnk-test
  (:use clojure.test plumbing.core plumbing.fnk.pfnk))

(deftest meta-round-trip-test
  (let [i-schema {:x true}
        o-schema {:y true}
        schemata [i-schema o-schema]
        f (fn->fnk (fn [m] {:y (inc (safe-get m :x))}) schemata)]
    (is (= {:y 2} (f {:x 1})))
    (is (= schemata (io-schemata f)))
    (is (= i-schema (input-schema f)))
    (is (= o-schema (output-schema f)))))