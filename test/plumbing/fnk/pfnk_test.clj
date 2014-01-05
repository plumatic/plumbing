(ns plumbing.fnk.pfnk-test
  (:use clojure.test plumbing.core plumbing.fnk.pfnk)
  (:require [schema.core :as s]))

(deftest meta-round-trip-test
  (let [i-schema {:x s/Any}
        o-schema {:y s/Any}
        schemata [i-schema o-schema]
        f (fn->fnk (fn [m] {:y (inc (safe-get m :x))}) schemata)]
    (is (= {:y 2} (f {:x 1})))
    (is (= schemata (io-schemata f)))
    (is (= i-schema (input-schema f)))
    (is (= o-schema (output-schema f)))))