(ns plumbing.fnk.pfnk-test
  #+cljs
  (:require-macros
   [cemerick.cljs.test :refer [is deftest testing]])
  (:require
   [schema.core :as s]
   [plumbing.core :as p :include-macros true]
   [plumbing.fnk.pfnk :as pfnk]
   #+clj [clojure.test :refer :all]
   #+cljs cemerick.cljs.test))

(deftest meta-round-trip-test
  (let [i-schema {:x s/Any}
        o-schema {:y s/Any}
        schemata [i-schema o-schema]
        f (pfnk/fn->fnk (fn [m] {:y (inc (p/safe-get m :x))}) schemata)]
    (is (= {:y 2} (f {:x 1})))
    (is (= schemata (pfnk/io-schemata f)))
    (is (= i-schema (pfnk/input-schema f)))
    (is (= o-schema (pfnk/output-schema f)))))
