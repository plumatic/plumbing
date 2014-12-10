(ns plumbing.graph-async-test
  #+cljs
  (:require-macros
   [cljs.core.async.macros :refer [go]])
  (:require
   [plumbing.core :as plumbing :include-macros true]
   [plumbing.graph-async :as graph-async]
   #+clj [clojure.core.async :as async :refer [go <!]]
   #+cljs [cljs.core.async :as async :refer [<!]]
   #+clj [clojure.test :refer :all]
   #+cljs [cemerick.cljs.test :refer-macros [is deftest testing done]]))

(deftest async-compile-test
  (let [c ((graph-async/async-compile
            {:a {:a1 (plumbing/fnk [x] (inc x))
                 :a2 (plumbing/fnk [x] (go (<! (async/timeout 100)) (- x 10)))}
             :b (plumbing/fnk [[:a a1]] (* a1 2))
             :c (plumbing/fnk [[:a a2]] (* a2 2))
             :d (plumbing/fnk [{y 2}] (* y y))
             :e (plumbing/fnk [{d 9}] (+ d 90))})
           {:x 1 :y 7})
        expected-result {:a {:a1 2 :a2 -9} :b 4 :c -18 :d 49 :e 139}]
    #+clj (is (= expected-result
                 (async/<!! c)))
    #+cljs (go
            (is (= expected-result (<! c)))
            (done))))
