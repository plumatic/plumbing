(ns plumbing.map-test
  (:refer-clojure :exclude [flatten])
  (:require
   [plumbing.core :as plumbing]
   [plumbing.map :as map]
   [clojure.string :as str]
   #+clj [clojure.test :refer :all]
   #+cljs [cemerick.cljs.test :refer-macros [is deftest testing use-fixtures]])
  #+cljs
  (:require-macros [plumbing.map :as map]))

#+cljs
(do
  (def Exception js/Error)
  (def AssertionError js/Error)
  (def Throwable js/Error))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Clojure immutable maps

(deftest safe-select-keys-test
  (is (= {:a 1 :c 3}
         (map/safe-select-keys {:a 1 :b 2 :c 3} [:a :c])))
  (is (= {}
         (map/safe-select-keys {:a 1 :b 2 :c 3} [])))
  (is (thrown? Throwable
               (map/safe-select-keys {:a 1 :b 2 :c 3} [:a :b :d]))))

(deftest merge-disjoint-test
  (is (= {:a 1 :b 2 :c 3}
         (map/merge-disjoint
          {} {:a 1 :b 2} {:c 3} {})))
  (is (thrown? Throwable
               (map/merge-disjoint
                {} {:a 1 :b 2} {:b 5 :c 3} {}))))

(deftest merge-with-key-test
  (is (=
       {"k1" "v1" :k1 :v2}
       (map/merge-with-key
        (fn [k v1 v2]
          (if (string? k)
            v1
            v2))
        {"k1" "v1"
         :k1 :v1}
        {"k1" "v2"
         :k1 :v2}))))

(deftest flatten-test
  (is (empty? (map/flatten nil)))
  (is (empty? (map/flatten {})))
  (is (= [[[] :foo]] (map/flatten :foo)))
  (is (= {[:a] 1
          [:b :c] 2
          [:b :d :e] 3
          [:b :d :f] 4}
         (into {} (map/flatten {:a 1 :b {:c 2 :d {:e 3 :f 4}}})))))

(deftest unflatten-test
  (is (= {} (map/unflatten nil)))
  (is (= :foo (map/unflatten [[[] :foo]])))
  (is (= {:a 1 :b {:c 2 :d {:e 3 :f 4}}}
         (map/unflatten
          {[:a] 1
           [:b :c] 2
           [:b :d :e] 3
           [:b :d :f] 4}))))

(deftest map-leaves-and-path-test
  (is (empty? (map/map-leaves-and-path (constantly 2) nil)))
  (is (= {:a {:b "a,b2"} :c {:d "c,d3"} :e "e11"}
         (map/map-leaves-and-path
          (fn [ks v] (str (str/join "," (map name ks)) (inc v)))
          {:a {:b 1} :c {:d 2} :e 10}))))

(deftest map-leaves-test
  (is (empty? (map/map-leaves (constantly 2) nil)))
  (is (= {:a {:b "1"} :c {:d "2"} :e "10"}
         (map/map-leaves str {:a {:b 1} :c {:d 2} :e 10})))
  (is (= {:a {:b nil} :c {:d nil} :e nil}
         (map/map-leaves (constantly nil) {:a {:b 1} :c {:d 2} :e 10}))))

(deftest keep-leaves-test
  (is (empty? (map/keep-leaves (constantly 2) {})))
  (is (= {:a {:b "1"} :c {:d "2"} :e "10"}
         (map/keep-leaves str {:a {:b 1} :c {:d 2} :e 10})))
  (is (= {:a {:b false} :c {:d false} :e false}
         (map/keep-leaves (constantly false) {:a {:b 1} :c {:d 2} :e 10})))
  (is (= {}
         (map/keep-leaves (constantly nil) {:a {:b 1} :c {:d 2} :e 10})))
  (is (= {:c {:d 10} :e 4}
         (map/keep-leaves #(when (even? %) %) {:a {:b 5} :c {:d 10 :e {:f 5}} :e 4}))))

(def some-var "hey hey")

(deftest keyword-map-test
  (is (= {} (map/keyword-map)) "works with no args")
  (is (= {:x 42} (let [x (* 2 3 7)] (map/keyword-map x))))
  (is (= {:some-var "hey hey"
          :$ \$}
         (let [$ \$]
           (map/keyword-map some-var $)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Java mutable Maps

#+clj
(do
  (deftest update-key!-test
    (let [m (java.util.HashMap. {:a 1 :b 2})]
      (map/update-key! m :a inc)
      (is (= {:a 2 :b 2} (into {} m)))
      (map/update-key! m :c conj "foo")
      (is (= {:a 2 :b 2 :c ["foo"]} (into {} m)))))

  (deftest get!-test
    (let [m (java.util.HashMap.)
          a! (fn [k v] (.add ^java.util.List (map/get! m k (java.util.ArrayList.)) v))
          value (fn [] (plumbing/map-vals seq m))]
      (is (= {} (value)))
      (a! :a 1)
      (is (= {:a [1]} (value)))
      (a! :a 2)
      (a! :b 3)
      (is (= {:a [1 2] :b [3]} (value)))))

  (defn clojureize [m] (plumbing/map-vals #(if (map? %) (into {} %) %) m))

  (deftest inc-key!-test
    (let [m (java.util.HashMap.)]
      (is (= {} (clojureize m)))
      (map/inc-key! m :a 1.0)
      (is (= {:a 1.0} (clojureize m)))
      (map/inc-key! m :a 2.0)
      (map/inc-key! m :b 4.0)
      (is (= {:a 3.0 :b 4.0} (clojureize m)))))

  (deftest inc-key-in!-test
    (let [m (java.util.HashMap.)]
      (is (= {} (clojureize m)))
      (map/inc-key-in! m [:a :b] 1.0)
      (is (= {:a {:b 1.0}} (clojureize m)))
      (map/inc-key-in! m [:a :b] 2.0)
      (map/inc-key-in! m [:a :c] -1.0)
      (map/inc-key-in! m [:b] 4.0)
      (is (= {:a {:b 3.0 :c -1.0} :b 4.0} (clojureize m)))))


  (deftest collate-test
    (is (= {:a 3.0 :b 2.0}
           (clojureize (map/collate [[:a 1] [:b 3.0] [:a 2] [:b -1.0]])))))

  (deftest deep-collate-test
    (is (= {:a {:b 3.0 :c -1.0} :b 4.0}
           (clojureize (map/deep-collate [[[:a :b] 1.0] [[:a :c] -1.0] [[:a :b] 2.0] [[:b] 4.0]]))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ops on graphs represented as maps.

(deftest topological-sort-test
  (is (= [:first :second :third :fourth :fifth]
         (map/topological-sort {:first [:second :fourth]
                                :second [:third]
                                :third [:fourth]
                                :fourth [:fifth]
                                :fifth []})))
  (is (= (range 100)
         (map/topological-sort (into {99 []} (for [i (range 99)] [i [(inc i)]])))))
  (is (= (range 99)
         (map/topological-sort (into {} (for [i (range 99)] [i [(inc i)]])))))
  (testing "include-leaves?"
    (is (= (range 1000)
           (map/topological-sort (into {} (for [i (range 999)] [i [(inc i)]])) true))))
  (testing "exception thrown if cycle"
    (is (thrown? Exception (map/topological-sort {:first [:second :fourth]
                                                  :second [:third]
                                                  :third [:fourth]
                                                  :fourth [:fifth]
                                                  :fifth [:first]})))))
