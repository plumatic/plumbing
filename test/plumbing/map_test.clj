(ns plumbing.map-test
  (:refer-clojure :exclude [flatten])
  (:use plumbing.map clojure.test plumbing.core)
  (:require [clojure.string :as str]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Clojure immutable maps

(deftest safe-select-keys-test
  (is (= {:a 1 :c 3}
         (safe-select-keys {:a 1 :b 2 :c 3} [:a :c])))
  (is (= {}
         (safe-select-keys {:a 1 :b 2 :c 3} [])))
  (is (thrown? Throwable
               (safe-select-keys {:a 1 :b 2 :c 3} [:a :b :d]))))

(deftest merge-disjoint-test
  (is (= {:a 1 :b 2 :c 3}
         (merge-disjoint
          {} {:a 1 :b 2} {:c 3} {})))
  (is (thrown? Throwable
               (merge-disjoint
                {} {:a 1 :b 2} {:b 5 :c 3} {}))))

(deftest merge-with-key-test
  (is (=
       {"k1" "v1" :k1 :v2}
       (merge-with-key
        (fn [k v1 v2]
          (if (string? k)
            v1
            v2))
        {"k1" "v1"
         :k1 :v1}
        {"k1" "v2"
         :k1 :v2}))))

(deftest flatten-test
  (is (empty? (flatten nil)))
  (is (empty? (flatten {})))
  (is (= [[[] :foo]] (flatten :foo)))
  (is (= {[:a] 1
          [:b :c] 2
          [:b :d :e] 3
          [:b :d :f] 4}
         (into {} (flatten {:a 1 :b {:c 2 :d {:e 3 :f 4}}})))))

(deftest unflatten-test
  (is (= {} (unflatten nil)))
  (is (= :foo (unflatten [[[] :foo]])))
  (is (= {:a 1 :b {:c 2 :d {:e 3 :f 4}}}
         (unflatten
          {[:a] 1
           [:b :c] 2
           [:b :d :e] 3
           [:b :d :f] 4}))))

(deftest map-leaves-and-path-test
  (is (empty? (map-leaves-and-path (constantly 2) nil)))
  (is (= {:a {:b "a,b2"} :c {:d "c,d3"} :e "e11"}
         (map-leaves-and-path
          (fn [ks v] (str (str/join "," (map name ks)) (inc v)))
          {:a {:b 1} :c {:d 2} :e 10}))))

(deftest map-leaves-test
  (is (empty? (map-leaves (constantly 2) nil)))
  (is (= {:a {:b "1"} :c {:d "2"} :e "10"}
         (map-leaves str {:a {:b 1} :c {:d 2} :e 10})))
  (is (= {:a {:b nil} :c {:d nil} :e nil}
         (map-leaves (constantly nil) {:a {:b 1} :c {:d 2} :e 10}))))

(deftest keep-leaves-test
  (is (empty? (keep-leaves (constantly 2) {})))
  (is (= {:a {:b "1"} :c {:d "2"} :e "10"}
         (keep-leaves str {:a {:b 1} :c {:d 2} :e 10})))
  (is (= {:a {:b false} :c {:d false} :e false}
         (keep-leaves (constantly false) {:a {:b 1} :c {:d 2} :e 10})))
  (is (= {}
         (keep-leaves (constantly nil) {:a {:b 1} :c {:d 2} :e 10})))
  (is (= {:c {:d 10} :e 4}
         (keep-leaves #(when (even? %) %) {:a {:b 5} :c {:d 10 :e {:f 5}} :e 4}))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Java mutable Maps

(deftest update-key!-test
  (let [m (java.util.HashMap. {:a 1 :b 2})]
    (is 2 (= (update-key! m :a inc)))
    (is (= {:a 2 :b 2} (into {} m)))
    (is 2 (= (update-key! m :c conj "foo")))
    (is (= {:a 2 :b 2 :c ["foo"]} (into {} m)))))

(deftest get!-test
  (let [m (java.util.HashMap.)
        a! (fn [k v] (.add ^java.util.List (get! m k (java.util.ArrayList.)) v))
        value (fn [] (map-vals seq m))]
    (is (= {} (value)))
    (a! :a 1)
    (is (= {:a [1]} (value)))
    (a! :a 2)
    (a! :b 3)
    (is (= {:a [1 2] :b [3]} (value)))))

(defn clojureize [m] (map-vals #(if (map? %) (into {} %) %) m))

(deftest inc-key!-test
  (let [m (java.util.HashMap.)]
    (is (= {} (clojureize m)))
    (inc-key! m :a 1.0)
    (is (= {:a 1.0} (clojureize m)))
    (inc-key! m :a 2.0)
    (inc-key! m :b 4.0)
    (is (= {:a 3.0 :b 4.0} (clojureize m)))))

(deftest inc-key-in!-test
  (let [m (java.util.HashMap.)]
    (is (= {} (clojureize m)))
    (inc-key-in! m [:a :b] 1.0)
    (is (= {:a {:b 1.0}} (clojureize m)))
    (inc-key-in! m [:a :b] 2.0)
    (inc-key-in! m [:a :c] -1.0)
    (inc-key-in! m [:b] 4.0)
    (is (= {:a {:b 3.0 :c -1.0} :b 4.0} (clojureize m)))))


(deftest collate-test
  (is (= {:a 3.0 :b 2.0}
         (clojureize (collate [[:a 1] [:b 3.0] [:a 2] [:b -1.0]])))))

(deftest deep-collate-test
  (is (= {:a {:b 3.0 :c -1.0} :b 4.0}
         (clojureize (deep-collate [[[:a :b] 1.0] [[:a :c] -1.0] [[:a :b] 2.0] [[:b] 4.0]])))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ops on graphs represented as maps.

(deftest topological-sort-test
  (is (= [:first :second :third :fourth :fifth]
         (topological-sort {:first [:second :fourth] :second [:third] :third [:fourth] :fourth [:fifth] :fifth []})))
  (is (= (range 1000)
         (topological-sort (into {999 []} (for [i (range 999)] [i [(inc i)]])))))
  (is (= (range 999)
         (topological-sort (into {} (for [i (range 999)] [i [(inc i)]])))))
  (is (= (range 1000)
         (topological-sort (into {} (for [i (range 999)] [i [(inc i)]])) true)))
  (is (thrown? Exception (topological-sort {:first [:second :fourth] :second [:third] :third [:fourth] :fourth [:fifth] :fifth [:first]})))
  )
