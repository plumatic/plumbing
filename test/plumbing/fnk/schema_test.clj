(ns plumbing.fnk.schema-test
  (:use clojure.test plumbing.fnk.schema plumbing.core)
  (:require [plumbing.fnk.schema :as schema]
            [plumbing.fnk.pfnk :as pfnk]))


(deftest union-input-schemata-test
  (is (= {:a true}
         (union-input-schemata {:a true} {:a false})))
  (is (= {:a {:a1 true}}
         (union-input-schemata {:a true} {:a {:a1 true}})))
  (is (= {:a {:a1 false :a2 true :a3 true} :b false}
         (union-input-schemata {:a {:a1 false :a2 false} :b false} {:a {:a2 true :a3 true}}))))

(deftest required-toplevel-keys-test
  (is (= #{:a :b}
         (set (required-toplevel-keys {:a {:a1 true} :b true :c false})))))

(deftest guess-expr-output-schema-test
  (is (= true (@#'guess-expr-output-schema "foo")))
  (is (= {:a true :b true} (@#'guess-expr-output-schema {:a (+ 1 1) :b false}))))

(deftest assert-satisfies-schema-test
  (doseq [[yes? x y]
          (partition 3 [true {:x true} {:x 2 :y 3}
                        true {:x true :z false} {:x 2 :y 3}
                        true {:x true :z true} {:x 2 :y 3 :z 1}
                        false {:x true :z true} {:x 2 :y 3}
                        true {:x {:y true :z false}} {:x {:y 1}}
                        false {:x {:y true :z false}} {:x 1}
                        false {:x {:y true :z false}} {:x {:z 1}}])]
    (if-not yes?
      (is (thrown? Exception (assert-satisfies-schema x y)))
      (is (do (assert-satisfies-schema x y) true)))))

(deftest compose-schemata-test
  (is (= [{:a true :c true :d true}
          {:x true}]
         (compose-schemata
          [{:a true :b {:b1 true} :c true}
           {:x true}]
          [{:c true :d true}
           {:b {:b1 true}}])))

  (is (= [{:a true :e false :c true :d true}
          {:x true}]
         (compose-schemata
          [{:a true :b {:b1 true} :c false :e false :f false}
           {:x true}]
          [{:c true :d true}
           {:b {:b1 true} :c true :f true}])))

  (is (thrown? Exception
               (compose-schemata
                [{:a true :b {:b1 true} :c true}
                 {:x true}]
                [{:c true :d true}
                 {:b true}]))))

(deftest sequence-schemata-test
  (is (= [{:a true :b false} {:c true :o2 {:o21 true}}]
         (sequence-schemata [{:a true} {:c true}] [:o2 [{:b false :c true} {:o21 true}]])))
  (is (thrown? IllegalArgumentException
               (sequence-schemata [{:a true} {:c true}] [:o2 [{:b false :c true :o2 true} {:o21 true}]])))
  (is (thrown? IllegalArgumentException
               (sequence-schemata [{:a true} {:c true :o2 true}] [:o2 [{:b false :c true} {:o21 true}]])))
  (is (thrown? IllegalArgumentException
               (sequence-schemata [{:a true :o2 true} {:c true}] [:o2 [{:b false :c true} {:o21 true}]]))))


(deftest fnk-input-schemata-test
  (are [in fnk-form] (= in (first (pfnk/io-schemata fnk-form)))
       {:x true :y true} (fnk [x y])
       {:x true :y false :z true} (fnk [x {y 2} z])
       {:x true :y false :z true :q {:r true}} (fnk [x {y 2} z [:q r] :as m & more])
       {:x false :y {:alias true}} (fnk [ {x 1} [:y alias]])
       {:o1 false :o2 true :o3 {:x true :y false :z true :q {:r true}}} (fnk [{o1 1} o2 [:o3 x {y 2} z [:q r]]]))
  (is (= [1 2] ((eval `(fnk [[:x ~'x] [:y ~'y]] [~'x ~'y])) {:x {:x 1} :y {:y 2}})))
  (is (thrown? Throwable (eval `(fnk [{:y ~'x} {:y ~'y}] [~'x ~'y]))))
  (is (thrown? Throwable (eval `(fnk [{:x ~'x} {:y ~'x}] [~'x]))))
  (is (thrown? Throwable (eval `(fnk [[:x ~'x] ~'x] [~'x]))))
  (is (thrown? Throwable (eval `(fnk [{~'x 1} ~'x] [~'x])))))

(deftest fnk-out-schemata-test
  ;; Are somehow breaks the metadata on fnk forms.
  (is (= true (second (pfnk/io-schemata (fnk [])))))
  (is (= true (second (pfnk/io-schemata (fnk [] (hash-map :x :y))))))
  (is (= {:o1 true :o2 {:i true :j {:q true}}} (second (pfnk/io-schemata (fnk  [x] {:o1 x :o2 {:i x :j {:q 2}}})))))
  (is (= {:o1 true :o2 true} (second (pfnk/io-schemata (fnk ^{:output-schema {:o1 true :o2 true}} [x])))))
  (is (= {:o1 true :o2 true} (second (pfnk/io-schemata (fnk ^{:output-schema {:o1 true :o2 true}} [x])))))
  (is (= {:o1 true :o2 true} (second (pfnk/io-schemata (fnk ^{:output-schema {:o1 true :o2 true}} [x])))))
  (is (= {:o1 true :o2 true} (second (pfnk/io-schemata (fnk ^{:output-schema {:o1 true :o2 true}} [x]
                                                         {:o1 x :o2 {:i x :j {:q 2}}})))))
  (is (fn? (eval `(fnk ^{:output-schema {:o1 true}} [] {:o1 2})))))
