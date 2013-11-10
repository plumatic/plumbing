(ns plumbing.fnk.schema-test
  (:use clojure.test plumbing.core plumbing.fnk.schema)
  (:require
   [schema.core :as s]
   [schema.test :as schema-test]
   [plumbing.fnk.pfnk :as pfnk]))

(deftest explicit-schema-key-map-test
  (is (= {:foo true :bar false}
         (explicit-schema-key-map
          {:foo s/Any (s/optional-key :bar) s/Any s/Keyword s/Keyword}))))

(deftest split-schema-keys-test
  (is (= [[:foo :bar] [:baz :bat]]
         (split-schema-keys
          (array-map :foo true :baz false :bar true :bat false)))))

(deftest merge-on-with-test
  (is (= {0 5 4 9 9 12}
         (#'plumbing.fnk.schema/merge-on-with #(quot % 2) min + {1 2 4 9 9 4} {9 8 0 3}))))

(deftest union-input-schemata-test
  (is (= {:a s/Any}
         (union-input-schemata {:a s/Any} {:a s/Any})))
  (is (= {:a String}
         (union-input-schemata {:a String} {(s/optional-key :a) String})))
  (is (= {:a String}
         (union-input-schemata {(s/optional-key :a) String} {:a s/Any})))
  (is (= {:a (s/both String Object)}
         (union-input-schemata {(s/optional-key :a) String} {:a Object})))
  (is (= {:a {(s/optional-key :a1) String
              :a2 Object
              :a3 String}
          (s/optional-key :b) Object}
         (union-input-schemata {:a {(s/optional-key :a1) String
                                    (s/optional-key :a2) Object}
                                (s/optional-key :b) Object}
                               {:a {:a2 Object :a3 String}}))))

(deftest required-toplevel-keys-test
  (is (= #{:a :b}
         (set (required-toplevel-keys {:a {:a1 String} :b Long (s/optional-key :c) Object})))))

(deftest guess-expr-output-schema-test
  (is (= `s/Any (@#'guess-expr-output-schema "foo")))
  (is (= {:a `s/Any :b `s/Any} (@#'guess-expr-output-schema {:a (+ 1 1) :b false})))
  (is (= `s/Any (@#'guess-expr-output-schema {'a (+ 1 1)}))))

(deftest compose-schemata-test
  (is (= [{:a s/Any :c s/Any :d s/Any}
          {:x s/Any}]
         (compose-schemata
          [{:a s/Any :b {:b1 s/Any} :c s/Any}
           {:x s/Any}]
          [{:c s/Any :d s/Any}
           {:b {:b1 s/Any}}])))

  (is (= [{:a s/Any (s/optional-key :e) s/Any :c s/Any :d s/Any}
          {:x s/Any}]
         (compose-schemata
          [{:a s/Any
            :b {:b1 s/Any}
            (s/optional-key :c) s/Any
            (s/optional-key :e) s/Any
            (s/optional-key :f) s/Any}
           {:x s/Any}]
          [{:c s/Any :d s/Any}
           {:b {:b1 s/Any} :c s/Any :f s/Any}])))

  (is (thrown? Exception
               (compose-schemata
                [{:a s/Any :b {:b1 s/Any} :c s/Any}
                 {:x s/Any}]
                [{:c s/Any :d s/Any}
                 {:b s/Any}]))))

(deftest sequence-schemata-test
  (is (= [{:a s/Any (s/optional-key :b) s/Any} {:c s/Any :o2 {:o21 s/Any}}]
         (sequence-schemata [{:a s/Any} {:c s/Any}] [:o2 [{(s/optional-key :b) s/Any :c s/Any} {:o21 s/Any}]])))
  (is (thrown? IllegalArgumentException
               (sequence-schemata [{:a s/Any} {:c s/Any}] [:o2 [{(s/optional-key :b) s/Any :c s/Any :o2 s/Any} {:o21 s/Any}]])))
  (is (thrown? IllegalArgumentException
               (sequence-schemata [{:a s/Any} {:c s/Any :o2 s/Any}] [:o2 [{(s/optional-key :b) s/Any :c s/Any} {:o21 s/Any}]])))
  (is (thrown? IllegalArgumentException
               (sequence-schemata [{:a s/Any :o2 s/Any} {:c s/Any}] [:o2 [{(s/optional-key :b) s/Any :c s/Any} {:o21 s/Any}]]))))


(deftest fnk-input-schemata-test
  (are [in fnk-form] (= in (pfnk/input-schema fnk-form))
       {:x s/Any :y s/Any s/Keyword s/Any}
       (fnk [x y])

       {:x s/Any (s/optional-key :y) s/Any :z s/Any s/Keyword s/Any}
       (fnk [x {y 2} z])

       {:x s/Any (s/optional-key :y) s/Any :z s/Any :q {:r s/Any s/Keyword s/Any} s/Keyword s/Any}
       (fnk [x {y 2} z [:q r] :as m & more])

       {(s/optional-key :x) s/Any :y {:alias s/Any s/Keyword s/Any} s/Keyword s/Any}
       (fnk [ {x 1} [:y alias]])

       {(s/optional-key :o1) s/Any
        :o2 s/Any
        :o3 {:x s/Any (s/optional-key :y) s/Any :z s/Any :q {:r s/Any s/Keyword s/Any} s/Keyword s/Any}
        s/Keyword s/Any}
       (fnk [{o1 1} o2 [:o3 x {y 2} z [:q r]]]))
  (is (= [1 2] ((eval `(fnk [[:x ~'x] [:y ~'y]] [~'x ~'y])) {:x {:x 1} :y {:y 2}})))
  (is (thrown? Throwable (eval `(fnk [{:y ~'x} {:y ~'y}] [~'x ~'y]))))
  (is (thrown? Throwable (eval `(fnk [{:x ~'x} {:y ~'x}] [~'x]))))
  (is (thrown? Throwable (eval `(fnk [[:x ~'x] ~'x] [~'x]))))
  (is (thrown? Throwable (eval `(fnk [{~'x 1} ~'x] [~'x])))))

(deftest fnk-out-schemata-test
  ;; Are somehow breaks the metadata on fnk forms.
  (is (= s/Any (pfnk/output-schema (fnk []))))
  (is (= s/Any (pfnk/output-schema (fnk [] (hash-map :x :y)))))
  (is (= {:o1 s/Any :o2 {:i s/Any :j {:q s/Any}}} (pfnk/output-schema (fnk [x] {:o1 x :o2 {:i x :j {:q 2}}}))))
  (is (= {:o1 s/Any :o2 s/Any} (pfnk/output-schema (fnk f :- {:o1 s/Any :o2 s/Any} [x]))))
  (is (= {:o1 s/Any :o2 s/Any} (pfnk/output-schema (fnk f :- {:o1 s/Any :o2 s/Any} [x]
                                                     {:o1 x :o2 {:i x :j {:q 2}}}))))
  (is (fn? (eval `(fnk f :- {:o1 s/Any} [] {:o1 2})))))

(use-fixtures :once schema-test/validate-schemas)