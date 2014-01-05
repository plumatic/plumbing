(ns plumbing.fnk.fnk-examples-test
  "Explaining input and output schemata, fnk syntax, and their relationships
   by example."
  (:use clojure.test plumbing.core)
  (:require
   [schema.core :as s]
   [plumbing.fnk.schema :as schema]
   [plumbing.fnk.pfnk :as pfnk]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Input and output schemata

;; Input and output schemas describe the shape of nested maps with keyword keys
;; that are inputs and outputs of keyword functions, using the relevant
;; portions of the prismatic/schema library.

;; The structure of an input map is described using a nested map with keyword
;; keys, value schemas at the leaves, and (s/optional-key) for optional keys.

(def input-schema-1
  {(s/optional-key :a) s/Any
   :b s/Any
   :c {:c1 s/Any (s/optional-key :c2) s/Any}})

;; Fnk and graph understand only this subset of schema; additional constructs
;; are allowed, but fnk cannot 'see through' them to reason about their
;; semantics.

;; Output schemas are similar, except that the output schemas for Graphs
;; must consist of only required keys at the top level.

(def output-schema-1
  {:b s/Any
   :c {:c1 s/Any :c3 s/Any}})


(def output-schema-2
  {:b s/Any
   :c s/Any})

;; plumbing.fnk.schema has library functions for building, composing,
;; and checking schemata

(deftest assert-satisfies-schema-test
  (is (thrown? Exception (schema/assert-satisfies-schema input-schema-1 output-schema-2)))
  (is (do (schema/assert-satisfies-schema input-schema-1 output-schema-1) true)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fnk

;; For our purposes, a keyword function is an ordinary clojure fn? that
;; accepts a nested map with keyword keys as input, whose 'leaves' are
;; arbitrary values (including maps with non-keyword keys), and returns
;; an arbitrary value.

;; In addition, a keyword function must respond to the pfnk/io-schemata
;; call, returning a pair of an input schema and output schema.
;; (fnks also carry general function schemas via prismatic/schema, and
;;  the pfnk/io-schemata protocol is just a convencience method on top of this).

;; We can manually define a simple fnk by attaching io-schemata metadata
;; to a fn satisfying the above properties:

(def a-manual-keyword-function
  (pfnk/fn->fnk
   (fn [{:keys [a b o] :or {o 10} :as m}]
     (assert (every? #(contains? m %) [:a :b]))
     {:x (+ a b o)})
   [{:a s/Any :b s/Any (s/optional-key :o) s/Any s/Keyword s/Any}
    {:x s/Any}]))


(defn test-simple-keyword-function [f]
  (is (= {:x 13}
         (f {:a 1 :b 2})))

  ;; for convience, you can also extract a pair of input and output scheams
  (is (= [{:a s/Any :b s/Any (s/optional-key :o) s/Any s/Keyword s/Any}
          {:x s/Any}]
         (pfnk/io-schemata f)))

  ;; or the input-schema or output-schema individually.
  (is (= {:a s/Any :b s/Any (s/optional-key :o) s/Any s/Keyword s/Any}
         (pfnk/input-schema f)))
  (is (= {:x s/Any}
         (pfnk/output-schema f)))

  ;; a keyword function should throw if required keys not given.
  (is (thrown? Throwable (f {:a 3}))))

(deftest a-manual-keyword-function-test
  (testing "manual keyword fn"
    (test-simple-keyword-function a-manual-keyword-function)))


;; As a shortcut for defining keyword functions, we've defined macros
;; 'fnk' and 'defnk' with a different destructuring syntax than
;; 'fn' and 'defn', and which automatically infer input and output
;; schemata.  For more details and rationale for this syntax, see
;; plumbing.fnk/readme.md.

(defnk a-simple-fnk
  "This fnk has required keys :a and :b, and an optional key :o
   that defaults to 10 -- equivalent to a-manual-keyword-function."
  [a b {o 10}]
  {:x (+ a b o)})

;; This fnk automatically throws if required keys aren't present,
;; and infers its input schema from the binding form and output
;; schema from the literal map in its body.

(deftest a-simple-fnk-test
  (testing "fnk macro keyword fn"
    (test-simple-keyword-function a-simple-fnk)))


(defnk a-simple-fnk2
  "This fnk is like a-simple-fnk, but does not have a literal
   map body so nothing can be automatically inferred about its
   output schema"
  [a b {o 10}]
  (hash-map :x (+ a b o)))

(deftest a-simple-fnk2-test
  (is (= s/Any
         (pfnk/output-schema a-simple-fnk2))))

;; For these cases, we can provide explicit metadata to hint the
;; output schema of the fnk.

(defnk a-simple-fnk3 :- {:x s/Any}
  "This fnk is like a-simple-fnk2, but uses an explicit output
   schema hint, and is equivalent to a-simple-fnk"
  [a b {o 10}]
  (hash-map :x (+ a b o)))

(deftest a-simple-fnk3-test
  (testing "fnk with explicit output schema"
    (test-simple-keyword-function a-simple-fnk3)))

;; You can also provide schema information on the inputs, with
;; validation like schema.core/defn.  See (doc fnk) for details.

(defnk a-schematized-fnk :- (s/pred odd?)
  [a :- long b :- int]
  (+ a b))

(deftest a-schematized-fnk-test
  (is (= [{:a long :b int s/Keyword s/Any} (s/pred odd?)]
         (pfnk/io-schemata a-schematized-fnk)))
  (testing "No validation by default"
    (is (= 2 (a-schematized-fnk {:a 1 :b 1}))))
  (s/with-fn-validation
    (is (= 3 (a-schematized-fnk {:a 1 :b (int 2)})))
    (is (thrown? Exception (a-schematized-fnk {:a 1 :b 2})))
    (is (thrown? Exception (a-schematized-fnk {:a 1 :b (int 1)})))))


;; fnks also have support for nested bindings, and nested maps
;; for input and output schemata.
;; A nested map binding is introduced by an inner vector, whose
;; first element is a keyword specifying the key to bind under.

(defnk a-nested-fnk
  [a [:b b1 {b2 5}] c]
  {:sum (+ a b1 b2 c)
   :products {:as a
              :bs (* b1 b2)
              :cs c}})

(deftest a-nested-fnk-test
  (is (= {:sum 20
          :products {:as 1
                     :bs 60
                     :cs 2}}
         (a-nested-fnk {:a 1
                        :b {:b1 12}
                        :c 2})))

  (is (= {:a s/Any
          :b {:b1 s/Any (s/optional-key :b2) s/Any s/Keyword s/Any}
          :c s/Any
          s/Keyword s/Any}
         (pfnk/input-schema a-nested-fnk)))
  (is (= {:sum s/Any :products {:as s/Any :bs s/Any :cs s/Any}}
         (pfnk/output-schema a-nested-fnk)))

  (is (thrown? Throwable (a-nested-fnk {:a 1 :b {:b2 10} :c 3}))) ;; :b1 is missing
  )

;; finally, fnks have support for :as and & bindings like Clojure's
;; built-in destructuring.  :as binds a symbol to the entire map
;; input, and & binds to a map of any extra keys not destructured.

(defnk a-fancier-nested-fnk
  [a [:b b1 :as b] :as m & more]
  [a b1 b m more])

(deftest a-fancier-nested-fnk-test
  ;; :as and & are not reflected in input schema currently.
  (is (= {:a s/Any :b {:b1 s/Any s/Keyword s/Any} s/Keyword s/Any}
         (pfnk/input-schema a-fancier-nested-fnk)))
  (is (= s/Any
         (pfnk/output-schema a-fancier-nested-fnk)))

  (is (= [1 2 {:b1 2 :b2 3} {:a 1 :b {:b1 2 :b2 3} :c 4} {:c 4}]
         (a-fancier-nested-fnk {:a 1 :b {:b1 2 :b2 3} :c 4}))))
