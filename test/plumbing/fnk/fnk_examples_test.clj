(ns plumbing.fnk.fnk-examples-test
  "Explaining input and output schemata, fnk syntax, and their relationships
   by example."
  (:use clojure.test plumbing.core)
  (:require 
   [plumbing.fnk.schema :as schema]
   [plumbing.fnk.pfnk :as pfnk]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Input and output schemata

;; Input and output schemata are about describing the shape of nested maps
;; with keyword keys that are inputs and outputs of keyword functions.

;; An input schema is a nested map with true and false at the leaves
;; True = required, false = optional

(def input-schema-1
  {:a false
   :b true
   :c {:c1 true :c2 false}})

;; An output schema is similar, but always has true at the leaves
;; (More keys might be returned, we don't need to say)

(def output-schema-1
  {:b true
   :c {:c1 true :c3 true}})


(def output-schema-2
  {:b true
   :c true})

;; plumbing.fnk.schema has library functions for building, composing,
;; and checking schemata

(deftest satisfies-schema-test 
  (is (false? (schema/satisfies-schema? input-schema-1 output-schema-2)))
  (is (schema/satisfies-schema? input-schema-1 output-schema-1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fnk

;; For our purposes, a keyword function is an ordinary clojure fn? that
;; accepts a nested map with keyword keys as input, whose 'leaves' are 
;; arbitrary values (including maps with non-keyword keys), and returns
;; an arbitrary value.  

;; In addition, a keyword function must respond to the pfnk/io-schemata
;; call, returning a pair of an input schema and output schema.

;; We can manually define a simple fnk by attaching io-schemata metadata
;; to a fn satisfying the above properties:

(def a-manual-keyword-function
  (pfnk/fn->fnk
   (fn [{:keys [a b o] :or {o 10} :as m}]
     (assert (every? #(contains? m %) [:a :b]))
     {:x (+ a b o)})
   [{:a true :b true :o false}
    {:x true}]))


(defn test-simple-keyword-function [f]
  (is (= {:x 13}
         (f {:a 1 :b 2})))

  ;; a keyword function knows its io-schemata
  (is (= [{:a true :b true :o false}
          {:x true}]
         (pfnk/io-schemata f)))
  
  ;; we can also ask for just the input-schema or output-schema
  (is (= {:a true :b true :o false}
         (pfnk/input-schema f)))
  (is (= {:x true}
         (pfnk/output-schema f)))
  
  ;; a keyword function should throw if required keys not given.
  (is (thrown? Throwable (f {:a 3}))))

(deftest a-manual-keyword-function-test
  (test-simple-keyword-function a-manual-keyword-function))


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
  (test-simple-keyword-function a-simple-fnk))


(defnk a-simple-fnk2
  "This fnk is like a-simple-fnk, but does not have a literal
   map body so nothing can be automatically inferred about its
   output schema"
  [a b {o 10}]
  (hash-map :x (+ a b o)))

(deftest a-simple-fnk2-test
  ;; true is the trivial output schema, which puts no constraints
  ;; on its output.
  (is (= true
         (pfnk/output-schema a-simple-fnk2))))

;; For these cases, we can provide explicit metadata to hint the 
;; output schema of the fnk.

(defnk a-simple-fnk3
  "This fnk is like a-simple-fnk2, but uses an explicit output
   schema hint, and is equivalent to a-simple-fnk"
  ^{:output-schema {:x true}} [a b {o 10}]
  (hash-map :x (+ a b o)))

(deftest a-simple-fnk3-test
  (test-simple-keyword-function a-simple-fnk3))


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
  
  (is (= {:a true :b {:b1 true :b2 false} :c true}
         (pfnk/input-schema a-nested-fnk)))
  (is (= {:sum true :products {:as true :bs true :cs true}}
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
  (is (= {:a true :b {:b1 true}}
         (pfnk/input-schema a-fancier-nested-fnk)))
  (is (= true
         (pfnk/output-schema a-fancier-nested-fnk)))
  
  (is (= [1 2 {:b1 2 :b2 3} {:a 1 :b {:b1 2 :b2 3} :c 4} {:c 4}]
         (a-fancier-nested-fnk {:a 1 :b {:b1 2 :b2 3} :c 4}))))

;; pfnk also defines an composition operation on keyword functions called
;; comp-partial, which is useful in graph and elsewhere.  
;; (comp-partial f1 f2) returns a new keyword function equivalent to 
;; #(f1 (merge % (f2 %))), with inferred input and output schemata.
;; comp-partial throws if outputs of f2 used by f1 do not satisfy
;; the input structure required by f1.

(deftest comp-partial-test
  (let [f1 (fnk [a [:b b1] c]
            {:x (+ a b1 c)})
        f2 (fnk [c d]
             {:b {:b1 (* c d)}})
        composed (pfnk/comp-partial f1 f2)]
    ;; the final function does not require :b, since it is provided to f1 by f2.
    (is (= [{:a true :c true :d true}
            {:x true}]
           (pfnk/io-schemata composed)))
    
    (is (= {:x 20}
           (composed {:a 2 :c 2 :d 8})))
    
    ;; This throws, because the value output by the second function under :b
    ;; cannot satisfy the input schema of f1 under :b.
    (is (thrown? Throwable (pfnk/comp-partial f1 (fnk [c d] {:b (* c d)}))))))






