(ns plumbing.clojure-core-test
  "tests stolen from clojure.core sequences.clj tests"
  (:use plumbing.core clojure.test)
  (:require plumbing.clojure-core))


(deftest frequencies-test
  (are [expected test-seq] (= (frequencies test-seq) expected)
       {\p 2, \s 4, \i 4, \m 1} "mississippi"
       {1 4 2 2 3 1} [1 1 1 1 2 2 3]
       {1 4 2 2 3 1} '(1 1 1 1 2 2 3)))

(deftest test-distinct
  (are [x y] (= x y)
      (distinct ()) ()
      (distinct '(1)) '(1)
      (distinct '(1 2 3)) '(1 2 3)
      (distinct '(1 2 3 1 1 1)) '(1 2 3)
      (distinct '(1 1 1 2)) '(1 2)
      (distinct '(1 2 1 2)) '(1 2)

      (distinct []) ()
      (distinct [1]) '(1)
      (distinct [1 2 3]) '(1 2 3)
      (distinct [1 2 3 1 2 2 1 1]) '(1 2 3)
      (distinct [1 1 1 2]) '(1 2)
      (distinct [1 2 1 2]) '(1 2)

      (distinct "") ()
      (distinct "a") '(\a)
      (distinct "abc") '(\a \b \c)
      (distinct "abcabab") '(\a \b \c)
      (distinct "aaab") '(\a \b)
      (distinct "abab") '(\a \b) )

  (are [x] (= (distinct [x x]) [x])   
      nil
      false true
      0 42
      0.0 3.14
      2/3
      0M 1M
      \c
      "" "abc"
      'sym
      :kw
      () '(1 2)
      [] [1 2]
      {} {:a 1 :b 2}
      #{} #{1 2} ))