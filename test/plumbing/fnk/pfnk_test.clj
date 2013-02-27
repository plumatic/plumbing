(ns plumbing.fnk.pfnk-test
  (:use clojure.test plumbing.core plumbing.fnk.pfnk))

(deftest comp-partial-test
  (let [in  (fnk [a b {c 2} :as m] m)]
    (let [out (comp-partial in (fnk [d a {q 2}] {:b d :e (inc a)}))]
      (is (= {:a 1 :b 5 :d 5 :e 2}
             (out {:a 1 :d 5})))
      (is (= {:a 1 :b 5 :c 4 :d 5 :e 2}
             (out {:a 1 :c 4 :d 5})))
      (is (= {:a true :d true :c false :q false}
             (input-schema out))))    
    (let [out (comp-partial in (fnk [d a {q 2}] {:b d :e (inc a) :c q}))]
      (is (= {:a 1 :b 5 :c 2 :d 5 :e 2}
             (out {:a 1 :d 5})))
      (is (= {:a 1 :b 5 :c 2 :d 5 :e 2}
             (out {:a 1 :c 4 :d 5})))
      (is (= {:a true :d true :q false}
             (input-schema out)))))
  
  (let [in2 (fnk [[:a a1] b] (+ a1 b))]
    (let [out (comp-partial in2 (fnk [x] {:a {:a1 x} :b (inc x)}))]
      (is (= 3 (out {:x 1})))
      (is (= {:x true} (input-schema out))))
    (is (thrown? Exception (comp-partial in2 (fnk [x] {:a x :b (inc x)})))))
   
  (is (= 10 ((comp-partial (fnk [x {y 2} z] (+ x y z)) (fnk [] {:x 7})) 
             {:z 1})))
  (is (= 12 ((comp-partial (fnk [x {y 2} z :as m & more]
                                (is (= [5 2 5] [x y z]))
                                (is (= {:x 5 :z 5 :efour 4 :enine 9 :q 44 :r 5} m))
                                (is (= {:efour 4 :enine 9 :q 44 :r 5 } more))
                                (+ x y z))
              (fnk [r enine] {:efour 4 :x r :z r :enine enine})) 
             {:r 5 :enine 9 :q 44}))))

