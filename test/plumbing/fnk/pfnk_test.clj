(ns plumbing.fnk.pfnk-test
  (:use clojure.test plumbing.core plumbing.fnk.pfnk))

(deftest comp-partial-test
  (let [in  (fnk [a b {c 2} :as m] m)
        out (comp-partial in (fnk [d a {q 2}] {:b d :e (inc a)}))]
     (is (= {:a 1 :b 5 :d 5 :e 2}
            (out {:a 1 :d 5})))
     (is (= {:a 1 :b 5 :c 4 :d 5 :e 2}
            (out {:a 1 :c 4 :d 5})))
     (is (= {:a true :d true :c false :q false}
            (input-schema out))))
   
  (is (= 10 ((comp-partial (fnk [x {y 2} z] (+ x y z)) (fnk [] {:x 7})) 
             {:z 1})))
  (is (= 12 ((comp-partial (fnk [x {y 2} z :as m & more]
                                (is (= [5 2 5] [x y z]))
                                (is (= {:x 5 :z 5 :efour 4 :enine 9 :q 44 :r 5} m))
                                (is (= {:efour 4 :enine 9 :q 44 :r 5 } more))
                                (+ x y z))
              (fnk [r enine] {:efour 4 :x r :z r :enine enine})) 
             {:r 5 :enine 9 :q 44}))))

