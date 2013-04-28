(ns plumbing.graph-test
  (:use plumbing.core plumbing.graph clojure.test)
  (:require
   [plumbing.fnk.pfnk :as pfnk]))


(deftest graph-construction-test
  ;; io-schemata works correctly for flat graphs
  (is (= [{:x true :z true :q false :y false :r false}
          {:foo {:foox true :fooy true} :bar true}]
         (pfnk/io-schemata
          (graph :foo (fnk [x {y 1} {q 2}] {:foox x :fooy y})
                 :bar (fnk [foo z {q 4} {r 1}] [foo z])))))

  ;; io-schemata works correctly for nested graphs
  (is (= [{:x true :q false :y false}
          {:foo {:foox true :fooy true} :bar {:a true :baz {:foo true}}}]
         (pfnk/io-schemata
          (graph :foo (fnk [x {y 1} {q 2}] {:foox x :fooy y})
                 :bar {:a (fnk [foo] (inc foo))
                       :baz {:foo (fnk [x] x)}}))))

  ;; io-schemata works correctly for inline graphs
  (is (= [{:x true :q false :y false :b true}
          {:foo {:foox true :fooy true} :a true :baz {:foo true} :z true}]
         (pfnk/io-schemata
          (graph :foo (fnk [x {y 1} {q 2}] {:foox x :fooy y})
                 (graph
                  :a (fnk [foo] (inc foo))
                  :baz {:foo (fnk [x] x)})
                 :z (fnk [a b])))))

  (let [g {:foo (fnk [x {y 1} {q 2}] {:foox x :fooy y})
           :bar {:a (fnk [foo] (inc foo))
                 :baz {:foo (fnk [x] x)}}}]
    (is (= g (->graph g))))

  ;; Key order should be preserved by graph.
  (let [ks (map #(keyword (str %)) (range 1000))]
    (is (= ks
           (keys (apply graph (interleave ks (repeat (fnk [x] (inc x)))))))))

  (is (thrown? Exception (graph :foo (fnk [x]) :foo (fnk [y]))))     ;; duplicate keys are bad
  (is (thrown? Exception (graph :foo (fnk [x {y 1}]) :x (fnk [y])))) ;; cycles are bad
  (is (thrown? Exception (graph :foo (fnk [x {y 1}]) :y (fnk [y])))) ;; even self-cycles
  )

(deftest eager-compile-test
  (let [a (atom [])
        g (graph
           :x (fnk xfn [p1] (swap! a conj :x) (inc p1))
           :y (fnk yfn [x] (swap! a conj :y) (inc x)))
        c (eager-compile g)
        l (c {:p1 42})]
    (is (= [:x :y] @a))
    (is (= (:y l) 44))
    (is (= (:x l) 43)))
  (let [g2 (graph
            :x (fnk [] 1)
            :y {:z (fnk [a] 1)})]
    (is (= {:x 1 :y {:z 1}}
           (run g2 {:a 1}))))
  (is (= {:x {:y 6} :q 12}
         (run (graph
               :x {:y (fnk [a] (inc a))}
               :q (fnk [[:x y]] (* 2 y)))
              {:a 5})))

  (is (= {:foo 6 :bar {:a -6 :baz {:foo -5}}}
         (run (graph :foo (fnk [x] (inc x))
                     :bar {:a (fnk [foo] (- foo))
                           :baz {:foo (fnk [a] (inc a))}})
              {:x 5})))
  (is (thrown? Exception
               (run (graph
                     :x {:y (fnk [a] (inc a))}
                     :q (fnk [[:x z]] z))
                    {:a 5})))


  (is (= {:foo 6 :bar {:a -6 :baz {:foo 4}}}
         (run (graph :foo (fnk [x] (inc x))
                     :bar {:a (fnk [foo] (- foo))
                           :baz {:foo (fnk [x] (dec x))}})
              {:x 5})))

  (is (thrown? Exception
               (eager-compile (graph :foo {:bar (fnk [] 1)}
                                     :baz (fnk [[:foo baz]] (inc baz)))))))

(deftest lazy-compile-test
  (let [a (atom [])
        g (graph
           :x (fnk [p1] (swap! a conj :x) (inc p1))
           :y (fnk [x] (swap! a conj :y) (inc x))
           :z (fnk [x] (swap! a conj :z)))
	l ((lazy-compile g) {:p1 42})]
    (is (empty? @a))
    (is (= (:y l) 44))
    (is (= (:x l) 43))
    (is (= [:x :y] @a)))
  ;; lazy about error checking
  (is (= 5 (:z ((lazy-compile (graph :x (fnk [a]) :y (fnk [b] (inc b)) :z (fnk [y] (inc y)))) {:b 3}))))
  (is (thrown? Exception (:x ((lazy-compile (graph :x (fnk [a]) :y (fnk [b] (inc b)) :z (fnk [y] (inc y)))) {:b 3})))))

(defn chain-graph [n]
  (for-map [i (range n)]
    (keyword (str "x" (inc i)))
    (let [p (keyword (str "x" i))]
      (pfnk/fn->fnk (fn [m] (inc (p m))) [{p true} true]))))

(deftest chain-graph-test
  (is (= 100 (:x100 ((eager-compile (chain-graph 100)) {:x0 0}))))
  (is (= 100 (:x100 ((lazy-compile (chain-graph 100)) {:x0 0})))))


(deftest comp-partial-fn-test
  (let [in  (fnk [a b {c 2} :as m] m)]
    (let [out (comp-partial-fn in (fnk [d a {q 2}] {:b d :e (inc a)}))]
      (is (= {:a 1 :b 5 :d 5 :e 2}
             (out {:a 1 :d 5})))
      (is (= {:a 1 :b 5 :c 4 :d 5 :e 2}
             (out {:a 1 :c 4 :d 5})))
      (is (= {:a true :d true :c false :q false}
             (pfnk/input-schema out))))
    (let [out (comp-partial-fn in (fnk [d a {q 2}] {:b d :e (inc a) :c q}))]
      (is (= {:a 1 :b 5 :c 2 :d 5 :e 2}
             (out {:a 1 :d 5})))
      (is (= {:a 1 :b 5 :c 2 :d 5 :e 2}
             (out {:a 1 :c 4 :d 5})))
      (is (= {:a true :d true :q false}
             (pfnk/input-schema out)))))

  (let [in2 (fnk [[:a a1] b] (+ a1 b))]
    (let [out (comp-partial-fn in2 (fnk [x] {:a {:a1 x} :b (inc x)}))]
      (is (= 3 (out {:x 1})))
      (is (= {:x true} (pfnk/input-schema out))))
    (is (thrown? Exception (comp-partial-fn in2 (fnk [x] {:a x :b (inc x)})))))

  (is (= 10 ((comp-partial-fn (fnk [x {y 2} z] (+ x y z)) (fnk [] {:x 7}))
             {:z 1})))
  (is (= 12 ((comp-partial-fn (fnk [x {y 2} z :as m & more]
                                (is (= [5 2 5] [x y z]))
                                (is (= {:x 5 :z 5 :efour 4 :enine 9 :q 44 :r 5} m))
                                (is (= {:efour 4 :enine 9 :q 44 :r 5 } more))
                                (+ x y z))
              (fnk [r enine] {:efour 4 :x r :z r :enine enine}))
             {:r 5 :enine 9 :q 44}))))


(deftest instance-test
    ;; on a fnk, instance should just return a fnk.
  (is (= 21 ((instance (fnk [x] (inc x)) [y] {:x (* y 2)}) {:y 10})))

  (let [raw-g {:x (fnk [a] (* a 2))
               :y (fnk [x] (+ x 1))}
        inst-g (instance raw-g [z] {:a (+ z 5)})]
    (is (= {:z true} (pfnk/input-schema inst-g)))
    (is (= {:x true :y true} (select-keys (pfnk/output-schema inst-g) [:x :y])))

    (is (= {:x 16 :y 17} (select-keys (run inst-g {:z 3}) [:x :y])))

    (is (thrown? Exception (instance raw-g [z] {:q 22}))))

  (let [raw-g {:x (fnk [[:a a1]] (* a1 2))
               :y (fnk [x] (+ x 1))}]
    (let [inst-g (instance raw-g [z] {:a {:a1 (+ z 5)}})]
      (is (= {:z true} (pfnk/input-schema inst-g)))
      (is (= {:x true :y true} (select-keys (pfnk/output-schema inst-g) [:x :y])))
      (is (= {:x 16 :y 17} (select-keys (run inst-g {:z 3}) [:x :y]))))
    (is (thrown? Exception (instance raw-g [z] {:a z})))))


(deftest ^:slow profiled-test
  (let [approx-= (fn [x y] (< (Math/abs (- x y)) 5))
        times {:a 100 :b 200 :c 400}
        raw-g (graph
               :a (fnk [i] (Thread/sleep (times :a)) (inc i))
               :b (fnk [i] (Thread/sleep (times :b)) (- i))
               :c (fnk [a b] (Thread/sleep (times :c)) (* a b)))
        compiled (lazy-compile (profiled :profile-stats raw-g))
        execed (compiled {:i 10})]
    (is (= (select-keys execed [:a :b :c])
           {:a 11 :b -10 :c -110}))
    (doseq [[k t] times]
      (is (approx-= t (get @(:profile-stats execed) k))))))


(defn time-graphs "How slow are big chain graphs?" []
  (let [n 1000
        g (chain-graph n)
        tk (keyword (str "x" n))]
   (doseq [[k f]
           {:direct (fnk [x0] {tk (nth (iterate inc 1) n)})
            :eager  (time (eager-compile g))
            :lazy   (time (lazy-compile g))}]
     (println k)
     (dotimes [_ 5]
       (println (time (sum tk (repeatedly 10 #(f {:x0 1})))))))))
