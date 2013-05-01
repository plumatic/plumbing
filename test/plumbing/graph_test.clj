(ns plumbing.graph-test
  (:use plumbing.core plumbing.graph clojure.test)
  (:require
   [clojure.walk :as walk]
   [plumbing.fnk.pfnk :as pfnk]
   [plumbing.fnk.impl :as fnk-impl]))


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

(defn test-eager-compile
  "Test eager compilation eager-compile-fn, where normalize-output-fn turns the outputs
   into ordinary clojure maps from records if necessary."
  [compile-fn normalize-output-fn]
  (let [a (atom [])
        g (graph
           :x (fnk xfn [p1] (swap! a conj :x) (inc p1))
           :y (fnk yfn [x] (swap! a conj :y) (inc x)))
        c (compile-fn g)
        l (c {:p1 42})]
    (is (= [:x :y] @a))
    (is (= (:y l) 44))
    (is (= (:x l) 43)))
  (let [run-fn (fn [g m] (normalize-output-fn ((compile-fn g) m)))]
    (is (= {:x 1 :y {:z 1}}
           (run-fn (graph
                    :x (fnk [] 1)
                    :y {:z (fnk [a] 1)})
                   {:a 1})))
    (is (= {:x {:y 6} :q 12}
           (run-fn (graph
                    :x {:y (fnk [a] (inc a))}
                    :q (fnk [[:x y]] (* 2 y)))
                   {:a 5})))

    (is (= {:foo 6 :bar {:a -6 :baz {:foo -5}}}
           (run-fn (graph :foo (fnk [x] (inc x))
                          :bar {:a (fnk [foo] (- foo))
                                :baz {:foo (fnk [a] (inc a))}})
                   {:x 5})))
    (is (thrown? Exception
                 (run-fn (graph
                          :x {:y (fnk [a] (inc a))}
                          :q (fnk [[:x z]] z))
                         {:a 5})))

    (is (= {:foo 6 :bar {:a -6 :baz {:foo 4}}}
           (run-fn (graph :foo (fnk [x] (inc x))
                          :bar {:a (fnk [foo] (- foo))
                                :baz {:foo (fnk [x] (dec x))}})
                   {:x 5})))

    (is (thrown? Exception
                 (compile-fn (graph :foo {:bar (fnk [] 1)}
                                    :baz (fnk [[:foo baz]] (inc baz))))))

    ;; Test as many of the advanced Graph features as possible.
    (let [complex-graph
          {;; Ordinary fnks.
           :a (fnk [x] (+ x 2))
           ;; Fnks that use &.
           :b (fnk [a x & more] (+ a x (count more)))
           ;; Fnks that use :as.
           :c (fnk [b x :as inputs] (+ b (count inputs) (* x -1)))
           ;; Nested graphs.
           :g {:ga (fnk [x] (+ 5 x))
               ;; Fnks with hand-crafted schemas.
               :gm (pfnk/fn->fnk (fn [m] {:gmy (+ (:x m) (:ga m))
                                          :gmz (- 0 1 (:x m) (:ga m))})
                                 [{:ga true :x true}      ;; input schema
                                  {:gmy true :gmz true}]) ;; output schema
               ;; Fnks that depend on nested outputs.
               :gb (fnk [[:gm gmy gmz]] (+ gmy gmz 10))
               ;; Fnks with properly un-shadowed variables.
               :gc (let [gm 2]
                     (fnk [[:gm gmy] x] (+ gm gmy x)))}
           ;; Fnks that depend on deeply nested values.
           :d (fnk [[:g [:gm gmy]] b] (+ gmy b))
           ;; Fnks that are compiled graphs.
           :cg (interpreted-eager-compile {:cga (fnk [x b] (* 3 x b))})
           ;; Fnks that we'll remove.
           :z (fnk [x] (* x 10))}
          ;; Graphs modified at runtime
          complex-graph-modified (assoc (dissoc complex-graph :z)
                                        :e (fnk [x [:cg cga]] (+ cga (rem x cga))))]
      (is (= (run-fn (compile-fn complex-graph-modified)
                     {:x 1
                      :ignored 2})
             {:a 3
              :b 4
              :c 5
              :g {:ga 6
                  :gm {:gmy 7
                       :gmz -8}
                  :gb 9
                  :gc 10}
              :d 11
              :cg {:cga 12}
              :e 13})))))


(deftest interpreted-eager-compile-test
  (test-eager-compile interpreted-eager-compile identity))

(deftest eager-compile-test
  ;; eager-compile outputs records rather than ordinary maps as outputs.
  (test-eager-compile eager-compile (partial walk/prewalk #(if (map? %) (into {} %) %)))
  (let [o ((eager-compile (graph :x (fnk [y] (inc 1)))) {:y 1})]
    (is (= [:x] (keys o)))
    (is (= [2] (vals o)))
    (is (= 2 (o :x) (get o :x) (:x o)))
    (is (= {:x 2} (into {} o)))
    (is (not= {:x 2} o))))

(deftest positional-eager-compile-test
  (let [f (positional-eager-compile (graph :x (fnk [a {b 1} {c 2}] (+ a (* b 2) (* c 3)))) [:b :a])]
    (is (= 19 (:x (f 5 3))))
    (is (= 11 (:x (f fnk-impl/+none+ 3))))
    (is (thrown? Exception (f 1)))
    (is (thrown? Exception (f 3 fnk-impl/+none+)))))

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
