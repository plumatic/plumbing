(ns plumbing.graph-test
  (:require
   [plumbing.core :as plumbing :include-macros true]
   [plumbing.graph :as graph :include-macros true]
   [clojure.walk :as walk]
   [schema.core :as s]
   [schema.test :as schema-test]
   [plumbing.fnk.pfnk :as pfnk]
   #+clj [plumbing.fnk.impl :as fnk-impl]
   #+clj [clojure.test :refer :all]
   #+cljs [cemerick.cljs.test :refer-macros [is deftest testing use-fixtures]]))

#+cljs
(do
  (def Exception js/Error)
  (def AssertionError js/Error)
  (def Throwable js/Error))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest graph-construction-test
  (testing "io-schemata works correctly for flat graphs"
    (is (= [{:x s/Any :z s/Any
             (s/optional-key :q) s/Any (s/optional-key :y) s/Int (s/optional-key :r) s/Any
             s/Keyword s/Any}
            {:foo {:foox s/Any :fooy s/Any} :bar s/Any}]
           (pfnk/io-schemata
            (graph/graph :foo (plumbing/fnk [x {y :- s/Int 1} {q 2}] {:foox x :fooy y})
                         :bar (plumbing/fnk [foo z {q 4} {r 1}] [foo z]))))))

  (testing "io-schemata works correctly for nested graphs"
    (is (= [{:x s/Any (s/optional-key :q) s/Any (s/optional-key :y) s/Any s/Keyword s/Any}
            {:foo {:foox s/Any :fooy s/Any} :bar {:a s/Int :baz {:foo s/Any}}}]
           (pfnk/io-schemata
            (graph/graph :foo (plumbing/fnk [x {y 1} {q 2}] {:foox x :fooy y})
                         :bar {:a (plumbing/fnk f :- s/Int [foo] (inc foo))
                               :baz {:foo (plumbing/fnk [x] x)}})))))


  (testing "io-schemata works correctly for inline graphs"
    (is (= [{:x s/Any (s/optional-key :q) s/Any (s/optional-key :y) s/Any :b s/Any s/Keyword s/Any}
            {:foo {:foox s/Any :fooy s/Any} :a s/Any :baz {:foo s/Any} :z s/Any}]
           (pfnk/io-schemata
            (graph/graph :foo (plumbing/fnk [x {y 1} {q 2}] {:foox x :fooy y})
                         (graph/graph
                          :a (plumbing/fnk [foo] (inc foo))
                          :baz {:foo (plumbing/fnk [x] x)})
                         :z (plumbing/fnk [a b]))))))

  (testing "named fnks work as expected"
    (let [f (plumbing/fnk foo [x {y 1}] (+ x y))
          g (graph/graph
             f
             (plumbing/fnk bar [foo] (* foo 2)))]
      (is (= [{:x s/Any (s/optional-key :y) s/Any s/Keyword s/Any}
              {:foo s/Any :bar s/Any}]
             (pfnk/io-schemata g)))
      (is (= (set (keys g)) #{:foo :bar}))
      (is (identical? f (:foo g)))
      (is (= {:foo 3 :bar 6} (graph/run g {:x 2}))))
    (testing "non-named fnks generate an error"
      (is (thrown? Exception (graph/graph (plumbing/fnk []))))))

  (let [g {:foo (plumbing/fnk [x {y 1} {q 2}] {:foox x :fooy y})
           :bar {:a (plumbing/fnk [foo] (inc foo))
                 :baz {:foo (plumbing/fnk [x] x)}}}]
    (is (= g (graph/->graph g))))

  (testing "Key order should be preserved by graph."
    (let [ks (map #(keyword (str %)) (range 100))]
      (is (= ks
             (keys (apply graph/graph (interleave ks (repeat (plumbing/fnk [x] (inc x))))))))))

  (testing "Exception on duplicate keys"
    (is (thrown? Exception (graph/graph :foo (plumbing/fnk [x]) :foo (plumbing/fnk [y])))))
  (testing "Exception on cycle"
    (is (thrown? Exception (graph/graph :foo (plumbing/fnk [x {y 1}]) :x (plumbing/fnk [y])))))
  (testing "Exception on self-cycle"
    (is (thrown? Exception (graph/graph :foo (plumbing/fnk [x {y 1}]) :y (plumbing/fnk [y]))))))

(defn test-eager-compile
  "Test eager compilation eager-compile-fn, where normalize-output-fn turns the outputs
   into ordinary clojure maps from records if necessary."
  [compile-fn normalize-output-fn]
  (let [a (atom [])
        g (graph/graph
           :x (plumbing/fnk xfn [p1] (swap! a conj :x) (inc p1))
           :y (plumbing/fnk yfn [x] (swap! a conj :y) (inc x)))
        c (compile-fn g)
        l (c {:p1 42})]
    (is (= [:x :y] @a))
    (is (= (:y l) 44))
    (is (= (:x l) 43)))
  (let [run-fn (fn [g m] (normalize-output-fn ((compile-fn g) m)))]
    (is (= {:x 1 :y {:z 1}}
           (run-fn (graph/graph
                    :x (plumbing/fnk [] 1)
                    :y {:z (plumbing/fnk [a] 1)})
                   {:a 1})))
    (is (= {:x {:y 6} :q 12}
           (run-fn (graph/graph
                    :x {:y (plumbing/fnk [a] (inc a))}
                    :q (plumbing/fnk [[:x y]] (* 2 y)))
                   {:a 5})))

    (is (= {:foo 6 :bar {:a -6 :baz {:foo -5}}}
           (run-fn (graph/graph :foo (plumbing/fnk [x] (inc x))
                                :bar {:a (plumbing/fnk [foo] (- foo))
                                      :baz {:foo (plumbing/fnk [a] (inc a))}})
                   {:x 5})))
    (is (thrown? Exception
                 (run-fn (graph/graph
                          :x {:y (plumbing/fnk [a] (inc a))}
                          :q (plumbing/fnk [[:x z]] z))
                         {:a 5})))

    (is (= {:foo 6 :bar {:a -6 :baz {:foo 4}}}
           (run-fn (graph/graph :foo (plumbing/fnk [x] (inc x))
                                :bar {:a (plumbing/fnk [foo] (- foo))
                                      :baz {:foo (plumbing/fnk [x] (dec x))}})
                   {:x 5})))

    (is (thrown? Exception
                 (compile-fn (graph/graph :foo {:bar (plumbing/fnk [] 1)}
                                          :baz (plumbing/fnk [[:foo baz]] (inc baz))))))

    ;; Test as many of the advanced Graph features as possible.
    (let [complex-graph
          {;; Ordinary fnks.
           :a (plumbing/fnk [x] (+ x 2))
           ;; Fnks that use &.
           :b (plumbing/fnk [a x & more] (+ a x (count more)))
           ;; Fnks that use :as.
           :c (plumbing/fnk [b x :as inputs] (+ b (count inputs) (* x -1)))
           ;; Nested graphs.
           :g {:ga (plumbing/fnk [x] (+ 5 x))
               ;; Fnks with hand-crafted schemas.
               :gm (pfnk/fn->fnk (fn [m] {:gmy (+ (:x m) (:ga m))
                                          :gmz (- 0 1 (:x m) (:ga m))})
                                 [{:ga s/Any :x s/Any}      ;; input schema
                                  {:gmy s/Any :gmz s/Any}]) ;; output schema
               ;; Fnks that depend on nested outputs.
               :gb (plumbing/fnk [[:gm gmy gmz]] (+ gmy gmz 10))
               ;; Fnks with properly un-shadowed variables.
               :gc (let [gm 2]
                     (plumbing/fnk [[:gm gmy] x] (+ gm gmy x)))}
           ;; Fnks that depend on deeply nested values.
           :d (plumbing/fnk [[:g [:gm gmy]] b] (+ gmy b))
           ;; Fnks that are compiled graphs.
           :cg (graph/interpreted-eager-compile {:cga (plumbing/fnk [x b] (* 3 x b))})
           ;; Fnks that we'll remove.
           :z (plumbing/fnk [x] (* x 10))}
          ;; Graphs modified at runtime
          complex-graph-modified (assoc (dissoc complex-graph :z)
                                   :e (plumbing/fnk [x [:cg cga]] (+ cga (rem x cga))))]
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
  (test-eager-compile graph/interpreted-eager-compile identity))

#+clj
(deftest eager-compile-test
  ;; eager-compile outputs records rather than ordinary maps as outputs.
  (test-eager-compile graph/eager-compile (partial walk/prewalk #(if (map? %) (into {} %) %)))
  (let [o ((graph/eager-compile (graph/graph :x (plumbing/fnk [y] (inc 1)))) {:y 1})]
    (is (= [:x] (keys o)))
    (is (= [2] (vals o)))
    (is (= 2 (o :x) (get o :x) (:x o)))
    (is (= {:x 2} (into {} o)))
    (is (not= {:x 2} o))))

#+clj
(do ;; test defschema with eager-compile -- there were some issues previously
  (ns test (:require [schema.core :as s]))
  (s/defschema Foo {s/Keyword s/Num})

  (ns plumbing.graph-test)
  (deftest eager-compile-defschema-test
    (let [g {:foo (plumbing/fnk [bar :- test/Foo])}
          f (graph/eager-compile g)]
      (is (= [{:bar test/Foo s/Keyword s/Any}
              {:foo s/Any}]
             (pfnk/io-schemata f)
             (pfnk/io-schemata g))))))

#+clj
(deftest positional-eager-compile-test
  (let [f (graph/positional-eager-compile
           (graph/graph
            :x (plumbing/fnk [a {b 1} {c 2}]
                 (+ a (* b 2) (* c 3))))
           [:b :a])]
    (is (= 19 (:x (f 5 3))))
    (is (= 11 (:x (f fnk-impl/+none+ 3))))
    (is (thrown? Exception (f 1)))
    (is (thrown? Exception (f 3 fnk-impl/+none+)))))

#+clj
(deftest lazy-compile-test
  (let [a (atom [])
        g (graph/graph
           :x (plumbing/fnk [p1] (swap! a conj :x) (inc p1))
           :y (plumbing/fnk [x] (swap! a conj :y) (inc x))
           :z (plumbing/fnk [x] (swap! a conj :z)))
        l ((graph/lazy-compile g) {:p1 42})]
    (is (empty? @a))
    (is (= (:y l) 44))
    (is (= (:x l) 43))
    (is (= [:x :y] @a)))
  (testing "lazy about error checking"
    (is (= 5 (:z ((graph/lazy-compile
                   (graph/graph :x (plumbing/fnk [a])
                                :y (plumbing/fnk [b] (inc b))
                                :z (plumbing/fnk [y] (inc y))))
                  {:b 3})))))
  (is (thrown? Exception (:x ((graph/lazy-compile
                               (graph/graph
                                :x (plumbing/fnk [a])
                                :y (plumbing/fnk [b] (inc b))
                                :z (plumbing/fnk [y] (inc y))))
                              {:b 3})))))

(deftest bind-non-map-with-as-test
  (is (= (:y (graph/run (graph/graph :x (plumbing/fnk [] {:a "1"})
                                     :y (plumbing/fnk [[:x [:a :as q]]] q))
                        {}))
         "1")))

#+clj
(defn chain-graph [n]
  (plumbing/for-map [i (range n)]
    (keyword (str "x" (inc i)))
    (let [p (keyword (str "x" i))]
      (pfnk/fn->fnk (fn [m] (inc (p m))) [{p s/Any} s/Any]))))

#+clj
(deftest chain-graph-test
  (is (= 100 (:x100 ((graph/eager-compile (chain-graph 100)) {:x0 0}))))
  (is (= 100 (:x100 ((graph/lazy-compile (chain-graph 100)) {:x0 0})))))


(deftest comp-partial-fn-test
  (let [in (plumbing/fnk [a b {c 2} :as m] m)]
    (let [out (graph/comp-partial-fn in (plumbing/fnk [d a {q 2}] {:b d :e (inc a)}))]
      (is (= {:a 1 :b 5 :d 5 :e 2}
             (out {:a 1 :d 5})))
      (is (= {:a 1 :b 5 :c 4 :d 5 :e 2}
             (out {:a 1 :c 4 :d 5})))
      (is (= {:a s/Any :d s/Any
              (s/optional-key :c) s/Any (s/optional-key :q) s/Any
              s/Keyword s/Any}
             (pfnk/input-schema out))))
    (let [out (graph/comp-partial-fn in (plumbing/fnk [d a {q 2}] {:b d :e (inc a) :c q}))]
      (is (= {:a 1 :b 5 :c 2 :d 5 :e 2}
             (out {:a 1 :d 5})))
      (is (= {:a 1 :b 5 :c 2 :d 5 :e 2}
             (out {:a 1 :c 4 :d 5})))
      (is (= {:a s/Any :d s/Any (s/optional-key :q) s/Any s/Keyword s/Any}
             (pfnk/input-schema out)))))

  (let [in2 (plumbing/fnk [[:a a1] b] (+ a1 b))]
    (let [out (graph/comp-partial-fn in2 (plumbing/fnk [x] {:a {:a1 x} :b (inc x)}))]
      (is (= 3 (out {:x 1})))
      (is (= {:x s/Any s/Keyword s/Any} (pfnk/input-schema out))))
    (is (thrown? Exception (graph/comp-partial-fn in2 (plumbing/fnk [x] {:a x :b (inc x)})))))

  (is (= 10 ((graph/comp-partial-fn (plumbing/fnk [x {y 2} z] (+ x y z)) (plumbing/fnk [] {:x 7}))
             {:z 1})))
  (is (= 12 ((graph/comp-partial-fn (plumbing/fnk [x {y 2} z :as m & more]
                                      (is (= [5 2 5] [x y z]))
                                      (is (= {:x 5 :z 5 :efour 4 :enine 9 :q 44 :r 5} m))
                                      (is (= {:efour 4 :enine 9 :q 44 :r 5 } more))
                                      (+ x y z))
                                    (plumbing/fnk [r enine] {:efour 4 :x r :z r :enine enine}))
             {:r 5 :enine 9 :q 44}))))


(deftest instance-test
  ;; on a fnk, instance should just return a fnk.
  (is (= 21 ((graph/instance (plumbing/fnk [x] (inc x)) [y] {:x (* y 2)}) {:y 10})))
  (is (= 23 ((graph/instance (plumbing/fnk [x {z 1}] (+ x z)) [y] {:z (* y 2)}) {:x 3 :y 10})))

  (let [raw-g {:x (plumbing/fnk [a] (* a 2))
               :y (plumbing/fnk [x] (+ x 1))}
        inst-g (graph/instance raw-g [z] {:a (+ z 5)})]
    (is (= {:z s/Any s/Keyword s/Any} (pfnk/input-schema inst-g)))
    (is (= {:x s/Any :y s/Any} (select-keys (pfnk/output-schema inst-g) [:x :y])))

    (is (= {:x 16 :y 17} (select-keys (graph/run inst-g {:z 3}) [:x :y])))

    (is (thrown? Exception (graph/instance raw-g [z] {:q 22}))))

  (let [raw-g {:x (plumbing/fnk [[:a a1]] (* a1 2))
               :y (plumbing/fnk [x {o 1}] (+ x o))}]
    (let [inst-g (graph/instance raw-g [z] {:a {:a1 (+ z 5)}})]
      (is (= {:z s/Any (s/optional-key :o) s/Any s/Keyword s/Any} (pfnk/input-schema inst-g)))
      (is (= {:x s/Any :y s/Any} (select-keys (pfnk/output-schema inst-g) [:x :y])))
      (is (= {:x 16 :y 17} (select-keys (graph/run inst-g {:z 3}) [:x :y]))))
    (testing "optional keys"
      (let [inst-o (graph/instance raw-g [z] {:a {:a1 (+ z 5)} :o 10})]
        (is (= {:z s/Any s/Keyword s/Any} (pfnk/input-schema inst-o)))
        (is (= {:x 16 :y 26} (select-keys (graph/run inst-o {:z 3}) [:x :y])))))
    (is (thrown? Exception (graph/instance raw-g [z] {:a z})))))

#+clj
(deftest ^:slow profiled-test
  (let [approx-= (fn [x y] (< (Math/abs (- x y)) 10))
        times {:a 100 :b 200 :c 400}
        raw-g (graph/graph
               :a (plumbing/fnk [i] (Thread/sleep (times :a)) (inc i))
               :b (plumbing/fnk [i] (Thread/sleep (times :b)) (- i))
               :c (plumbing/fnk [a b] (Thread/sleep (times :c)) (* a b)))
        compiled (graph/lazy-compile (graph/profiled :profile-stats raw-g))
        execed (compiled {:i 10})]
    (is (= (select-keys execed [:a :b :c])
           {:a 11 :b -10 :c -110}))
    (doseq [[k t] times]
      (is (approx-= t (get @(:profile-stats execed) k))))))

#+cljs
(deftest profiled-test
  (let [stats-graph {:n  (plumbing/fnk [xs]   (count xs))
                     :m  (plumbing/fnk [xs n] (/ (plumbing/sum identity xs) n))
                     :m2 (plumbing/fnk [xs n] (/ (plumbing/sum #(* % %) xs) n))
                     :v  (plumbing/fnk [m m2] (- m2 (* m m)))}
        compiled (graph/compile (graph/profiled ::profile-stats stats-graph))
        output (compiled {:xs (range 5000)})
        profile-stats @(::profile-stats output)]
    (is (map? profile-stats))
    (is (= #{:n :m :m2 :v}
           (set (keys profile-stats))))))

#+clj
(defn time-graphs "How slow are big chain graphs?" []
  (let [n 1000
        g (chain-graph n)
        tk (keyword (str "x" n))]
    (doseq [[k f]
            {:direct (plumbing/fnk [x0] {tk (nth (iterate inc 1) n)})
             :eager  (time (graph/eager-compile g))
             :lazy   (time (graph/lazy-compile g))}]
      (println k)
      (dotimes [_ 5]
        (println (time (plumbing/sum tk (repeatedly 10 #(f {:x0 1})))))))))

(use-fixtures :once schema-test/validate-schemas)
