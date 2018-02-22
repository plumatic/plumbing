(ns plumbing.core-test
  (:require
   [schema.core :as s]
   [schema.test :as schema-test]
   [plumbing.core :as p :include-macros true]
   [plumbing.fnk.pfnk :as pfnk]
   #+clj [plumbing.fnk.impl :as fnk-impl]
   #+clj [clojure.test :refer :all]
   #+cljs [cemerick.cljs.test :refer-macros [is are deftest testing use-fixtures]]))

#+cljs
(do
  (def Exception js/Error)
  (def AssertionError js/Error)
  (def Throwable js/Error))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Maps

(deftest for-map-test
  (is (= (p/for-map [i [1 2]
                     j [10 20]]
           (+ i j)
           j)
         {11 10 12 10
          21 20 22 20}))
  (is (= (p/for-map [i [1 2]
                     j [10 20]]
           i
           j)
         {1 20 2 20}))
  (let [m (p/for-map m [i (range 1000)] i (+ i (get m (dec i) 0)))]
    (is (= (count m) 1000))
    (is (= (m 999) 499500))))

(p/-unless-update
 (deftest update-test
   (testing "0 extra args"
     (is (= {:a 5, :b 0}
            (p/update {:a 4, :b 0} :a inc)))
     (is (= {:a 1}
            (p/update {} :a (fnil inc 0)))))
   (testing "1 extra arg"
     (is (= {:a 42, :b 0}
            (p/update {:a 6, :b 0} :a * 7)))
     (is (= {:a #{42}}
            (p/update {} :a (fnil conj #{}) 42))))
   (testing "100 extra args"
     (is (= {:a 4951} (apply p/update {:a 1} :a + (range 100)))))))

(deftest map-vals-test
  (is (= (p/map-vals inc {:a 0 :b 0})
         {:a 1 :b 1}))
  (is (= (p/map-vals inc [[:a 0] [:b 0]])
         {:a 1 :b 1}))
  (is (= (p/map-vals inc (sorted-map :a 0 :b 0))
         {:a 1 :b 1}))
  (is (sorted? (p/map-vals inc (sorted-map :a 0 :b 0)))))

(deftest map-keys-test
  (is (= (p/map-keys str {:a 1 :b 1})
         {":a" 1 ":b" 1}))
  (is (= (p/map-keys str [[:a 1] [:b 1]])
         {":a" 1 ":b" 1})))

(deftest map-from-keys-test
  (is (= (p/map-from-keys inc [0 1 2])
         {0 1, 1 2, 2 3})))

(deftest map-from-vals-test
  (is (= (p/map-from-vals inc [0 1 2])
         {1 0, 2 1, 3 2})))

(deftest map-from-coll-test
  (is (= (p/map-from-coll inc identity [0 1 2])
         {1 0, 2 1, 3 2}))
  (is (= (p/map-from-coll identity inc [0 1 2])
         {0 1, 1 2, 2 3}))
  (is (= (-> (p/map-from-coll #(mod % 3) inc [0 1 2 3 4]) keys set)
         #{0 1 2})))

(deftest dissoc-in-test
  (is (= {:a 1} (p/dissoc-in {:a 1 :b 2} [:b])))
  (is (= {:a 1 :b {:d 3}} (p/dissoc-in {:a 1 :b {:c 2 :d 3}} [:b :c])))
  (is (= {:a 1} (p/dissoc-in {:a 1 :b {:c 2}} [:b :c])))
  (is (= {:a 1} (p/dissoc-in {:a 1} [:b :c])))
  (is (thrown? Exception (p/dissoc-in {:a 1 :b :not-a-map} [:b :c])))
  (is (= nil (p/dissoc-in {:a 1} [:a])))
  (is (= nil (p/dissoc-in nil [:a])))
  (is (= nil (p/dissoc-in {} [:a]))))

(deftest keywordize-map-test
  (is (= {:foo 1
          :bar [2]
          :baz [{:x 42}]}
         (p/keywordize-map {"foo" 1
                            "bar" [2]
                            :baz [{"x" 42}]})))

  (is (= {:foo 1
          :bar [2]
          :baz {:x 42}}
         (p/keywordize-map {"foo" 1
                            "bar" [2]
                            :baz {"x" 42}}))))

(deftest lazy-get-test
  (let [counter (atom 0)]
    (is (= 1 (p/lazy-get {:a 1} :a (do (swap! counter inc) 2))))
    (is (zero? @counter))
    (is (= 2 (p/lazy-get {:a 1} :b (do (swap! counter inc) 2))))
    (is (= 1 @counter))
    (is (= 2 (p/lazy-get {:a 1 :b 2} :b (do (swap! counter inc) 2))))
    (is (= 1 @counter))))


(deftest safe-get-test
  (is (= 2 (p/safe-get {:a 2} :a)))
  (is (thrown? Exception (p/safe-get {:a 2} :b)))
  (is (= 2 (p/safe-get-in {:a {:b 2}} [:a :b])))
  (is (thrown? Exception (p/safe-get-in {:a {:b 2}} [:b])))
  (is (thrown? Exception (p/safe-get-in {:a {:b 2}} [:a :c])))
  (is (thrown? Exception (p/safe-get-in {:a {:b 2}} [:a :b :d]))))

(deftest assoc-when-test
  (is (= {:a 1} (p/assoc-when nil :a 1)))
  (is (= {:a 1 :c 2} (p/assoc-when {:a 1} :b nil :c 2))))

(deftest update-in-when-test
  (is (= nil (p/update-in-when nil [:a] inc)))
  (is (= {:a {:b 2}} (p/update-in-when {:a {:b 2}} [:a :c] inc)))
  (is (= {} (p/update-in-when {} [:foo :bar] inc)))
  (is (= {:foo 2 :bar 1} (p/update-in-when {:foo 1 :bar 1} [:foo] inc)))
  (is (= {:a {:b 3 :z 5}} (p/update-in-when {:a {:b 2 :z 5}} [:a :b] inc))))

(deftest grouped-map-test
  (is (= {:a [1 2] :b [3]} (p/grouped-map first second [[:a 1] [:b 3] [:a 2]]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Seqs

(deftest aconcat-test
  (is (= [1 2 3 4 5 6] (p/aconcat [[1 2 3] [4 5 6]]))))

(deftest unchunk-test
  (let [realized (atom #{})
        xs (map (fn [x]
                  (swap! realized conj x)
                  x)
                (p/unchunk (range 10)))]
    (is (empty? @realized))
    (doseq [x (range 10)]
      (is (not (@realized x)))
      (is (= x (nth xs x)))
      (is (@realized x)))))

(deftest sum-test
  (is (= 55 (p/sum (range 1 11))))
  (is (= 55 (p/sum inc (range 10)))))

(deftest singleton-test
  (is (= 1 (p/singleton [1])))
  (is (nil? (p/singleton [1 2]))))

(deftest indexed-test
  (is (empty? (p/indexed nil)))
  (is (= [[0 :a] [1 :b] [2 :c]] (p/indexed [:a :b :c])))
  (is (= [[0 :a] [1 :b] [2 :c] [3 0]] (take 4 (p/indexed (concat [:a :b :c] (range)))))))

(deftest positions-test
  (is (empty? (p/positions odd? [2 4 6 8 10])))
  (is (= [0 1 2] (p/positions odd? [1 3 5 2 4 6])))
  (is (= [1 3 5] (take 3 (p/positions odd? (range))))))

#+clj
(deftest frequencies-fast-test
  (is (= {\p 2, \s 4, \i 4, \m 1}
         (p/frequencies-fast "mississippi")))
  (is (= {1 3 2 2 3 1}
         (p/frequencies-fast [1 2 3 1 2 1])))
  ;; We don't return the right thing on = but not .equals things,
  ;; because of the difference between Java Maps and Clojure maps.
  (is (= {1 1}
         (p/frequencies-fast [1 (BigInteger. "1")]))))
#+clj
(deftest distinct-fast-test
  (is (= [1 2 3]
         (p/distinct-fast [1 2 3])))
  (is (= [1 2 3]
         (p/distinct-fast [1 2 3 2 1 2 3 2 2])))
  (is (= []
         (p/distinct-fast []))))

#+clj
(defn are-fast-things-faster []
  (let [s (apply concat (repeat 100 (range 10000)))]
    (doseq [f [frequencies p/frequencies-fast distinct p/distinct-fast]]
      (println f)
      (dotimes [_ 5]
        (time (doall (f s)))))))

(deftest distinct-by-test
  (is (= [{:id 1 :data "a"}]
         (p/distinct-by :id
                        [{:id 1 :data "a"}
                         {:id 1 :data "b"}])))
  (is (= [1 2 3 2 1]
         (map second
              (p/distinct-by
               first
               [[1 1]
                [1 10]
                [17 2]
                [1 12]
                [:foo 3]
                [:foo 3]
                ['bar 2]
                [1 3]
                [3 1]])))))

#+clj
(deftest distinct-id-test
  (let [x (p/distinct-id [:a :b :c :a :b (Long. 1) (Long. 1)])]
    (is (= 5 (count x)))
    (is (= #{:a :b :c 1} (set x)))
    (is (= #{:a :b :c 1} (set x)))
    (is (empty? (p/distinct-id nil)))))

(deftest interleave-all-test
  (is (= [:a 0 :b 1 :c :d] (p/interleave-all [:a :b :c :d] [0 1]))))

(deftest count-when-test
  (is (= 5 (p/count-when even? (range 10)))))

(deftest conj-when-test
  (is (= [:a :b :c]
         (p/conj-when [:a] :b nil :c))))

(deftest cons-when-test
  (is (= [1 2] (p/cons-when nil [1 2])))
  (is (= [1 2] (p/cons-when false [1 2])))
  (is (= [3 1 2] (p/cons-when 3 [1 2]))))

(deftest rsort-by-test
  (is (= [5 4 3 2 1] (p/rsort-by identity [3 2 1 4 5]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Control flow

(deftest ?>>-test
  (let [side-effect (atom [])]
    (is (= (range 10)
           (->> (range 10)
                (p/?>> false
                       ((do (swap! side-effect conj :bad) map) inc)
                       (map inc)))))
    (is (empty? @side-effect))
    (is (= (range 2 12)
           (->> (range 10)
                (p/?>> true
                       ((do (swap! side-effect conj :good) map) inc)
                       (map inc)))))
    (is (= @side-effect [:good]))))

(deftest ?>-test
  (let [side-effect (atom [])]
    (is (= {:a 1}
           (-> {:a 1}
               (p/?> false
                     ((do (swap! side-effect conj :bad) assoc) :b 1)
                     (dissoc :a)))))
    (is (empty? @side-effect))
    (is (= {:b 1}
           (-> {:a 1}
               (p/?> true
                     ((do (swap! side-effect conj :good) assoc) :b 1)
                     (dissoc :a)))))
    (is (= @side-effect [:good]))))

(deftest fn->-test
  (is (= {:a 1 :b 1} ((p/fn-> (assoc :a 1)) {:b 1}))))

(deftest fn->>-test
  (is (= (range 1 11)  ((p/fn->> (map inc)) (range 10)))))

(deftest <--test
  (is (= [2 3]
         (-> {1 1}
             (assoc 3 4)
             (update-in [1] inc)
             (->> (p/map-vals dec)
                  (p/map-keys inc)
                  (p/<- (update-in [2] inc)
                        (map [2 4])))))))

(deftest as->>-test
  (is (= [1 2 3]
         (->> (range 5)
              (map inc)
              (p/as->> x (drop-last 2 x))))))

(deftest memoized-fn-test
  (let [calls (atom 0)]
    (is (= 55
           ((p/memoized-fn fib [x] (swap! calls inc)
                           (case x 0 0 1 1 (+ (fib (- x 1)) (fib (- x 2)))))
            10)))
    (is (= 11 @calls))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous

(deftest swap-pair!-test
  (let [a (atom {:a 1})]
    (is (= [{:a 1} {:a 2}] (p/swap-pair! a #(update-in % [:a] inc)))))
  (let [a (atom {:a 1})]
    (is (= [{:a 1} {:a 2}] (p/swap-pair! a update-in [:a] inc)))))

(deftest get-and-set!-test
  (let [a (atom 1)]
    (is (= 1 (p/get-and-set! a 2)))
    (is (= 2 @a))))

(deftest mapply-test
  (letfn [(f [& {:as m}]
            (p/for-map [[k v] m] v k))
          (g [a b c & {:as m}]
            {:init [a b c] :m m})]
    (is (= {42 :foo 90 :bar} (p/mapply f {:bar 90 :foo 42})))
    (is (= {:init [1 2 3]
            :m nil}
           (p/mapply g 1 2 3 {})))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fnk

(deftest letk-test
  (let [called? (atom false)
        om {:a 1 :c 3 :d 4 :e 17 :g 22}]
    (p/letk [[a { b 2} c d {e 5} :as m & more] om]
      (is (= [a b c d e] [1 2 3 4 17]))
      (is (= m om))
      (is (= {:g 22} more))
      (reset! called? true))
    (is @called?)
    (p/letk [[:as m] om]
      (is (= m om)))
    (p/letk [[a & m] om]
      (is (= a 1))
      (is (= m (dissoc om :a))))
    (p/letk [[a] {:a {:b 1}}
             [b] a]
      (is (= b 1)))
    (p/letk [[a] {:a [{:c 3}]}
             b (first a)
             [c] b]
      (is (= c 3)))
    (is (thrown? Throwable
                 (p/letk [[a] {:b 2}] a)))))

(deftest letk-self-shadow-test
  (is (= 2 (let [a 1] (p/letk [[{a a}] {:a 2}] a))))
  (is (= 1 (let [a 1] (p/letk [[{a a}] {}] a))))
  (is (= 2 (let [a 1] (p/letk [[{b/a a}] {:b/a 2}] a))))
  (is (= 1 (let [a 1] (p/letk [[{b/a a}] {}] a)))))

(deftest letk-single-shadow-test
  (let [a 1 b 2 c 3 e 4 e 5]
    (is (= [8 8 8 5 10] (p/letk [[c {a c} {b a} {d e} e] {:c 8 :e 10}] [a b c d e])))
    (is (= [8 8 8 5 10] (p/letk [[c [:nest {a c} {b a} {d e}] e] {:c 8 :e 10 :nest {}}] [a b c d e])))))

(deftest letk-dont-require-map-for-nested-only-as
  (is (= 1 (p/letk [[[:a :as a]] {:a 1}] a))))

#+clj
(deftest letk-no-multiple-binding-test
  (is (thrown? Exception (eval '(p/letk [[a a] {:a 1}] a))))
  (is (thrown? Exception (eval '(p/letk [[a/a b/a] {:a/a 1 :b/a 2}] a))))
  (is (= 1 (p/letk [[a] {:a 1} [a] {:a a}] a)))
  (is (= 1 (p/letk [[a/b] {:a/b 1} [a/b] {:a/b b}] b))))

(deftest letk-multi-shadow-test
  (let [a 1 b 2 c 3 e 4 e 5
        inp {:c 8 :e 10}]
    (is (= [8 8 8 5 10] (p/letk [[c] inp
                                 [{a c}] inp
                                 [{b a}] inp
                                 [{d e}] inp
                                 [e] inp]
                          [a b c d e])))))

(deftest letk-qualified-key-test
  (let [m {:a/b 1 :c/d {:e/f 2 :a/b 2}}]
    (is (= 1 (p/letk [[a/b] m] b)))
    (is (= 2 (p/letk [[[:c/d e/f]] m] f)))
    (is (= 2 (p/letk [[a/b] m [[:c/d a/b]] m] b))))
  (is (= 2 (p/letk [[a/b] {:a/b 1} [a/b] {:a/b 2}] b)))
  (is (= 2 (p/letk [[[:a/b :as c]] {:a/b 2}] c))))

(deftest when-letk-test
  (is (= "123" (p/when-letk [[a b c] {:a 1 :b 2 :c 3}] (str a b c))))
  (is (= 5 (p/when-letk [[five] {:five 5}] 1 2 3 4 five)))
  (is (nil? (p/when-letk [[a b c] nil] (throw (Exception.))))))

(deftest if-letk-test
  (is (= "then" (p/if-letk [[a b c] {:a 1 :b 2 :c "then"}] c (throw (Exception.)))))
  (is (= "else" (p/if-letk [[a b c] nil] (throw (Exception.)) "else")))
  (is (nil? (p/if-letk [[a b c] nil] (throw (Exception.))))))

(deftest fnk-test
  (testing "error on invalid input"
    (is (thrown? Throwable ((p/fnk [a] a) {:b 1}))))

  (let [call-count (atom 0)
        om {:a 1 :c 3 :d 4 :e 17 :g 22}]
    (testing "basic fnk"
      ((p/fnk [a b]
         (is (= a 1))
         (is (= b 2))
         (swap! call-count inc))
       {:a 1 :b 2}))
    (testing "complex fnk"
      ((p/fnk [a {b 2} c d {e 5} :as m & more]
         (is (= [a b c d e] [4 2 3 4 17]))
         (is (= m (assoc om :a 4 :h 77)))
         (is (= {:g 22 :h 77} more))
         (swap! call-count inc))
       (assoc om :a 4 :h 77)))
    (testing "both fnks called"
      (is (= @call-count 2)))
    (testing "dependent optional values"
      (is (= [1 2 3]
             ((p/fnk [a {b (* a 2)} {c (inc b)}] [a b c]) {:a 1}))))

    #+clj
    (testing "positional-fn"
      (let [f (p/fnk [a {b 2} [:c :as c0] [:d d1 {d2 2} [:d3 :as d30] [:d4 d41 :as d4]]]
                (is (= [a b c0 d1 d2 d30 d41 d4]
                       [4 2 3 4 2 17 18 {:d41 18 :d42 :foo}]))
                (swap! call-count inc))]
        (f {:a 4 :c 3 :d {:d1 4 :d3 17 :d4 {:d41 18 :d42 :foo}}})
        ((fnk-impl/positional-fn f [:d :a :c])
         {:d1 4 :d3 17 :d4 {:d41 18 :d42 :foo}} 4 3)
        (is (= @call-count 4))
        (is (thrown? Throwable ((p/fnk [a] a) {:b 3}))))))

  (testing "fnk output-schema"
    (doseq [f [(p/fnk [] {:a 1 :b {:b1 2}})
               (p/fnk f :- {:a s/Any :b {:b1 s/Any}} []
                 (hash-map :a 1 :b {:b1 2} :c 3))]]
      (is (= (pfnk/output-schema f) {:a s/Any :b {:b1 s/Any}})))
    (let [a :k]
      (is (= (pfnk/output-schema (p/fnk [a] {a a})) s/Any))))

  (testing "metadata via reader macro"
    (let [fnk-with-meta ^{:has-meta true} (p/fnk [])]
      (is (:has-meta (meta fnk-with-meta)))))

  (testing "name if proivded"
    (is (= 'bob (pfnk/fnk-name (p/fnk bob []))))
    (is (nil? (pfnk/fnk-name (p/fnk []))))))

(deftest fnk-input-schema-test
  (testing "simple fnk with one string key"
    (doseq [[t f] {"no-as" (p/fnk [a :- s/Str] a)
                   "with-as" (p/fnk [a :- s/Str :as b] a)}]
      (testing t
        (is (= {:a s/Str s/Keyword s/Any}
               (pfnk/input-schema f)))
        (is (= "hi" (f {:a "hi"})))
        (is (= "hi" (f {:a "hi" :b 123})))
        (is (thrown? Exception (f {:a :lo})))
        (is (thrown? Exception (f {:a "hi" "b" "no-string-keys"})))))
    (is (= :lo ((p/fnk ^:never-validate foo [a :- s/Str] a) {:a :lo}))))

  (testing "schemas on nested and optional bindings"
    (doseq [[t f] {"no-as" (p/fnk [a :- s/Str {b :- s/Str "1"} [:c d :- s/Num]]
                             [a b d])
                   "with-as" (p/fnk [a :- s/Str {b :- s/Str "1"} [:c d :- s/Num] :as m]
                               [a b d])}]
      (testing t
        (is (= {:a s/Str
                (s/optional-key :b) s/Str
                :c {:d s/Num s/Keyword s/Any}
                s/Keyword s/Any}
               (pfnk/input-schema f)))
        (is (= ["hi" "1" 2] (f {:a "hi" :c {:d 2}})))
        (is (= ["hi" "1" 2] (f {:a "hi" :c {:d 2 :e 3} :f :g})))
        (is (= ["hi" "bye" 2] (f {:a "hi" :b "bye" :c {:d 2}})))
        (is (thrown? Exception (f {:a "hi" :c {:d "2"}})))
        (is (thrown? Exception (f {:a "hi" :b :bye :c {:d 2}}))))))

  (testing "schemas on & bindings"
    (let [f (p/fnk [a :- s/Str [:b c & more :- {s/Keyword s/Num}] & more :- {}]
              [a c])]
      (is (= {:a s/Str
              :b {:c s/Any s/Keyword s/Num}}
             (pfnk/input-schema f)))
      (is (= ["hi" 1] (f {:a "hi" :b {:c 1}})))
      (is (= ["hi" 1] (f {:a "hi" :b {:c 1 :z 3}})))
      (is (thrown? Exception (f {:a "hi" :b {:c 1 :z "3"}})))
      (is (thrown? Exception (f {:a "hi" :b {:c 1} :d :e})))))

  (testing "schema override on top-level map bindings"
    (let [override {:a s/Num (s/optional-key :b) s/Str (s/optional-key :e) s/Str}]
      (doseq [[t f] {"no-as" (p/fnk [a :- s/Str {b :- s/Str "1"}] :- override
                               [a b])
                     "with-as" (p/fnk [a :- s/Str {b :- s/Str "1"} :as m] :- override
                                 [a b])}]
        (testing t
          (is (= override (pfnk/input-schema f)))
          (is (= [2 "1"] (f {:a 2})))
          (is (= [2 "2"] (f {:a 2 :b "2"})))
          (is (= [2 "2"] (f {:a 2 :b "2" :e "asdf"})))
          (is (thrown? Exception (f {:a "2"})))
          (is (thrown? Exception (f {:a 2 :b 2})))
          (is (thrown? Exception (f {:a 2 :z :huh})))))))

  (testing "schema override on inner map bindings"
    (let [f (p/fnk [a :- s/Str [:b c] :- {:c s/Str}]
              [a c])]
      (is (= {:a s/Str :b {:c s/Str} s/Keyword s/Any} (pfnk/input-schema f)))
      (is (= ["1" "2"] (f {:a "1" :b {:c "2"}})))
      (is (thrown? Exception (f {:a "1" :b {:c 2}})))
      (is (thrown? Exception (f {:a "1" :b {:c "2" :d "3"}})))))

  (testing "default values"
    (let [first-key-meta (p/fn-> pfnk/input-schema (dissoc s/Keyword) keys first meta)]
      (is (= {:default "foo"}
             (first-key-meta (p/fnk [{a :- s/Str "foo"}]))))
      (is (= {:default 'apple}
             (first-key-meta (p/fnk [apple {a :- s/Str apple}])))))))

(deftest fnk-qualified-key-test
  (is (= [1 2 3] ((p/fnk [a/b b/c c/d] [b c d]) {:a/b 1 :b/c 2 :c/d 3})))
  (is (= 1 ((p/fnk [[:a/b b/c]] c) {:a/b {:b/c 1}})))
  (is (= 1 ((p/fnk [{a/b 1}] b) {})))
  (is (= 1 ((p/fnk [[:a/b :as c]] c) {:a/b 1})))
  (testing "schemas"
    (let [f (p/fnk [a/b :- s/Str [:b/c c/d :- s/Keyword]] [b d])]
      (is (= ["hi" :bye] (f {:a/b "hi" :b/c {:c/d :bye}})))
      (is (= {:a/b s/Str
              :b/c {:c/d s/Keyword s/Keyword s/Any}
              s/Keyword s/Any}
             (pfnk/input-schema f)))
      (are [invalid-input] (thrown? Exception (f invalid-input))
           nil
           {}
           {:b "hi" :c {:d :bye}}
           {:a/b nil :b/c nil}
           {:a/b nil :b/c {:c/d :bye}}
           {:a/b "hi" :b/c {:c/d "bye"}}
           {:a/b "hi" :b/c :bye}))))

(p/defnk keyfn-test-docstring "whoa" [dude {wheres :foo} :as my & car]
  [dude wheres my car])

(p/defnk keyfn-test-no-docstring  [{city :sf} foo]
  [foo city])

(deftest defnk-test
  (is (= [11 :foo {:dude 11 :sweet 17} {:sweet 17}]
         (keyfn-test-docstring {:dude 11 :sweet 17})))
  (is (= [:foo :sf] (keyfn-test-no-docstring {:foo :foo})))
  (is (= [{:foo s/Any (s/optional-key :city) s/Any s/Keyword s/Any} s/Any]
         (pfnk/io-schemata keyfn-test-no-docstring)))
  (is (thrown? Throwable (keyfn-test-docstring :wheres :mycar))))

;; Test that type hints are properly propagated for fnk and defnk.
#+clj
(p/defnk ^Byte a-typehinted-defnk [^Long l]
  (.byteValue l))

#+clj
(deftest type-hints-test
  (is (= Byte (:tag (meta #'a-typehinted-defnk))))
  (doseq [f [a-typehinted-defnk
             (p/fnk [^Long l] (.byteValue l))
             (p/fnk [{^Long l 1}] (.byteValue l))
             (p/fnk [^Long l & m] (.byteValue l))]]
    (is (= (Byte. (byte 1)) (f {:l (Long. 1)})))
    (is (thrown? Exception (f {:l (Integer. 1)})))))

#+clj
(deftest ^:slow repeated-bindings-test
  (is (thrown? Exception (eval '(p/fnk [x [:x y]] (+ x y)))))
  (is (thrown? Exception (eval '(p/fnk [{x {:y 1}} [:x y]] (+ x y)))))
  (is (thrown? Exception (eval '(p/fnk [x :as x] (+ x y)))))
  (is (thrown? Exception (eval '(p/fnk [x & x] (+ x y)))))
  (is (thrown? Exception (eval '(p/fnk [{x {:y 1}} x] (+ x y)))))
  (is (thrown? Exception (eval '(p/fnk [x [:x y] :as m] (+ x y)))))
  (is (thrown? Exception (eval '(p/fnk [{x {:y 1}} [:x y] :as m] (+ x y)))))
  (is (thrown? Exception (eval '(p/fnk [{x {:y 1}} x :as m] (+ x y))))))

(deftest optional-self-shadow-test
  (is (= 1 (let [b 1] ((p/fnk [{a b}] a) {}))))
  (doseq [[desc f] (let [a 1]
                     {"pos" (p/fnk [{a a}] a)
                      "non-pos" (p/fnk [{a a} :as m] a)})]
    (testing desc
      (is (= 1 (f {})))
      (is (= 2 (f {:a 2}))))))

(deftest optional-cross-arg-shadow-test
  (doseq [[desc f] (let [a 1 b 2 c 3 e 4 e 5]
                     {"pos" (p/fnk [c {a c} {b a} {d e} e] [a b c d e])
                      "non-pos" (p/fnk [c {a c} {b a} {d e} e :as m] [a b c d e])})]
    (testing desc
      (is (= [6 7 8 9 10] (f {:a 6 :b 7 :c 8 :d 9 :e 10})))
      (is (= [8 7 8 9 10] (f {:b 7 :c 8 :d 9 :e 10})))
      (is (= [8 8 8 9 10] (f {:c 8 :d 9 :e 10})))
      (is (= [8 8 8 5 10] (f {:c 8 :e 10}))))))

(deftest dont-shadow-nested-test
  (let [m {:x 1}]
    (is (= 3 ((p/fnk [[:m x]] (+ x (:x m))) {:m {:x 2}})))))

(deftest miliis-test
  (let [now #+clj (System/currentTimeMillis) #+cljs (.getTime (js/Date.))
        threshold 5]
    (is (> threshold
           (- (p/millis) now)))))

(use-fixtures :once schema-test/validate-schemas)
