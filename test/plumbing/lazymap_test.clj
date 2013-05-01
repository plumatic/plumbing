(ns plumbing.lazymap-test
  (:use plumbing.core clojure.test lazymap.core))

(deftest lazy-map-entry-extend-test
  (is (= :a (get-key [:a 2])))
  (is (= 2 @(get-raw-value [:a 2])))
  (is (thrown? IllegalArgumentException (get-key [:a])))
  (is (thrown? IllegalArgumentException (get-raw-value [:a])))

  (is (= :a (get-key (first {:a 2}))))
  (is (= 2 @(get-raw-value (first {:a 2})))))

(deftest lazy-hash-map-test
  (let [evals (atom [])
        recorded-val (fn [x] (swap! evals conj x) x)
        base (lazy-hash-map :a (recorded-val 1) :b (recorded-val 2))
        b2 (lazy-assoc base :a (recorded-val 11) :d (recorded-val 3))
        b3 (assoc b2 :d 33)]
    (is (true? (map? b2)))
    (is (= #{:a :b :d} (set (keys b2))))
    (is (= #{:b :d} (set (keys (dissoc b2 :a)))))
    (is (empty? (meta base)))
    (is (= {:foo :bar} (meta (with-meta b2 {:foo :bar}))))
    (is (true? (contains? b3 :a)))
    (is (not (contains? b3 :f)))
    (is (not (nil? (find b3 :a))))
    (is (nil? (find b3 :f)))
    (is (not (empty? b3)))
    (is (empty? (dissoc b3 :a :b :d)))

    (is (empty? @evals))

    (is (= {:a 11 :d 33} (select-keys b3 [:a :d])))
    (is (= #{11} (set @evals)))

    (is (= 11 (get b3 :a)))
    (is (= 11 (get b3 :a 12)))
    (is (= nil (get b3 :f)))
    (is (= 12 (get b3 :f 12)))
    (is (= #{11 3} (set (vals (dissoc b2 :b)))))
    (is (= #{:a :b :c} (set (keys (merge base {:c :e})))))
    (is (= :a (key (first (dissoc base :b)))))

    (is (= #{11 3} (set @evals)))
    (is (= 2 (count @evals)))

    (is (= 1 (val (first (dissoc base :b)))))
    (is (= #{11 3 1} (set @evals)))
    (is (= 3 (count @evals)))

    (is (= 2 (base :b)))
    (is (= 3 (base :f 3)))
    (is (= nil (base :f)))
    (is (= 3 (apply base [:f 3])))

    (is (= #{11 2 3 1} (set @evals)))
    (is (= 4 (count @evals)))

    (is (= {:a 1} (select-keys base [:a])))
    (is (= {:a 1 :b 2} (into {} base)))
    ;; (is (= [[:a 1]] (seq (dissoc base :b)))) ;; TODO: this fails because of entry equality.

    (let [b4 (delay-assoc b2 :d (delay (recorded-val 42)))]
      (is (= 4 (count @evals)))
      (is (= {:a 11 :b 2 :d 42} (into {} b4))))

    (is (= #{11 2 3 1 42} (set @evals)))
    (is (= 5 (count @evals)))

    (is (= {:a 1 :b 2 :d 3} (into {} (merge b3 b2 base))))
    (is (= 5 (count @evals)))))