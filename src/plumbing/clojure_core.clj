(ns plumbing.clojure-core
  "faster versions of clojure.core functions"
  (:refer-clojure :exclude [frequencies distinct]))

(defn frequencies
  "can be faster if the same item will show up multiple times"
  [xs]
  (let [res (java.util.HashMap.)]
    (doseq [x xs]
      (.put res x (unchecked-inc (int (or (.get res x) 0)))))
    res
    (into {} res)))
 
(defn distinct [xs] 
  (let [s (java.util.HashSet.)] 
    (filter #(when-not (.contains s %) (.add s %) true) xs)))

;; Replace clojure.core with faster versions
(alter-var-root #'clojure.core/frequencies (constantly frequencies))
(alter-var-root #'clojure.core/distinct (constantly distinct))
