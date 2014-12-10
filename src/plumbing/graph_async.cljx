(ns plumbing.graph-async
  #+cljs
  (:require-macros
   [cljs.core.async.macros :refer [go]])
  (:require
   #+clj [clojure.core.async :as async :refer [go <! >!]]
   #+cljs [cljs.core.async :as async :refer [<! >!]]
   #+clj [clojure.core.async.impl.protocols :as async-protocols]
   #+cljs [cljs.core.async.impl.protocols :as async-protocols]
   [plumbing.fnk.pfnk :as pfnk]
   [plumbing.fnk.schema :as schema :include-macros true]
   [plumbing.core :as plumbing :include-macros true]
   [plumbing.graph :as graph :include-macros true]))

(defn asyncify
  "Take a fnk f and return an async version by wrapping non-channel
  return values in a channel"
  [f]
  (pfnk/fn->fnk
   (fn [m]
     (let [v (f m)]
       (if (satisfies? async-protocols/ReadPort v)
         v
         (go v))))
   (pfnk/io-schemata f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public

(defn async-compile
  "Experimental.

   Compile a hierarchical graph with (some) async fnks into an channel that
   contains the computed graph once completed.

   Each fnk can perform async operations by returning a channel that contains
   its node value once completed.

   Each node function will be evaluated as its dependencies have been fully
   computed."
  [g]
  (if (fn? g)
    (asyncify g)
    (let [g (graph/->graph (plumbing/map-vals async-compile g))
          req-ks (schema/required-toplevel-keys (pfnk/input-schema g))
          edges (concat
                 (for [[k v] g
                       parent-k (filter g (pfnk/input-schema-keys v))]
                   [parent-k k])
                 (for [k (keys g)]
                   [k ::done]))
          child-map (->> edges
                         (group-by first)
                         (plumbing/map-vals #(set (map second %))))
          parent-map (->> edges
                          (group-by second)
                          (plumbing/map-vals #(set (map first %))))]
      (pfnk/fn->fnk
       (fn [m]
         (let [missing-keys (seq (remove #(contains? m %) req-ks))]
           (schema/assert-iae (empty? missing-keys)
                              "Missing top-level keys in graph input: %s"
                              (set missing-keys)))
         (let [result (async/chan)
               remaining-parents (atom parent-map)
               results (atom m)
               run-node (fn run-node [k]
                          (go
                           (if (= ::done k)
                             (>! result (select-keys @results (keys g)))
                             (let [f (g k)
                                   r (<! (f (select-keys @results (pfnk/input-schema-keys f))))]
                               (swap! results assoc k r)
                               (doseq [c (child-map k)]
                                 (when (empty? (c (swap! remaining-parents
                                                         update-in [c]
                                                         disj k)))
                                   (run-node c)))))))]
           (doseq [k (keys g)]
             (when (empty? (parent-map k))
               (run-node k)))
           result))
       (pfnk/io-schemata g)))))
