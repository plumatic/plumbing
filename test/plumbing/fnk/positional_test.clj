(ns plumbing.fnk.positional-test
  (:use plumbing.core clojure.test)
  (:require
   [plumbing.graph :as graph]
   #_[plumbing.fnk.positional :as fnk-positional]))

(comment
 (defn run [g ks vs]
   (apply (fnk-positional/positional-compile g ks) vs))

 (deftest positional-compile-test
   (let [g (graph/graph
            :c (fnk-positional/fnk-positional cfn [] 1)
            :x (fnk-positional/fnk-positional xfn [p1] (inc p1))
            :y (fnk-positional/fnk-positional yfn [x] (inc x)))
         c (fnk-positional/positional-compile g [:p1])
         c-just-y (fnk-positional/positional-compile g [:p1] :y)
         l (c 42)]
     (is (= (:c l) 1))
     (is (= (:y l) 44))
     (is (= (:x l) 43))
     (is (= (c-just-y 42) 44)))

   (comment
     ;; nested subgraphs don't work yet.
     (let [g2 (graph/graph
               :x (fnk-positional/fnk-positional [] 1)
               :y {:z (fnk-positional/fnk-positional [a] 1)})]
       (is (= {:x 1 :y {:z 1}}
              (run g2 [:a] [1])))))
   ))