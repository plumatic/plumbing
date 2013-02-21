(ns plumbing.resource-test
  (:use clojure.test plumbing.resource plumbing.core)
  (:require
   [plumbing.fnk.pfnk :as pfnk]
   [plumbing.map :as map]
   [plumbing.graph :as graph]
   ))

(defn simple-deref-resource [r close-fn handle-get-fn] 
  (make-fundle (constantly r) (when handle-get-fn {:get handle-get-fn}) 
               (when close-fn #(close-fn r))))

(deftest resource-transform-test
  (with-redefs [plumbing.observer/sub-observer (fn [o k] (conj o k))]
    (let [g {:first (fnk [i observer]
                      (is (= observer [:first]))
                      (simple-deref-resource
                       (inc i)
                       nil (constantly :first-h)))
             :second (fnk [first] (inc @first))
             :third {:third1 (fnk [second observer]
                                  (is (= observer [:third :third1]))
                                  (inc second))
                     :third2 (fnk [third1] 
                               (simple-deref-resource
                                (inc third1)
                                nil (constantly :second-h)))}}
          res (graph/run (resource-transform :instantiated-atom (observer-transform g)) 
                         {:i 1 :observer []})]
      (is (= {:first 2
              :second 3
              :third {:third1 4
                      :third2 5}}
             (map/map-leaves #(if (number? %) % @%) (dissoc res :instantiated-atom))))
      (is (= [[:first] [:second] [:third :third1] [:third :third2]]
             (map first @(:instantiated-atom res))))
      (let [[v1 v2 v3 v4] (map second @(:instantiated-atom res))]
        (is (= [3 4] [v2 v3])))
      (is (= {:first {:get :first-h}
              :third {:third2 {:get :second-h}}}
             (map/map-leaves #(% {}) (handlers-map @(:instantiated-atom res))))))))


(deftest shutdown!-test
  (let [shutdown-atom (atom [])
        shut! (fn [k] (swap! shutdown-atom conj k))
        g {:first (fnk [i]
                    (simple-deref-resource (inc i) shut! nil))
           :second (fnk [first] (inc @first))
           :third {:third1 (fnk [second] (inc second))
                   :third2 (fnk [third1] 
                             (simple-deref-resource (inc third1) shut! nil))}
           :fourth (fnk [[:third third1]]
                     (simple-deref-resource (* third1 2) shut! nil))
           :fifth (fnk [fourth]
                    (reify
                      clojure.lang.IDeref
                      (deref [this] (* @fourth 10))
                      PCloseable
                      (close [this] (shut! "SPARTA!!!!"))))}
        res (graph/run (resource-transform :instantiated-atom g)
                       {:i 1})]
    (is (= {:first 2
            :second 3
            :third {:third1 4
                    :third2 5}
            :fourth 8
            :fifth 80}
           (map/map-leaves #(if (number? %) % @%) (dissoc res :instantiated-atom))))
    (shutdown! @(:instantiated-atom res))
    (is (= ["SPARTA!!!!" 8 5 2] @shutdown-atom))))
