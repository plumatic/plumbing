;-
; Copyright 2008-2011 (c) Meikel Brandmeyer.
; All rights reserved.
;
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
;
; The above copyright notice and this permission notice shall be included in
; all copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
; THE SOFTWARE.

;; Retreived from https://bitbucket.org/kotarak/lazymap/raw/5a2437e70a91/src/main/clojure/lazymap/core.clj
;; by JW on 11/17/2011, and slightly modified to fix a bug in .entryAt and add a safety check.
;; Also replaced 'force' with 'deref' so this can be used with futures (with marginal utility).
;; (our pull request with these changes has been been ignored, so we've just included
;;  this fixed file in plumbing for the time being).

;; Known Issues: LazyMapEntries are not equal to persistent vectors like ordinary map entries.


(ns plumbing.lazymap
  "Lazymap is to maps what lazy-seq is to lists. It allows to store values
  with evaluating them. This is only done in case the value is really accessed.
  Lazymap works with any map type (hash-map, sorted-map, struct-map) and may
  be used as a drop-in replacement everywhere where a normal map type may be
  used.

  Available macros:
  lazy-hash-map, lazy-sorted-map, lazy-struct-map, lazy-struct, lazy-assoc
  and their * counterpart functions."
  (:import
    clojure.lang.IObj
    clojure.lang.IFn
    clojure.lang.ILookup
    clojure.lang.IMapEntry
    clojure.lang.IPersistentMap
    clojure.lang.IPersistentVector
    clojure.lang.ASeq
    clojure.lang.ISeq
    clojure.lang.Seqable
    clojure.lang.SeqIterator))

(defprotocol ILazyMapEntry
  "ILazyMapEntry describes the behaviour of a lazy MapEntry. It provides
  an additional method (over IMapEntry), which returns the raw delay object
  and not the forced value."
  (get-key       [lazy-map-entry] "Return the key of the map entry.")
  (get-raw-value [lazy-map-entry] "Return the underlying delay object."))

; Extend the IMapEntry interface to act also like ILazyMapEntry.
; For a IMapEntry get-raw-value just returns the given value as
; wrapped in a delay. Similar a vector of two elements might be
; used in place of a IMapEntry.
(extend-protocol ILazyMapEntry
  IMapEntry
  (get-key       [#^IMapEntry this] (.getKey this))
  (get-raw-value [#^IMapEntry this] (let [v (.getValue this)] (delay v)))
  IPersistentVector
  (get-key [this]
    (when-not (= (count this) 2)
      (throw (IllegalArgumentException.
               "Vector used as IMapEntry must be a pair")))
    (this 0))
  (get-raw-value
    [this]
    (when-not (= (count this) 2)
      (throw (IllegalArgumentException.
               "Vector used as IMapEntry must be a pair")))
    (let [v (this 1)]
      (delay v))))

(defprotocol ILazyPersistentMap
  "ILazyPersistentMap extends IPersistentMap with a method to allow
  transportation of the underlying delay objects."
  (delay-assoc [lazy-map key delay] "Associates the given delay in the map."))

(deftype LazyMapEntry [k v]
  ILazyMapEntry
  (get-key       [_] k)
  (get-raw-value [_] v)
  IMapEntry
  (key           [_] k)
  (getKey        [_] k)
  (val           [_] (deref v))
  (getValue      [_] (deref v))
  Object
  (toString      [_] (str \[ (pr-str k) \space (pr-str (deref v)) \])))

(defmethod print-method LazyMapEntry
  [this #^java.io.Writer w]
  (.write w (str this)))

(defn create-lazy-map-seq
  ([inner-seq]
   (create-lazy-map-seq inner-seq nil))
  ([inner-seq metadata]
   (proxy [ASeq] [metadata]
     ; ISeq
     (first []
       (let [first-val (first inner-seq)]
         (LazyMapEntry. (key first-val) (val first-val))))
     (next []
       (when-let [inner-rest (next inner-seq)]
         (create-lazy-map-seq inner-rest metadata)))
     (more [] (lazy-seq (next this))))))

(declare create-lazy-map)

(deftype LazyPersistentMap
  [base metadata]
  ILazyPersistentMap
  (delay-assoc [this k v] (create-lazy-map (assoc base k v) metadata))
  IPersistentMap
  (assoc       [this k v] (create-lazy-map (assoc base k (delay v)) metadata))
  (assocEx
    [this k v]
    (when (contains? base k)
      (throw (Exception. (str "Key already present in map: " k))))
    (.assoc this k v))
  (without     [this k] (create-lazy-map (dissoc base k) metadata))
  ; Associative
  (containsKey [this k] (contains? base k))
  (entryAt     [this k] (when-let [v (base k)] (LazyMapEntry. k v)))
  ; IPersistentCollection
  (count       [this]   (count base))
  (cons
    [this o]
    (if (satisfies? ILazyMapEntry o)
      (delay-assoc this (get-key o) (get-raw-value o))
      (into this o)))
  (empty [this]   (create-lazy-map (empty base) nil))
  ILookup
  (valAt [this k] (.valAt this k nil))
  (valAt
    [this k not-found]
    (if (contains? base k)
      (-> base (get k) deref)
      not-found))
  IFn
  (invoke [this k]           (.valAt this k nil))
  (invoke [this k not-found] (.valAt this k not-found))
  (applyTo
    [this args]
    (let [[k v & rest-args :as args] (seq args)]
      (when (or (not args) rest-args)
        (throw (Exception. "lazy map must be called with one or two arguments")))
      (.valAt this k v)))
  Seqable
  (seq
    [this]
    (when-let [inner-seq (seq base)]
      (create-lazy-map-seq inner-seq)))
  IObj
  (withMeta [this new-metadata] (create-lazy-map base new-metadata))
  ; IMeta
  (meta     [this] metadata)
  Iterable
  (iterator [this] (-> this .seq SeqIterator.)))

(defn create-lazy-map
  ([base]
   (create-lazy-map base nil))
  ([base metadata]
   (LazyPersistentMap. base metadata)))

(defn- quote-values
  [kvs]
  (assert (even? (count kvs)))
  (mapcat (fn [[k v]] [k `(delay ~v)]) (partition 2 kvs)))

(defn lazy-assoc*
  "lazy-assoc* is like lazy-assoc but a function and takes values as delays
  instead of expanding into a delay of val."
  [m & kvs]
  (assert (even? (count kvs)))
  (reduce (fn [m [k v]] (delay-assoc m k v)) m (partition 2 kvs)))

(defmacro lazy-assoc
  "lazy-assoc associates new values to the given keys in the given lazy map.
  The values are not evaluated, before their first retrieval. They are
  evaluated at most once."
  [m & kvs]
  `(lazy-assoc* ~m ~@(quote-values kvs)))

(defn lazy-hash-map*
  "lazy-hash-map* is the same as lazy-hash-map except that its a function
  and it takes a seq of keys-delayed-value pairs."
  [& kvs]
  (create-lazy-map (apply hash-map kvs)))

(defmacro lazy-hash-map
  "lazy-hash-map creates a map. The values are not evaluated before their
  first retrieval. Each value is evaluated at most once. The underlying map
  is a hash map."
  [& kvs]
  `(lazy-hash-map* ~@(quote-values kvs)))

(defn lazy-sorted-map*
  "lazy-sorted-map* is the same as lazy-sorted-map except that its a
  function and it takes a seq of keys-delayed-value pairs."
  [& kvs]
  (create-lazy-map (apply sorted-map kvs)))

(defmacro lazy-sorted-map
  "lazy-sorted-map creates a map. The values are not evaluated before their
  first retrieval. Each value is evaluated at most once. The underlying map
  is a sorted map."
  [& kvs]
  `(lazy-sorted-map* ~@(quote-values kvs)))

(defn lazy-struct-map*
  "lazy-struct-map* is the same as lazy-struct-map except that its a
  function and it takes a seq of keys-delayed-value pairs together with the
  struct basis."
  [s & kvs]
  (create-lazy-map (apply struct-map s kvs)))

(defmacro lazy-struct-map
  "lazy-struct-map creates a map. The values are not evaluated before their
  first retrieval. Each value is evaluated at most once. The underlying map
  is a struct map according to the provided structure s."
  [s & kvs]
  `(lazy-struct-map* ~s ~@(quote-values kvs)))

(defn lazy-struct*
  "lazy-struct* is the same as lazy-struct except that its a function and
  it takes a seq of delayed value together with the struct basis."
  [s & vs]
  (create-lazy-map (apply struct s vs)))

(defmacro lazy-struct
  "lazy-struct creates a map. The values are not evaluated before their
  first retrieval. Each value is evaluated at most once. The underlying map
  is a struct map according to the provided structure s. As with Clojure's
  struct the values have to appear in the order of the keys in the structure."
  [s & vs]
  (let [vs (map (fn [v] `(delay ~v)) vs)]
    `(lazy-struct* ~s ~@vs)))