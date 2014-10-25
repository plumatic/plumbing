(ns plumbing.fnk.pfnk
  "Core protocol and helpers for schema.core to extract and attach
   input and output schemas to fnks. This protocol says nothing about
   how fnks are created, so users are free to create PFnks directly
   using fn->fnk, or using custom binding syntax (of which 'fnk' et al
   are one possible example)."
  (:require
   [schema.core :as s :include-macros true]
   [plumbing.fnk.schema :as schema :include-macros true]))

#+clj (set! *warn-on-reflection* true)

(defprotocol PFnk
  "Protocol for keyword functions and their specifications, e.g., fnks and graphs."
  (io-schemata [this]
    "Return a pair of [input-schema output-schema], as specified in plumbing.fnk.schema."))

(defn input [^schema.core.FnSchema s]
  (let [[[is :as args] :as schemas] (.-input-schemas s)]
    (schema/assert-iae (= 1 (count schemas)) "Fnks have a single arity, not %s" (count schemas))
    (schema/assert-iae (= 1 (count args)) "Fnks take a single argument, not %s" (count args))
    (schema/assert-iae (instance? schema.core.One is) "Fnks take a single argument, not variadic")
    (let [s (.-schema ^schema.core.One is)]
      (schema/assert-iae (map? s) "Fnks take a map argument, not %s" (type s))
      s)))

(defn output [^schema.core.FnSchema s]
  (.-output-schema s))

(extend-type #+clj clojure.lang.Fn #+cljs object
             PFnk
             (io-schemata [this]
               (assert (fn? this))
               ((juxt input output) (s/fn-schema this))))

(defn input-schema [pfnk]
  (first (io-schemata pfnk)))

(defn output-schema [pfnk]
  (second (io-schemata pfnk)))

(defn input-schema-keys [f]
  (-> f input-schema schema/explicit-schema-key-map keys))

(defn fn->fnk
  "Make a keyword function into a PFnk, by associating input and output schema metadata."
  [f [input-schema output-schema :as io]]
  (s/schematize-fn f (s/=> output-schema input-schema)))

#+clj (set! *warn-on-reflection* false)
