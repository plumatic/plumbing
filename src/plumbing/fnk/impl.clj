(ns plumbing.fnk.impl
  "Core utilities for parsing our 'fnk'-style binding syntax and generating bodies.
   Documented and tested through the actual 'letk','fnk', and 'defnk' macros in plumbing.core."
  (require 
   [plumbing.fnk.schema :as schema]
   [plumbing.fnk.pfnk :as pfnk]))


;;; Helpers

(defn safe-get 
  "Like (get m k), but throws if k is not present in m."
  [m k key-path]
  (when-not (map? m)
    (throw (RuntimeException. (format "Expected a map at key-path %s, got type %s" key-path (class m)))))
  (let [[_ v :as p] (find m k)]
    (when-not p (throw (RuntimeException. (format "Key %s not found in %s" k (keys m)))))
    v))

(defn assert-distinct 
  "Like (assert (distinct things)) but with a more helpful error message."
  [things]
  (let [repeated-things (->> things
                             frequencies
                             (filter #(> (val %) 1))
                             seq)]
    (schema/assert-iae (empty? repeated-things) "Got repeated items (expected distinct): %s" repeated-things)))

;;; Core fnk utilities

(defn- parse-letk-binding
  "Parse a binding form into required [sym k] bindings (here sym~=k), optional [sym v] bindings,
   nested {sym binding-form} bindings, and possible :as and & symbols."
  [binding-form]
  (schema/assert-iae (vector? binding-form) "Binding form is not vector: %s" binding-form)
  (let [[binding-form more-sym] (if (= (last (butlast binding-form)) '&)
                                  [(drop-last 2 binding-form)
                                   (doto (last binding-form)
                                     (-> symbol? (schema/assert-iae "Argument to & not a symbol: %s" binding-form)))]
                                  [binding-form nil])
        [bindings as-sym]       (if (= (last (butlast binding-form)) :as)
                                  [(drop-last 2 binding-form)
                                   (doto (last binding-form)
                                     (-> symbol? (schema/assert-iae "Argument to :as not a symbol: %s" binding-form)))]
                                  [binding-form (gensym "map")])
        [optional more]     ((juxt filter remove) map? bindings)
        optional            (for [opt optional]
                              (do (schema/assert-iae (and (= (count opt) 1) (symbol? (key (first opt))))
                                              "Invalid optional binding %s" opt)
                                  (first opt)))
        [nested more]       ((juxt filter remove) vector? more)
        parsed-nested       (for [[sub-k & sub-bind] nested]
                              (do (schema/assert-iae (keyword? sub-k)
                                              "First element to nested binding not a keyword: %s" sub-k)
                                  [(gensym (name sub-k)) [sub-k (vec sub-bind)]]))
        required            (concat
                             (for [[s [k]] parsed-nested]
                               [s k])
                             (for [b more]
                               (if (symbol? b)
                                 [b (keyword b)]
                                 (throw (RuntimeException. (format "Unsupported binding form %s" b))))))]
    (schema/assert-iae (not (some #{'&} (map first required))) "Cannot bind to &")
    (assert-distinct (concat (map first required) (map first optional) (remove nil? [more-sym as-sym])))
    (assert-distinct (concat (map second required) (map first optional)))
    {:required required :optional optional :nested parsed-nested :as as-sym :more more-sym}))

(defn generate-letk-level 
  "Generate the binding form for a single level of 'letk' bindings (i.e., with no nested bindings)"
  [{:keys [required optional as more]} map-form body key-path]
  [`(let ~(vec
             (concat
              [as map-form]
              (mapcat (fn [[r rk]] [r `(safe-get ~as ~rk ~key-path)]) required)
              (mapcat (fn [[o v]] [o `(get ~as ~(keyword o) ~v)]) optional)
              (when more
                [more `(dissoc ~as ~@(map keyword (concat (map second required) (map first optional))))])))
      ~@(when (or (seq required) (seq optional) more) ;; allow naked :as for non-map...
          [`(schema/assert-iae (map? ~as) "Form to be destructured is not a map")])
      ~@body)
   (map first (concat required optional))
   (into {}
         (concat
          (for [r (map second required)]
            [(keyword r) true])
          (for [o (map first optional)]
            [(keyword o) false])))])

(defn letk*
  "Take a letk/fnk binding form, a form that generates a map to bind from, and a body, and 
   return a triple [output-form bound-syms input-schema], where:  
     output-form is the final output form that executes body in the scope of the provided bindings,
     bound-syms is the set of symbols bound, which must be unique, and
     input-schema is the input schema imposed on the map-form by the binding."
  [binding-form map-form body & [key-path]]
  (let [{:keys [nested,] :as parsed-binding} (parse-letk-binding binding-form)

        [inner-form inner-bound-syms input-schema]
          (reduce
           (fn wrap-nested [[frm syms spec] [nest-sym [nest-k nest-b]]]
              (let [[sub-frm sub-syms sub-spec] (letk* nest-b nest-sym frm (conj (or key-path []) nest-k))]
                [(list sub-frm) (concat sub-syms syms) (assoc spec nest-k sub-spec)]))
            [body [] {}]
            nested)

        [outer-form outer-bound-syms outer-input-schema]
           (generate-letk-level parsed-binding map-form inner-form key-path)]
    (assert-distinct (concat outer-bound-syms inner-bound-syms))
    [outer-form 
     (distinct (concat outer-bound-syms inner-bound-syms)) 
     (merge outer-input-schema input-schema)]))

;; TODO: saving body form could enable more efficient graph compilations (like direct 'let' or positional fns)
(defn fnk* 
  "Take an optional name, binding form, and body for a fnk, and make an IFn/PFnk for these arguments"
  [name? bind body]
  (let [map-sym (gensym)
        [final-body _ input-schema] (letk* bind map-sym body)
        name? (or name? (gensym "fnk"))]
    (pfnk/fn->fnk
     `(fn ~name?
         ([~map-sym]
            (schema/assert-iae (map? ~map-sym) "fnk called on non-map: %s" ~map-sym)
            ~final-body)
         ([arg1# & more#] ;; allow splatting args like (f :a 1) rather than (f {:a 1}).  TODO: deprecate?
            (if (odd? (count more#))
              (~name? (apply hash-map arg1# more#))
              (do (schema/assert-iae (map? arg1#) "fnk called with odd args and no initial map")
                  (~name? (apply assoc arg1# more#))))))
     [input-schema
      (schema/make-output-schema (last body) (eval (:output-schema (meta bind) true)))])))
