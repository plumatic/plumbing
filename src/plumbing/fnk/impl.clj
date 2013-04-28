(ns plumbing.fnk.impl
  "Core utilities for parsing our 'fnk'-style binding syntax.
   Documented and tested through the actual 'letk','fnk', and 'defnk'
   macros in plumbing.core.

   The core entry points into this namespace are 'letk*' and 'fnk*',
   which parse the new binding syntax and generate fnk bodies,
   respectively.

   For efficiency, two different methods of generating fnk bodies are
   used.  If the fnk takes a fixed set of arguments (i.e., no & or
   :as), then a 'positional' version of the fnk that is called like an
   ordinary Clojure fn (e.g., (f a b) rather than (f {:a a :b b}) is
   generated as an implementation detail, and stored in metadata of
   the actual keyword fnk (which is just a thin wrapper around the
   positional version).  If '& or :as are used, no such positional
   function is generated.

   The advantage of these 'positional' functions is that they can be
   accessed using 'efficient-call-forms' or 'positional-fn' to call
   the fnk without incurring the overhead of producing and then
   destructuring a top-level map.  See plumbing.graph.positional for
   an example use."
  (require
   [clojure.set :as set]
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

;;; Parsing new fnk binding style

(defn- parse-letk-binding
  "Parse a binding form into required [sym k] bindings (here sym~=k),
   optional [sym v] bindings, nested {sym binding-form} bindings, and
   possible :as and & symbols."
  [binding-form]
  (schema/assert-iae (vector? binding-form) "Binding form is not vector: %s" binding-form)
  (let [[binding-form more-sym] (if (= (last (butlast binding-form)) '&)
                                  [(drop-last 2 binding-form)
                                   (doto (last binding-form)
                                     (-> symbol? (schema/assert-iae "Argument to & not a symbol: %s"
                                                                    binding-form)))]
                                  [binding-form nil])
        [bindings as-sym]       (if (= (last (butlast binding-form)) :as)
                                  [(drop-last 2 binding-form)
                                   (doto (last binding-form)
                                     (-> symbol? (schema/assert-iae "Argument to :as not a symbol: %s"
                                                                    binding-form)))]
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
   -  output-form is the final output form that executes body in the scope of the provided bindings,
   -  bound-syms is the set of symbols bound, which must be unique, and
   -  input-schema is the input schema imposed on the map-form by the binding."
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

;;; Positional fnks

(def +none+
  "A sentinel value used to indicate a non-provided optional value in a positional form."
  ::none)

(defn positional-arg-bind-form
  "Generate the binding form to handle a single element of the fnk binding
   vector when called positionally"
  [body-expr binding]
  (cond (symbol? binding)
        body-expr

        (map? binding)
        (let [[bs ov] (first binding)]
          `(let [~bs (if (identical? +none+ ~bs) ~ov ~bs)]
             ~body-expr))

        (vector? binding)
        (let [[k & more] binding]
          (first (letk* (vec more) (symbol (name k)) [body-expr])))

        :else (throw (IllegalArgumentException. (format "bad binding: %s" binding)))))


(defn positional-info
  "If fnk has a positional function implementation, return the pair
   [positional-fn positional-arg-ks] such that if positional-arg-ks is [:a :b :c],
   calling (positional-fn a b c) is equivalent to calling (fnk {:a a :b b :c c}),
   but faster.  Optional values to fnk can be simulated by passing +none+ as the
   value, i.e., (positional-fn +none+ b +none) is like (fnk {:b b})."
  [fnk]
  (get (meta fnk) ::positional-info))

(defn efficient-call-forms
  "Get [f arg-forms] that can be used to call a fnk most efficiently, using the
   positional version if available, or otherwise the raw fnk.  arg-form-map
   is a map from keywords representing arguments to fnk to *forms* that evaluate
   to the corresponding arguments.

   The basic idea is that (eval (cons f arg-forms)) would yield code for an
   efficient call to fnk.  However, this form is not returned directly, because
   in most cases the literal function f cannot be directly evaluated due to
   a quirk in Clojure -- e.g., try (eval `(~(let [x 1] (fn [y] (+ y x))) 2)).

   For examples of how this is used, see 'positional-fn' below, or the positional
   compilation in plumbing.graph.positional."
  [fnk arg-form-map]
  (if-let [[positional-f positional-args] (positional-info fnk)]
    (do (schema/assert-iae (set/superset? (set (keys arg-form-map))
                                          (set positional-args))
                           "Trying to call fn that takes args %s with args %s"
                           positional-args arg-form-map)
        [positional-f (map arg-form-map positional-args)])
    [fnk [`(into {} (remove #(identical? +none+ (second %)) ~arg-form-map))]]))

(defn positional-fn
  "Given argument order in arg-ks, produce an ordinary fn that can be called
   with arguments in this order. arg-ks must include all required keys of fnk.

   Example: (= ((positional-fn fnk 1 2) [:b :a]) (fnk {:a 2 :b 1}))

   Can only be applied to fnks with a positional form, and should yield
   a function that is significantly faster than calling fnk directly by
   avoiding the construction and destructuring of the outer map.  Uses 'eval',
   so while the produced function is fast, the actual production of the
   positional-fn is generally relatively slow."
  [fnk arg-ks]
  (schema/assert-iae (apply distinct? ::dummy arg-ks)
                     "Invalid positional args %s contain duplicates" arg-ks)
  (schema/assert-iae (positional-info fnk)
                     "Called positional-fn on a fnk without a positional form")
  (let [input-schema (pfnk/input-schema fnk)
        missing-args (remove (set arg-ks) (keys input-schema))
        [missing-opt missing-req] ((juxt filter remove) #(false? (input-schema %)) missing-args)
        extra-args (remove (partial contains? input-schema) arg-ks)
        arg-syms (mapv (comp symbol name) arg-ks)
        [pos-fn pos-args] (efficient-call-forms
                           fnk
                           (merge (zipmap arg-ks arg-syms)
                                  (zipmap missing-opt (repeat +none+))))]
    (schema/assert-iae (and (empty? missing-req) (empty? extra-args))
                       "Invalid positional args %s missing %s, with extra %s"
                       arg-ks missing-req extra-args)
    ((eval `(fn [f#] (fn ~arg-syms (f# ~@pos-args))))
     pos-fn)))

(defn positional-fnk-form
  "Takes an optional name, io-schemata, and a positional fn body that can
   reference the symbol versions of keywords in (first io-schemata), and
   produces a form generating a IFn/PFnk that can be called as a keyword function,
   and has metadata containing the positional function for efficient compilation
   as described in 'efficient-call-forms' and 'positional-fn' above, with
   argument order the same as in (first io-schemata).   Example:

   (def f (eval (i/positional-fnk-form 'foo [{:x true :y false} true]
                   [`(+ ~'x (if (= ~'y i/+none+) 5 ~'y))])))

   (= [6 3] [(f {:x 1}) (f {:x 1 :y 2})])
   (= [6 3] [((i/positional-fn f [:x]) 1) ((i/positional-fn f [:y :x]) 2 1)])"
  [name? [input-schema :as io-schemata] body]
  (let [[opt-ks req-ks] ((juxt filter remove) #(false? (input-schema %)) (keys input-schema))
        pos-args (mapv (comp symbol name) (keys input-schema))]
    `(let [pos-fn# (fn ~@(when name? [(symbol (str name? "-positional"))])
                     ~pos-args
                     ~@body)]
       (vary-meta (pfnk/fn->fnk
                   (fn [m#]
                     (plumbing.core/letk [~(into (mapv (comp symbol name) req-ks)
                                                 (mapv #(hash-map (symbol (name %)) +none+) opt-ks)) m#]
                       (pos-fn# ~@pos-args)))
                   ~io-schemata)
                  assoc ::positional-info [pos-fn# ~(mapv keyword pos-args)]))))

;;; Generating fnk bodies

(defn fnk-form
  "Take an optional name, binding form, and body for a fnk, and make an
   IFn/PFnk for these arguments.

   For efficiency, two different methods of generating fnk bodies are
   used.  If the fnk takes a fixed set of arguments (i.e., no & or
   :as), then a 'positional' version of the fnk that is called like an
   ordinary Clojure fn (e.g., (f a b) rather than (f {:a a :b b}) is
   generated as an implementation detail, and stored in metadata of
   the actual keyword fnk (which is just a thin wrapper around the
   positional version).  If '& or :as are used, no such positional
   function is generated."
  [name? bind body]
  (let [map-sym (gensym)
        [fnk-body _ input-schema] (letk* bind map-sym body)
        schema [input-schema
                (schema/make-output-schema (last body)
                                           (eval (:output-schema (meta bind) true)))]]
    (if (not-any? #{'& :as} bind) ;; If we can make a positional fnk form, do it.
      (positional-fnk-form
       name?
       schema
       [(reduce
         positional-arg-bind-form
         `(do ~@body)
         bind)])
      (pfnk/fn->fnk
       `(fn ~@(when name? [name?])
          [~map-sym]
          (schema/assert-iae (map? ~map-sym) "fnk called on non-map: %s" ~map-sym)
          ~fnk-body)
       schema))))
