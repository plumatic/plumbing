(ns plumbing.fnk.impl
  "Core utilities for parsing our 'fnk'-style binding syntax and generating bodies.
   Documented and tested through the actual 'letk','fnk', and 'defnk' macros in plumbing.core."
  (require
   [clojure.set :as set]
   [plumbing.fnk.schema :as schema]
   [plumbing.fnk.pfnk :as pfnk]))


;;; Helpers

(def +none+
  "A sentinel value used to indicate a non-provided optional value in a positional form."
  ::none)

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

(defn keyword-fnk-form
  [name? map-sym final-body]
  `(fn ~name?
     [~map-sym]
     (schema/assert-iae (map? ~map-sym) "fnk called on non-map: %s" ~map-sym)
     ~final-body))

(defn extract-positional-args
  "If possible, generate the argument vector for the positional-fn, which has
  one symbol for each top-level key in the fnk binding vector, in order."
  [bind]
  (when (not-any? #(contains? #{'& :as} %) bind)
    (for [arg bind]
      (cond (symbol? arg) arg
            (map? arg) (ffirst arg)
            ;; Returns a keyword if we'll need to gensym for bindings like in
            ;; [:y z] where we don't want to bind anything to y.
            (vector? arg) (first arg)
            :else (throw (RuntimeException. (format "bad binding: %s" arg)))))))

(defn gensym-positional-arg-if-needed
  "If a positional argument is a keyword, that means we should gensym a symbol
  for it to avoid collisions."
  [arg]
  (if (keyword? arg)
    (gensym (name arg))
    arg))

;; handle nested bindings as usual, top-level is positional.
(defn positional-arg-bind-form
  "Generate the binding form to handle a single element of the fnk binding
   vector when called positionally"
  [body-expr [bs binding]]
  (cond (symbol? binding)
        (do (assert (= binding bs)) body-expr)

        (map? binding)
        (let [[bs* ov] (first binding)]
          (assert (= bs* bs))
          `(let [~bs (if (identical? +none+ ~bs) ~ov ~bs)]
             ~body-expr))

        (vector? binding)
        (let [[k & more] binding]
          (first (letk* (vec more) bs [body-expr])))

        :else (throw (RuntimeException. (format "bad binding: %s" binding)))))

(defn positional-fnk-form
  "Generate the form for a positional fn version of a fnk."
  [name? bind body]
  `(fn ~@(when name? [(symbol (str name? "-positional"))])
     ~(mapv first bind)
     ~(reduce
        positional-arg-bind-form
        `(do ~@body)
        bind)))

(defn fn->positional-fnk
  "Generate a fnk that has a positional form for calling efficiently."
  [f [input-schema output-schema :as io] positional-f positional-args]
  (vary-meta (pfnk/fn->fnk f io)
             assoc ::positional-info [positional-f positional-args]))

(defn positional-info [f]
  (get (meta f) ::positional-info))

(defn efficient-call-forms
  "Get [f arg-forms] needed to call a function most efficiently. This returns a
  vector, not a list, to avoid accidental use in eval. The arg-forms need to be
  evaluated, but in general functions (closures) can't be eval'ed. E.g. try:
    (eval `(~(let [x 1] (fn [y] (+ y x))) 2))"
  [fnk arg-form-map]
  (if-let [[positional-f positional-args] (positional-info fnk)]
    (do
      (schema/assert-iae (set/superset? (set (keys arg-form-map))
                                        (set positional-args))
                         "Trying to call fn that takes args %s with args %s"
                         positional-args arg-form-map)
      [positional-f (map arg-form-map positional-args)])
    [fnk [`(into {} (remove #(identical? +none+ (second %)) ~arg-form-map))]]))

(defn positional-fn
  "Given argument order in arg-ks, produce an ordinary fn that can be called
   with arguments in this order.  arg-ks must include all required keys of fnk.
   Can only be applied to fnks with a positional form."
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

(defn fnk*
  "Take an optional name, binding form, and body for a fnk, and make an IFn/PFnk for these arguments"
  [name? bind body]
  (let [map-sym (gensym)
        [final-body _ input-schema] (letk* bind map-sym body)
        name? (or name? (gensym "fnk"))
        keyword-form (keyword-fnk-form name? map-sym final-body)
        schema [input-schema
                (schema/make-output-schema (last body)
                                           (eval (:output-schema (meta bind) true)))]]
    (if-let [positional-args (extract-positional-args bind)]
      ;; If we can make a positional form, do so.
      (fn->positional-fnk
        keyword-form
        schema
        (positional-fnk-form name?
                             (map vector
                                  (map gensym-positional-arg-if-needed positional-args)
                                  bind)
                             body)
        (mapv keyword positional-args))
      (pfnk/fn->fnk
        keyword-form
        schema))))
