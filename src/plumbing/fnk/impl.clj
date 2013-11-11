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
   [schema.core :as s]
   [schema.macros :as schema-macros]
   [plumbing.fnk.schema :as schema]
   [plumbing.fnk.pfnk :as pfnk]))

;; TODO: allow schemas on entire bindings (e.g., this is a defrecord)
;; - but this poses problems since these schemas are unevaluated.
;; - can do it, but probably have to fork into internal and external schemas.
;; TODO: maybe ^:strict metadata to turn off accepting additional keys?

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

(defn k->sym
  "Make a keyword into a symbol"
  [k]
  (symbol (name k)))

;;; Parsing new fnk binding style

(declare letk-input-schema-and-body-form)

(defn- any-schema? [s]
  (= `s/Any s))

(defn- assert-unschematized [x]
  (let [schema (schema-macros/extract-schema-form x)]
    (schema/assert-iae (any-schema? schema) "Schema metadata not allowed on %s :- %s" x schema)))

(defn ensure-schema-metadata [env x]
  (schema-macros/normalized-metadata env x nil))

(defn schema-override [sym schema]
  (vary-meta sym assoc :schema schema))

(defn- process-schematized-map
  "Take an optional binding map like {a 2} or {a :- Number 2} and convert the schema
   information to canonical metadata, if present."
  [env binding]
  (case (count binding)
    1 (let [[sym v] (first binding)]
        {(ensure-schema-metadata env sym) v})

    2 (let [[[[sym _]] [[schema v]]] ((juxt filter remove) #(= (val %) :-) binding)]
        (schema/assert-iae (and (symbol? sym) schema)
                           "Bad schematized binding %s: should look like {a :- Number 2}" binding)
        {(schema-macros/normalized-metadata env sym schema) v})))

;; TODO: unify this with positional version.
(defn letk-arg-bind-sym-and-body-form
  "Given a single element of a single letk binding form and a current body form, return
   a map {:schema-entry :body-form} where schema-entry is a tuple
   [bound-key schema external-schema?], and body-form wraps body with destructuring
   for this binding as necessary."
  [env map-sym binding key-path body-form]
  (cond (symbol? binding)
        {:schema-entry [(keyword binding) (schema-macros/extract-schema-form binding)]
         :body-form `(let [~binding (safe-get ~map-sym ~(keyword binding) ~key-path)]
                       ~body-form)}

        (map? binding)
        (let [schema-fixed-binding (process-schematized-map env binding)
              [bound-sym opt-val-expr] (first schema-fixed-binding)
              bound-key (keyword bound-sym)]
          (assert-unschematized binding)
          (schema/assert-iae (= 1 (count schema-fixed-binding))
                             "optional binding has more than 1 entry: %s" schema-fixed-binding)
          {:schema-entry [(s/optional-key bound-key) (schema-macros/extract-schema-form bound-sym)]
           :body-form `(let [~bound-sym (get ~map-sym ~bound-key ~opt-val-expr)]
                         ~body-form)})

        (vector? binding)
        (let [[bound-key & more] binding
              {inner-input-schema :input-schema
               inner-external-input-schema :external-input-schema
               inner-map-sym :map-sym
               inner-body-form :body-form} (letk-input-schema-and-body-form
                                            env
                                            (with-meta (vec more) (meta binding))
                                            (conj key-path bound-key)
                                            body-form)]
          (schema/assert-iae
           (keyword? bound-key)
           "First element to nested binding not a keyword: %s" bound-key)
          {:schema-entry [bound-key inner-input-schema inner-external-input-schema]
           :body-form `(let [~inner-map-sym (safe-get ~map-sym ~bound-key ~key-path)]
                         ~inner-body-form)})

        :else (throw (IllegalArgumentException. (format "bad binding: %s" binding)))))

(defn- extract-special-arg
  "Extract a trailing & sym or :as sym, possibly with schema metadata. Returns
   [more-bindings extracted-symbol]"
  [env special-arg-signifier binding-form]
  (let [[more-bindings special-binding] (split-with #(not= % special-arg-signifier) binding-form)]
    [more-bindings
     (when (seq special-binding)
       (let [[sym extra-garbage] (schema-macros/extract-arrow-schematized-element env (next special-binding))]
         (schema/assert-iae (symbol? sym) "Argument to %s not a symbol: %s" special-arg-signifier binding-form)
         (schema/assert-iae (empty? extra-garbage) "Got illegal special binding: %s" special-binding )
         sym))]))

(defn letk-input-schema-and-body-form
  "Given a single letk binding form, value form, key path, and body
   form, return a map {:input-schema :external-input-schema :map-sym :body-form}
   where input-schema is the schema imposed by binding-form, external-input-schema
   is like input-schema but includes user overrides for binding vectors,
   map-sym is the symbol which it expects the bound value to be bound to,
   and body-form wraps body in the bindings from binding-form from map-sym."
  [env binding-form key-path body-form]
  (schema/assert-iae (vector? binding-form) "Binding form is not vector: %s" binding-form)
  (let [binding-schema (schema-macros/extract-schema-form binding-form)
        [binding-form more-sym] (extract-special-arg env '& binding-form)
        [bindings as-sym]       (extract-special-arg env :as binding-form)
        as-sym (or as-sym (ensure-schema-metadata env (gensym "map")))
        [input-schema-elts
         external-input-schema-elts
         bound-body-form] (reduce
                           (fn [[input-schema-elts external-input-schema-elts cur-body] binding]
                             (let [{:keys [schema-entry body-form]}
                                   (letk-arg-bind-sym-and-body-form
                                    env as-sym binding key-path cur-body)
                                   [bound-key input-schema external-input-schema] schema-entry]
                               [(conj input-schema-elts [bound-key input-schema])
                                (conj external-input-schema-elts
                                      [bound-key (or external-input-schema input-schema)])
                                body-form]))
                           [[] [] body-form]
                           (reverse
                            (schema-macros/process-arrow-schematized-args
                             env bindings)))
        explicit-schema-keys (->> input-schema-elts
                                  (map first)
                                  (filter s/specific-key?)
                                  (map s/explicit-schema-key))
        final-body-form (if more-sym
                          `(let [~more-sym (dissoc ~as-sym ~@explicit-schema-keys)]
                             ~bound-body-form)
                          bound-body-form)
        make-input-schema (fn [elts]
                            (if-not (or more-sym (seq elts) (empty? key-path))
                              `s/Any ;; allow [:a :as :b] inner bindings without requiring :a be a map
                              (merge
                               (into {} elts)
                               (let [more-schema (if more-sym
                                                   (schema-macros/extract-schema-form more-sym)
                                                   `s/Any)]
                                 (if (any-schema? more-schema)
                                   {`s/Keyword `s/Any}
                                   (do (schema/assert-iae (map? more-schema)
                                                          "& %s schema must be a map" more-sym)
                                       more-schema))))))]
    (when as-sym (assert-unschematized as-sym))
    (schema/assert-iae (not (some #{'&} (map first input-schema-elts))) "Cannot bind to &")
    (assert-distinct (concat (map k->sym explicit-schema-keys)
                             (remove nil? [more-sym as-sym])))
    {:input-schema (make-input-schema input-schema-elts)
     :external-input-schema (if-not (any-schema? binding-schema)
                              binding-schema
                              (make-input-schema external-input-schema-elts))
     :map-sym as-sym
     :body-form final-body-form}))

;;; Positional fnks

(def +none+
  "A sentinel value used to indicate a non-provided optional value in a positional form."
  ::none)

(defn positional-arg-bind-sym-and-body
  "Given a single element of a fnk binding form and a current body form, return
   a pair [[k bind-sym] new-body-form] where bind-sym is a suitable symbol to bind
   to k in the fnk arglist (including tag metadata if applicable) and new-body-form
   is wrapped with destructuring for this binding as necessary."
  [env binding body-form]
  (cond (symbol? binding)
        (let [bind-sym (gensym (name binding))]
          [[(keyword binding) bind-sym]
           `(let [~binding ~bind-sym] ~body-form)])

        (map? binding)
        (let [[bs ov] (first (process-schematized-map env binding))
              bind-sym (gensym (name bs))]
          [[(keyword bs) bind-sym]
           `(let [~bs (if (identical? +none+ ~bind-sym) ~ov ~bind-sym)]
              ~body-form)])

        (vector? binding)
        (let [[k & more] binding
              {:keys [map-sym body-form]} (letk-input-schema-and-body-form
                                           env (ensure-schema-metadata env (vec more)) [k]
                                           body-form)]
          [[k
            (with-meta map-sym
              (if (= (last (butlast binding)) :as) (meta (last binding)) {}))]
           body-form])

        :else (throw (IllegalArgumentException. (format "bad binding: %s" binding)))))

(defn positional-arg-bind-syms-and-body
  "Given a fnk binding form and body form, return a pair
   [bind-sym-map new-body-form] where bind-sym-map is a map from keyword args
   to binding symbols and and new-body-form wraps body to do any extra processing
   of nested or optional bindings above and beyond the bindings achieved by
   bind-sym-vector."
  [env bind body-form]
  (reduce
   (fn [[cur-bind cur-body] binding]
     (let [[bind-sym new-body] (positional-arg-bind-sym-and-body env binding cur-body)]
       [(conj cur-bind bind-sym) new-body]))
   [{} body-form]
   (reverse (schema-macros/process-arrow-schematized-args env bind))))


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

   Example: (= ((positional-fn a-fnk [:b :a]) [1 2]) (a-fnk {:a 2 :b 1}))

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
        [missing-req missing-opt] (schema/split-schema-keys
                                   (apply dissoc (schema/explicit-schema-key-map input-schema)
                                          (set arg-ks)))
        extra-args (remove (partial schema/possibly-contains? input-schema) arg-ks)
        arg-syms (mapv k->sym arg-ks)
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
  "Takes an optional name, input schema and a positional fn body that can
   reference the symbol versions of keywords in input-schema, and
   produces a form generating a IFn/PFnk that can be called as a keyword function,
   and has metadata containing the positional function for efficient compilation
   as described in 'efficient-call-forms' and 'positional-fn' above, with
   argument order the same as in input-schema by default.  An explicit
   arg-sym-map from keywords to symbols can also be passed to provide explicit
   symbols for each arg, and propagate argument tag megadata into the fn. Example:

   (def f (eval (i/positional-fnk-form 'foo [{:x true :y false} true]
                   [`(+ ~'x (if (= ~'y i/+none+) 5 ~'y))])))

   (= [6 3] [(f {:x 1}) (f {:x 1 :y 2})])
   (= [6 3] [((i/positional-fn f [:x]) 1) ((i/positional-fn f [:y :x]) 2 1)])"
  ([fn-name input-schema body]
     (positional-fnk-form
      fn-name
      input-schema
      input-schema
      (into {} (for [k (keys (schema/explicit-schema-key-map input-schema))] [k (k->sym k)]))
      body))

  ([fn-name input-schema external-input-schema arg-sym-map body]
     (let [[req-ks opt-ks] (-> input-schema schema/explicit-schema-key-map schema/split-schema-keys)
           explicit-schema-keys (vec (keys (schema/explicit-schema-key-map input-schema)))
           pos-args (mapv #(safe-get arg-sym-map % []) explicit-schema-keys)]
       `(let [pos-fn# (fn ~(symbol (str fn-name "-positional"))
                        ~pos-args
                        ~@body)]
          (vary-meta (schema-macros/fn
                       ~fn-name
                       [m# :- ~external-input-schema]
                       (plumbing.core/letk [~(into (mapv k->sym req-ks)
                                                   (mapv (fn [k] {(k->sym k) +none+}) opt-ks))
                                            m#]
                         (pos-fn# ~@(mapv k->sym explicit-schema-keys))))
                     assoc ::positional-info [pos-fn# ~explicit-schema-keys])))))

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
  [env name? bind body]
  (let [{:keys [map-sym body-form input-schema external-input-schema]}
        (letk-input-schema-and-body-form env bind [] `(do ~@body))

        explicit-output-schema (if name? (schema-macros/extract-schema-form name?) `s/Any)
        output-schema (if (any-schema? explicit-output-schema)
                        (schema/guess-expr-output-schema (last body))
                        explicit-output-schema)
        fn-name (vary-meta (or name? (gensym "fnk")) assoc :schema output-schema)]
    (if (not-any? #{'& :as} bind) ;; If we can make a positional fnk form, do it.
      (let [[bind-sym-map bound-body] (positional-arg-bind-syms-and-body env bind `(do ~@body))]
        (positional-fnk-form fn-name input-schema external-input-schema bind-sym-map [bound-body]))
      `(schema-macros/fn
         ~fn-name
         [~(schema-override map-sym external-input-schema)]
         (schema/assert-iae (map? ~map-sym) "fnk called on non-map: %s" ~map-sym)
         ~body-form))))
