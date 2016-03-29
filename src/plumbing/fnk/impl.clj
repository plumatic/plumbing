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
  (:require
   [clojure.set :as set]
   [schema.core :as s]
   [schema.macros :as schema-macros]
   [plumbing.fnk.schema :as schema]
   [plumbing.fnk.pfnk :as pfnk]))

;; TODO: maybe ^:strict metadata to turn off accepting additional keys?

;;; Helpers

(defn name-sym
  "Returns symbol of x's name.
   Converts a keyword/string to symbol, or removes namespace (if any) of symbol"
  [x]
  (with-meta (symbol (name x)) (meta x)))

(defn qualified-sym
  "Returns qualified symbol of x, an instance of Named"
  [x]
  (symbol (namespace x) (name x)))

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
         :body-form `(let [~(name-sym binding) (schema/safe-get ~map-sym ~(keyword binding) ~key-path)]
                       ~body-form)}

        (map? binding)
        (let [schema-fixed-binding (process-schematized-map env binding)
              [bound-sym opt-val-expr] (first schema-fixed-binding)
              bound-key (keyword bound-sym)]
          (assert-unschematized binding)
          (schema/assert-iae (= 1 (count schema-fixed-binding))
                             "optional binding has more than 1 entry: %s" schema-fixed-binding)
          {:schema-entry [`(with-meta (s/optional-key ~bound-key) {:default '~opt-val-expr}) (schema-macros/extract-schema-form bound-sym)]
           :body-form `(let [~(name-sym bound-sym) (get ~map-sym ~bound-key ~opt-val-expr)]
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
           :body-form `(let [~inner-map-sym (schema/safe-get ~map-sym ~bound-key ~key-path)]
                         ~inner-body-form)})

        :else (throw (IllegalArgumentException. (format "bad binding: %s" binding)))))

(defn- extract-special-args
  "Extract trailing & sym and :as sym, possibly with schema metadata. Returns
  [more-bindings special-args-map] where special-args-map is a map from each
  special symbol found to the symbol that was found."
  [env special-arg-signifier-set binding-form]
  {:pre [(set? special-arg-signifier-set)]}
  (let [[more-bindings special-bindings] (split-with (complement special-arg-signifier-set) binding-form)]
    (loop [special-args-map {}
           special-arg-set special-arg-signifier-set
           [arg-signifier & other-bindings :as special-bindings] special-bindings]
      (if-not (seq special-bindings)
        [more-bindings special-args-map]
        (do
          (schema/assert-iae (special-arg-set arg-signifier) "Got illegal special arg: " arg-signifier)
          (let [[sym remaining-bindings] (schema-macros/extract-arrow-schematized-element env other-bindings)]
            (schema/assert-iae (symbol? sym) "Argument to %s not a symbol: %s" arg-signifier binding-form)
            (recur (assoc special-args-map arg-signifier sym)
                   (disj special-arg-set arg-signifier)
                   remaining-bindings)))))))

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
        [bindings {more-sym '& as-sym :as}] (extract-special-args env #{'& :as} binding-form)
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
        explicit-schema-keys (keep (comp first schema/unwrap-schema-form-key first)
                                   input-schema-elts)
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
    (schema/assert-distinct (concat (map name-sym explicit-schema-keys)
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
           `(let [~(name-sym binding) ~bind-sym] ~body-form)])

        (map? binding)
        (let [[bs ov] (first (process-schematized-map env binding))
              bind-sym (gensym (name bs))]
          [[(keyword bs) bind-sym]
           `(let [~(name-sym bs) (if (identical? +none+ ~bind-sym) ~ov ~bind-sym)]
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
        arg-syms (mapv name-sym arg-ks)
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
  "Takes an optional name, input schema, seq of ordered [key optional?] pairs,
   an arg-sym-map from these keywords to symbols, and and a positional fn body
   that can reference these symbols.
   Produces a form generating a IFn/PFnk that can be called as a keyword function,
   and has metadata containing the positional function for efficient compilation
   as described in 'efficient-call-forms' and 'positional-fn' above, with
   argument order the same as in input-schema by default.   Example:

   (def f (eval (i/positional-fnk-form 'foo {:x s/Any (s/optional-key :y) s/Any}
                   [`(+ ~'x (if (= ~'y i/+none+) 5 ~'y))])))

   (= [6 3] [(f {:x 1}) (f {:x 1 :y 2})])
   (= [6 3] [((i/positional-fn f [:x]) 1) ((i/positional-fn f [:y :x]) 2 1)])."
  [fn-name external-input-schema ordered-ks->opt arg-sym-map body form]
  (let [[req-ks opt-ks] (schema/split-schema-keys (into {} ordered-ks->opt))
        explicit-schema-keys (mapv first ordered-ks->opt)
        pos-args (mapv #(do (schema-macros/assert! (contains? arg-sym-map %))
                            (arg-sym-map %))
                       explicit-schema-keys)]
    `(let [pos-fn# (fn ~(symbol (str fn-name "-positional"))
                     ~pos-args
                     ~@body)]
       (vary-meta
        (s/fn
          ~fn-name
          [m# :- ~external-input-schema]
          (plumbing.core/letk [~(into (mapv qualified-sym req-ks)
                                      (mapv (fn [k] {(qualified-sym k) +none+}) opt-ks))
                               m#]
            (pos-fn# ~@(mapv name-sym explicit-schema-keys))))
        merge
        (assoc ~(meta form) ::positional-info [pos-fn# ~explicit-schema-keys])))))

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
  [env name? bind body form]
  (let [{:keys [map-sym body-form input-schema external-input-schema]}
        (letk-input-schema-and-body-form env bind [] `(do ~@body))

        explicit-output-schema (if name? (schema-macros/extract-schema-form name?) `s/Any)
        output-schema (if (any-schema? explicit-output-schema)
                        (schema/guess-expr-output-schema (last body))
                        explicit-output-schema)
        fn-name (vary-meta (or name? (gensym "fnk")) assoc :schema output-schema)]
    ((fn [fn-form]
       `(vary-meta ~fn-form assoc :name '~name?))
     (if (and (not (schema-macros/cljs-env? env))
              (not-any? #{'& :as} bind)) ;; If we can make a positional fnk form, do it.
       (let [[bind-sym-map bound-body] (positional-arg-bind-syms-and-body env bind `(do ~@body))]
         (positional-fnk-form
          fn-name
          external-input-schema
          (vec (schema/explicit-schema-key-map input-schema))
          bind-sym-map
          [bound-body]
          form))
       (with-meta `(s/fn ~fn-name
                     [~(schema-override map-sym external-input-schema)]
                     (schema/assert-iae (map? ~map-sym) "fnk called on non-map: %s" ~map-sym)
                     ~body-form)
         (meta form))))))
