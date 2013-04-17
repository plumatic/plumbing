(ns plumbing.fnk.positional
  "Experimental WIP on more efficient eager compilations.
   To be merged into fnk/eager-compile."
  (:use plumbing.core)
  (:require 
   [plumbing.fnk.schema :as schema]
   [plumbing.fnk.pfnk :as pfnk]
   [plumbing.fnk.impl :as fnk-impl]
   [plumbing.graph :as graph]))

;; TODO: allow qualified keyword binding in graph?
;; https://github.com/Prismatic/plumbing/issues/6
;; TODO: generate records for output maps?
;; TODO: handle &as etc.
;; TODO: handle primitive returns (or interfaces)
;; TODO: generate .invokePrim for primitive hinted fns.

(def +none+ ::none)

(defn positional-fn 
  "The form that makes up the body of this fnk, or nil if body form unknown.
   Currently fnks inlined in this way cannot be closures."
  [f]
  (safe-get (meta f) ::positional-fn))

(defn positional-args
  "The argument vector for positional-fn, which has one symbol for each
   top-level key in the fnk binding vector, in order."
  [f]
  (safe-get (meta f) ::positional-args))

;; handle nested bindings as usual, top-level is positional.

(defn positional-bind-form 
  "Generate the binding form to handle a single element of the fnk binding
   vector when called positionally"
  [body-expr [bs binding]]
  (cond (symbol? binding)
        (do (assert (= binding bs)) body-expr)
        
        (map? binding)
        (let [[bs* ov] (first binding)]          
          (assert (= bs* bs))
          `(let [~bs (if (identical? ::none ~bs) ~ov ~bs)]
             ~body-expr))
        
        (vector? binding)
        (let [[k & more] binding]
          (assert (= k (keyword (name bs))))
          `(letk [~(vec more) ~(symbol (name k))] ~body-expr))
        
        :else (throw (RuntimeException. (format "bad binding: %s" binding)))))

(defn extract-positional-args 
  "Generate the argument vector for the positional-fn, which has one symbol
   for each top-level key in the fnk binding vector, in order."
  [bind]
  (for [arg bind]
    (cond (symbol? arg) arg          
          (map? arg) (ffirst arg)
          (vector? arg) (gensym (name (first arg)))
          :else (throw (RuntimeException. (format "bad binding: %s" arg))))))

(defn positional-fn-form 
  "Generate the form for a positional fn version of a fnk"
  [f name? bind body]
  `(fn ~@(when name? [name?]) ~(mapv first bind)
     ~(reduce 
       positional-bind-form
       `(do ~@body)
       bind)))

(defn positional 
  "Elaborate a fnk by adding a positional arg vector and function"
  [f name? bind body]
  (let [arg-syms (extract-positional-args bind)]
    (vary-meta f assoc 
               ::positional-args (mapv #(list 'quote %) arg-syms)
               ::positional-fn (positional-fn-form f name? (map vector arg-syms bind) body))))

(defn positional-fnk* 
  "Like fnk*, but also generate a positional fn version.  Will eventually replace fn*."
  [name? bind body]
  (positional
   (fnk-impl/fnk* name? bind body)
   name? bind body))

(defmacro fnk-positional
  "Like fnk, but also generate a positional fn version for efficient compilation.
   Will eventually replace fnk"
  [& args]
  (let [[name? [bind & body]] (if (symbol? (first args))
                                [(first args) (next args)]
                                [nil args])]
    (positional-fnk* name? bind body)))

(defmacro defnk-positional
  "Like defnk, but also generate a positional fn version for efficient compilation.
   Will eventually replace defnk"
  [name & args]
  (let [take-if (fn [p s] (if (p (first s)) [(first s) (next s)] [nil s]))
        [docstring? args] (take-if string? args)
        [attr-map? [bind & body]] (take-if map? args)]
    (schema/assert-iae (symbol? name) "Name for defnk is not a symbol: %s" name)
    (let [f (positional-fnk* name bind body)]
      `(def ~(with-meta name (assoc-when (or attr-map? {}) :doc docstring?))
         ~f))))

(defn generate-positional-body-expr 
  "Generate the body expression for a graph with positional calls to node functions"
  [g out-k]
  `(let ~(->> (for [[k f] g]
                [(symbol (name k))
                 `(~(positional-fn f) ~@(positional-args f))])
              (apply concat)
              vec)
     ~(if out-k
       (symbol (name out-k))
       (for-map [k (keys g)]
          k (symbol (name k))))))

(defn normalize-positional-arg-ks 
  "Take a graph input schema and provided seq of arg ks, validate that it contains
   all required keys and only valid input keys, and return a pair
   [opt-tail-count more-opt-arg-ks] where opt-tail-count is the number of 
   keys at the suffix of arg-ks that are optional (and can be omitted in the positional
   form) and more-opt-arg-ks is the set of optional keys that are not in arg-ks
   and should be bound to +none+ directly."
  [input-schema arg-ks]
  (assert (apply distinct? ::dummy arg-ks))
  (let [[opt-ks extra-schema] 
        (reduce
         (fn [[opt-ks extra-schema] k]
           (let [schema-val (safe-get extra-schema k)]
             [(if (false? schema-val) (conj opt-ks k) opt-ks)
              (dissoc extra-schema k)]))
         [#{} input-schema]
         arg-ks)]    
    [(->> arg-ks reverse (take-while opt-ks) count)
     (set (for [[k schema-val] extra-schema]
            (do (schema/assert-iae (false? schema-val) "Missing required arg key %s" k)
                k)))]))

(defn variadic-fn-form 
  "Generate a function expression where the full arglist is arg-syms, and a suffix of
   arglist can be omitted with the default values given by opt-arg-vals."
  [name arg-syms opt-arg-vals body]  
  `(fn ~name
     ~@(concat      
        (for [opt-arg-vals (take-while identity (iterate next (seq opt-arg-vals)))]
          (let [arg-syms (drop-last (count opt-arg-vals) arg-syms)]
            `(~(vec arg-syms) (~name ~@arg-syms ~@opt-arg-vals))))
       [`(~(vec arg-syms) ~@body)])))

;; This uses of fn->fnk, graph/->graph etc are off since they pretend a positional fn 
;; is a fnk.  This will be unified later by making everything a fnk and separating 
;; out the positional-call macro/fn
(defn positional-flat-compile
  "Positional compile for a flat (non-nested) graph."
  [g arg-ks & [out-k]]
  (let [g (graph/->graph g)
        [opt-tail-count more-opt-arg-ks] (normalize-positional-arg-ks (pfnk/input-schema g) arg-ks)
        body (generate-positional-body-expr g out-k)]
    (pfnk/fn->fnk
     (eval `(let ~(vec (interleave (map #(symbol (name %)) more-opt-arg-ks)
                                   (repeat +none+)))
             ~(variadic-fn-form 
              (gensym "positional-graph")
              (map #(symbol (name %)) arg-ks)
              (repeat opt-tail-count +none+)
              [body])))
     (pfnk/io-schemata g))))

(defn positional-compile
  "An efficient eager compilation for a graph where all leaf fnks are positional-fnks,
   which generates an ordinary fn that takes args from arg-ks in order (where suffixes 
   of optional args can be omitted), and returns a map of the outputs as usual.
   If out-k is passed, the output is the single node value referred to by out-k,
   which prevents the generation of an output map (for efficiency).
   Currently :as and & in node bindings is not supported, and nested subgraphs dont
   work either."
  [g arg-ks & [out-k]]
  (if (fn? g)
    g
    (positional-flat-compile
     (for [[k sub-g] (graph/->graph g)]
       [k (positional-compile sub-g (vec (keys (pfnk/input-schema sub-g))))])
     arg-ks
     out-k)))





