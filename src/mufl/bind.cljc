(ns mufl.bind
  "Core bind + pattern destructuring + constraint registration for mufl.

   bind takes [env expr] and returns a new tree where:
   1. New nodes are created for the expression
   2. Constraint nodes are added
   3. All domains are propagated to fixpoint

   After every bind, the tree is maximally narrowed.

   This namespace also contains pattern destructuring functions
   (bind-pattern, bind-pattern-map, etc.) and re-exports public API from:
   - mufl.narrow  (resolve, domain-of, set-domain, propagate, apply-composite-constraint)
   - mufl.schema  (resolve-domain-schema)"
  (:refer-clojure :exclude [resolve])
  (:require [mufl.tree :as tree]
            [mufl.domain :as dom]
            [mufl.narrow :as narrow]
            [mufl.schema :as schema]))

;; ════════════════════════════════════════════════════════════════
;; Re-exports — unified facade
;; ════════════════════════════════════════════════════════════════
;;
;; bind serves as the primary API surface for callers (env, core,
;; search, show). Rather than requiring narrow, schema, and pattern
;; individually, callers use bind/ for everything. This keeps import
;; lists short and decouples callers from the internal module split.

;; From mufl.narrow
(def resolve narrow/resolve)
(def resolve-at narrow/resolve-at)
(def domain-of narrow/domain-of)
(def set-domain narrow/set-domain)
(def propagate narrow/propagate)
(def apply-composite-constraint narrow/apply-composite-constraint)
(def propagate-watchers narrow/propagate-watchers)

;; From mufl.schema
(def resolve-domain-schema schema/resolve-domain-schema)

;; ════════════════════════════════════════════════════════════════
;; Forward declarations
;; ════════════════════════════════════════════════════════════════

(declare bind apply-domain-constraint
         bind-pattern bind-pattern-map bind-pattern-vec bind-pattern-operator)

;; ════════════════════════════════════════════════════════════════
;; Recursion depth tracking
;; ════════════════════════════════════════════════════════════════

(def ^:dynamic *call-depth*
  "Current function call depth. Incremented on each :mufl-fn application."
  0)

(def max-call-depth
  "Maximum allowed call depth before throwing an error."
  100)

;; ════════════════════════════════════════════════════════════════
;; Constraint registration
;; ════════════════════════════════════════════════════════════════

(defn- gensym-constraint-name []
  (keyword (gensym "c")))

(defn add-constraint
  "Add a constraint node to the tree and register watches.
   scope-path: the path of the enclosing scope (workspace or call scope)
               where the constraint should be stored. Constraints are placed
               under [scope-path ::constraints c-name].
   Returns the updated tree (rooted) with the constraint propagated to fixpoint,
   or nil on contradiction."
  [env constraint-op refs scope-path]
  (let [env (tree/root env)
        c-name (gensym-constraint-name)
        c-path (into scope-path [::constraints c-name])
        ;; Create constraint node
        env (-> env
                (tree/ensure-path c-path)
                (tree/put c-path {:constraint constraint-op
                                  :refs refs}))
        ;; Register watches on referenced nodes
        env (reduce (fn [e ref-path]
                      ;; Follow links to get actual target
                      (let [target (narrow/resolve-at e ref-path)
                            target-path (tree/position target)]
                        (tree/put e target-path :watched-by
                                  (conj (or (:watched-by (tree/at e target-path)) #{})
                                        c-path))))
                    env refs)]
    ;; Propagate
    (narrow/propagate env [c-path])))

;; ════════════════════════════════════════════════════════════════
;; The bind function — helpers
;; ════════════════════════════════════════════════════════════════

(defn- pairs&return
  "Split a sequence into pairs and optional trailing return expression."
  [xs]
  (if (odd? (count xs))
    [(partition 2 (butlast xs)) (last xs)]
    [(partition 2 xs) nil]))

(defn- ensure-node
  "Ensure an expression is bound as a node in the tree, returning [env' path].
   - Symbols: resolve to existing node's path
   - Literals: create an anonymous child node with singleton domain
   - Expressions: create an anonymous child, bind the expr there
   env must be at a scope position (not root)."
  [env expr]
  (cond
    ;; Symbol → resolve to existing node
    (symbol? expr)
    (if-let [found (tree/find env [expr])]
      (let [target (narrow/resolve found)]
        [env (tree/position target)])
      (throw (ex-info (str "Unresolved symbol: " expr) {:sym expr})))

    ;; Literal → create anonymous child with singleton domain
    (or (number? expr) (string? expr) (boolean? expr) (nil? expr) (keyword? expr))
    (let [anon (gensym "lit")
          env' (-> (bind env anon expr)
                   (tree/put [anon] :derived true))]
      [env' [anon]])

    ;; Expression or literal collection → create anonymous child, bind the expr there
    (or (seq? expr) (vector? expr) (map? expr))
    (let [anon (gensym "expr")
          env' (-> (bind env anon expr)
                   (tree/put [anon] :derived true))]
      [env' [anon]])

    :else
    (throw (ex-info "Cannot ensure node for expression" {:expr expr}))))

(defn ensure-node-abs
  "Like ensure-node but returns absolute paths (from root).
   Works by navigating to scope, binding, then extracting the absolute path."
  [env expr]
  (let [scope-pos (tree/position env)
        [env' rel-path] (ensure-node env expr)]
    ;; If it was a resolved symbol, the path is already absolute
    ;; If it was an anonymous child, the path is relative — make it absolute
    (if (symbol? expr)
      [env' rel-path]  ;; already absolute from resolve
      [env' (into scope-pos rel-path)])))

(defn apply-domain-constraint
  "Resolve target expression and apply domain constraint."
  [env domain args]
  (when (not= 1 (count args))
    (throw (ex-info "Domain constraint takes exactly one argument" {:args args})))
  (let [[env' target-path] (ensure-node-abs env (first args))
        resolved-target (narrow/resolve-at (tree/root env') target-path)
        resolved-path (tree/position resolved-target)]
    (schema/apply-domain-constraint env' domain resolved-path)))

(defn add-constraint-and-return
  "Add constraint, propagate, then navigate back to the given position.
   scope-path is the enclosing scope for the constraint.
   If not provided, it is derived from return-to (the return-to path itself,
   which is the scope where the constraint was created).
   Returns the env at `return-to` position, or throws on contradiction."
  ([env op paths return-to]
   (add-constraint-and-return env op paths return-to return-to))
  ([env op paths return-to scope-path]
   (let [env-root (tree/root env)
         result (add-constraint env-root op paths scope-path)]
     (if result
       (or (tree/cd result return-to) result)
       (throw (ex-info "Contradiction detected" {:op op :paths paths}))))))

(defn bind-relational
  "Bind a relational constraint like (< x y).
   Arguments can be symbols or expressions.
   Creates a constraint node and propagates.
   Returns env at the same position it was called from."
  [env op args]
  (let [my-pos (tree/position env)
        ;; Ensure each arg is a node, collect paths
        [env' paths]
        (reduce (fn [[env paths] arg]
                  (let [[env' path] (ensure-node-abs env arg)]
                    [env' (conj paths path)]))
                [env []]
                args)]
    (add-constraint-and-return env' op paths my-pos)))

(defn bind-arithmetic
  "Bind an arithmetic expression like (+ x y).
   Creates a derived node with computed domain and a constraint for inverse reasoning.
   Returns env at the same position it was called from (with :domain set)."
  [env op args]
  (let [[a-expr b-expr] args
        my-pos (tree/position env)
        ;; Ensure operands are nodes
        [env' a-path] (ensure-node-abs env a-expr)
        [env'' b-path] (ensure-node-abs env' b-expr)
        a-dom (narrow/domain-of (tree/root env'') a-path)
        b-dom (narrow/domain-of (tree/root env'') b-path)
        result-dom (case op
                     :+ (dom/domain-add a-dom b-dom)
                     :- (dom/domain-sub a-dom b-dom)
                     :* (dom/domain-mul a-dom b-dom)
                     :mod (dom/domain-mod a-dom b-dom)
                     :quot (dom/domain-div a-dom b-dom))
        ;; The result lives at the current node's position
        ;; We need to set the domain on this node before going to root
        env'' (assoc env'' :domain result-dom)
        result-path (tree/position env'')]
    (add-constraint-and-return env'' op [a-path b-path result-path] my-pos)))

;; ════════════════════════════════════════════════════════════════
;; Resolution helpers — resolve expressions to tree nodes
;; ════════════════════════════════════════════════════════════════

(defn resolve-to-node
  "Resolve an expression to its tree node.
   Returns [env' resolved-node] where env' may differ from env
   when expr is a compound expression (creates a temp node via ensure-node-abs).
   For symbol expressions, env' = env.
   Handles symbols, nested expressions, and literal collections (vectors, maps).
   Throws if the expression cannot be resolved."
  [env expr op-name]
  (cond
    (symbol? expr)
    (if-let [found (tree/find env [expr])]
      [env (narrow/resolve found)]
      (throw (ex-info (str op-name ": cannot resolve: " expr)
                      {:expr expr})))

    (or (seq? expr) (vector? expr) (map? expr))
    (let [[env' path] (ensure-node-abs env expr)]
      [env' (narrow/resolve-at (tree/root env') path)])

    :else
    (throw (ex-info (str op-name ": argument must be a symbol or expression")
                    {:expr expr}))))

(defn resolve-collection
  "Resolve an expression to a collection node and validate its kind.
   expected-kind is :vector or :map.
   Returns [env' resolved-node]. Throws on resolution failure or kind mismatch."
  [env expr expected-kind op-name]
  (let [[env' node] (resolve-to-node env expr op-name)]
    (when-not node
      (throw (ex-info (str op-name ": cannot resolve: " expr)
                      {:expr expr})))
    (when-not (get node expected-kind)
      (throw (ex-info (str op-name ": not a " (name expected-kind) " node: " expr)
                      {:expr expr})))
    [env' node]))

;; ════════════════════════════════════════════════════════════════
;; Pattern destructuring — helpers (from mufl.pattern)
;; ════════════════════════════════════════════════════════════════

(defn substitute-expr
  "Walk an expression tree, replacing occurrences of symbols in the
   substitution map with their replacement expressions.
   Used by defn/defc for macro-expansion of constraint functions."
  [expr subs-map]
  (cond
    (symbol? expr)   (get subs-map expr expr)
    (seq? expr)      (apply list (map #(substitute-expr % subs-map) expr))
    (vector? expr)   (mapv #(substitute-expr % subs-map) expr)
    (map? expr)      (into {} (map (fn [[k v]] [k (substitute-expr v subs-map)])) expr)
    :else            expr))

(defn- extract-field-domain
  "Extract the domain for a specific map field key from a seed.
   Checks the seed node's tree children first (structural maps),
   then falls back to composite seed-domain info (:map-of), then dom/any."
  [env seed-sym key seed-domain]
  (or
   ;; Try tree children — the common case for structural maps.
   (when-let [found (tree/find env [seed-sym])]
     (let [seed-node (narrow/resolve found)]
       (when (:map seed-node)
         (when-let [child (tree/cd seed-node [key])]
           (let [target (narrow/resolve child)]
             (:domain target))))))
   ;; map-of composite → all values share the value domain
   (when (= :map-of (:kind seed-domain))
     (:value seed-domain))
   ;; Fallback
   dom/any))

(defn- extract-element-domain
  "Extract the domain for a vector element at index i.
   Checks composite seed-domain info first (tuple per-position, vector-of uniform),
   then falls back to tree children, then dom/any."
  [env seed-sym i seed-domain]
  (or
   ;; Tuple composite: per-position domains
   (when (= :tuple (:kind seed-domain))
     (get (:elements seed-domain) i))
   ;; Vector-of composite: uniform element domain
   (when (= :vector-of (:kind seed-domain))
     (:element seed-domain))
   ;; Try tree children — structural vector with per-index children
   (when-let [found (tree/find env [seed-sym])]
     (let [seed-node (narrow/resolve found)]
       (when (:vector seed-node)
         (when-let [child (tree/cd seed-node [i])]
           (let [target (narrow/resolve child)]
             (:domain target))))))
   ;; Fallback
   dom/any))

;; ════════════════════════════════════════════════════════════════
;; domain-node — typed child binding
;; ════════════════════════════════════════════════════════════════

(defn domain-node
  "Create a child binding with a known domain.
   Uses `bind` to create the node (which handles get/nth dispatch,
   link creation, constraint watching, etc.), then narrows its domain to
   the intersection of its current domain and `domain`.
   This is mufl's equivalent of immucode's `typed-node`."
  [env sym domain build-expr]
  (let [env' (bind env sym build-expr)]
    (if (and domain (not (dom/any? domain)))
      (let [found (tree/find env' [sym])
            _ (when-not found
                (throw (ex-info (str "domain-node: cannot find just-bound symbol: " sym)
                                {:sym sym})))
            target (narrow/resolve found)
            target-path (tree/position target)
            current (or (:domain target) dom/any)
            narrowed (dom/intersect current domain)]
        (if (dom/void? narrowed)
          (throw (ex-info "Domain contradiction in destructuring"
                          {:sym sym :domain domain :current current}))
          (-> (tree/root env')
              (tree/put target-path :domain narrowed)
              (tree/cd (tree/position env')))))
      env')))

;; ════════════════════════════════════════════════════════════════
;; bind-pattern — main entry point
;; ════════════════════════════════════════════════════════════════

(defn bind-pattern
  "Recursively destructure a pattern against a seed symbol with domain propagation.
   Returns [env' bound-syms] where bound-syms is an ordered vector of all
   symbols bound (including scaffolding gensyms before user symbols)."
  [env pattern seed-sym seed-domain]
  (cond
    ;; Symbol: trivial bind — link to seed with its domain
    (symbol? pattern)
    (let [env' (domain-node env pattern seed-domain seed-sym)]
      [env' [pattern]])

    ;; Map pattern: {:k1 pat1 :k2 pat2 . rest}
    (map? pattern)
    (bind-pattern-map env pattern seed-sym seed-domain)

    ;; Vector pattern: [a b c] or [a b . xs]
    (vector? pattern)
    (bind-pattern-vec env pattern seed-sym seed-domain)

    ;; List pattern (operator): (ks a b), (as m pat), etc.
    (seq? pattern)
    (bind-pattern-operator env pattern seed-sym seed-domain)

    ;; Literal: constrain seed to equal this value
    :else
    (if (or (number? pattern) (string? pattern) (keyword? pattern)
            (boolean? pattern) (nil? pattern))
      (let [found (tree/find env [seed-sym])
            _ (when-not found
                (throw (ex-info "Cannot find seed for literal pattern"
                                {:seed seed-sym :pattern pattern})))
            target (narrow/resolve found)
            target-path (tree/position target)
            d (or (:domain target) dom/any)
            literal-dom (dom/finite #{pattern})
            narrowed (dom/intersect d literal-dom)]
        (if (dom/void? narrowed)
          (throw (ex-info "Literal pattern mismatch"
                          {:pattern pattern :domain d}))
          (let [env' (narrow/set-domain (tree/root env) target-path narrowed)
                env' (or (tree/cd env' (tree/position env)) env')]
            [env' []])))
      (throw (ex-info "Unknown pattern form" {:pattern pattern})))))

(defn bind-pattern-map
  "Destructure a map pattern {:k1 pat1 :k2 pat2 . rest} with domain flow.
   Extracts per-field domains from the seed's tree children or composite domain.
   Returns [env' bound-syms]."
  [env pattern seed-sym seed-domain]
  (let [dot-val (get pattern '.)
        entries (dissoc pattern '.)
        ;; Helpful error for legacy {:keys [...]} usage
        _ (when (:keys pattern)
            (throw (ex-info (str "The {:keys [...]} form is not supported. "
                                 "Use {:x x :y y} or (ks x y) instead.")
                            {:pattern pattern})))
        _ (doseq [k (keys entries)]
            (when-not (keyword? k)
              (throw (ex-info (str "Map destructuring keys must be keywords, got: " (pr-str k))
                              {:pattern pattern :key k}))))
        ;; Fold over entries, propagating field domains
        [env syms]
        (reduce
         (fn [[env syms] [k v-pattern]]
           (let [field-dom (extract-field-domain env seed-sym k seed-domain)
                 get-expr (list 'get seed-sym k)]
             (if (symbol? v-pattern)
               ;; Simple symbol: bind directly with field domain
               (let [env' (domain-node env v-pattern field-dom get-expr)]
                 [env' (conj syms v-pattern)])
               ;; Nested pattern: create scaffolding gensym, then recurse
               (let [gsym (gensym "map_")
                     env' (domain-node env gsym field-dom get-expr)
                     [env'' inner-syms] (bind-pattern env' v-pattern gsym field-dom)]
                 [env'' (into (conj syms gsym) inner-syms)]))))
         [env []]
         entries)]
    ;; Handle dot-rest: {... . rest-pattern}
    (if dot-val
      (let [remove-keys (keys entries)
            dissoc-expr (apply list 'dissoc seed-sym remove-keys)
            ;; Rest domain: if seed is map-of, rest is also map-of; else any
            rest-dom (if (= :map-of (:kind seed-domain))
                       seed-domain
                       dom/any)]
        (if (symbol? dot-val)
          (let [env' (domain-node env dot-val rest-dom dissoc-expr)]
            [env' (conj syms dot-val)])
          ;; Nested pattern on rest
          (let [gsym (gensym "maprest_")
                env' (domain-node env gsym rest-dom dissoc-expr)
                [env'' inner-syms] (bind-pattern env' dot-val gsym rest-dom)]
            [env'' (into (conj syms gsym) inner-syms)])))
      [env syms])))

(defn bind-pattern-vec
  "Destructure a vector pattern [a b c] or [a b . xs] with domain flow.
   Supports tail decomposition: [a . mid c] where mid captures the middle.
   Extracts per-element domains from tuple/vector-of composites or tree children.
   Returns [env' bound-syms]."
  [env pattern seed-sym seed-domain]
  (let [dot-idx (reduce-kv (fn [acc i x] (if (= x '.) (reduced i) acc)) -1 (vec pattern))
        has-dot? (>= dot-idx 0)
        positional (if has-dot?
                     (subvec pattern 0 dot-idx)
                     pattern)
        after-dot (when has-dot? (subvec pattern (inc dot-idx)))
        rest-pattern (when (and has-dot? (seq after-dot)) (first after-dot))
        tail-positionals (when (and has-dot? (> (count after-dot) 1))
                           (subvec after-dot 1))
        ;; Fold over head positional elements
        [env syms]
        (reduce
         (fn [[env syms] [i elem-pattern]]
           (let [elem-dom (extract-element-domain env seed-sym i seed-domain)
                 nth-expr (list 'nth seed-sym i)]
             (if (symbol? elem-pattern)
               (let [env' (domain-node env elem-pattern elem-dom nth-expr)]
                 [env' (conj syms elem-pattern)])
               (let [gsym (gensym "vec_")
                     env' (domain-node env gsym elem-dom nth-expr)
                     [env'' inner-syms] (bind-pattern env' elem-pattern gsym elem-dom)]
                 [env'' (into (conj syms gsym) inner-syms)]))))
         [env []]
         (map-indexed vector positional))]
    (cond
      ;; Tail positionals: [a . mid c d]
      tail-positionals
      (let [seed-node (when-let [found (tree/find env [seed-sym])]
                        (narrow/resolve found))
            _ (when-not (and seed-node (:vector seed-node))
                (throw (ex-info "Cannot resolve vector for tail positionals"
                                {:pattern pattern :seed seed-sym})))
            children (tree/int-children seed-node)
            total (count children)
            head-count (count positional)
            tail-count (count tail-positionals)
            _ (when (> (+ head-count tail-count) total)
                (throw (ex-info (str "Vector has " total " elements but pattern requires at least "
                                     (+ head-count tail-count))
                                {:pattern pattern :total total})))
            middle-start head-count
            middle-end (- total tail-count)
            ;; Bind tail positionals from the end
            [env syms]
            (reduce
             (fn [[env syms] [i pat]]
               (let [idx (+ middle-end i)
                     elem-dom (extract-element-domain env seed-sym idx seed-domain)
                     nth-expr (list 'nth seed-sym idx)]
                 (if (symbol? pat)
                   (let [env' (domain-node env pat elem-dom nth-expr)]
                     [env' (conj syms pat)])
                   (let [gsym (gensym "tail_")
                         env' (domain-node env gsym elem-dom nth-expr)
                         [env'' inner-syms] (bind-pattern env' pat gsym elem-dom)]
                     [env'' (into (conj syms gsym) inner-syms)]))))
             [env syms]
             (map-indexed vector tail-positionals))
            ;; Build middle slice as vector of nth expressions
            middle-exprs (mapv (fn [i] (list 'nth seed-sym i))
                               (range middle-start middle-end))
            rest-dom (if (= :vector-of (:kind seed-domain))
                       seed-domain
                       dom/any)]
        (if rest-pattern
          (if (symbol? rest-pattern)
            (let [env' (domain-node env rest-pattern rest-dom (vec middle-exprs))]
              [env' (conj syms rest-pattern)])
            (let [gsym (gensym "mid_")
                  env' (domain-node env gsym rest-dom (vec middle-exprs))
                  [env'' inner-syms] (bind-pattern env' rest-pattern gsym rest-dom)]
              [env'' (into (conj syms gsym) inner-syms)]))
          [env syms]))

      ;; Simple rest: [a b . xs]
      rest-pattern
      (let [drop-n (count positional)
            rest-expr (list 'drop drop-n seed-sym)
            rest-dom (cond
                       ;; Tuple: remaining elements form a sub-tuple
                       (= :tuple (:kind seed-domain))
                       (let [remaining (vec (drop drop-n (:elements seed-domain)))]
                         (if (seq remaining)
                           (dom/tuple-dom remaining)
                           dom/any))
                       ;; Vector-of: rest keeps the same element type
                       (= :vector-of (:kind seed-domain))
                       seed-domain
                       :else dom/any)]
        (if (symbol? rest-pattern)
          (let [env' (domain-node env rest-pattern rest-dom rest-expr)]
            [env' (conj syms rest-pattern)])
          (let [gsym (gensym "rest_")
                env' (domain-node env gsym rest-dom rest-expr)
                [env'' inner-syms] (bind-pattern env' rest-pattern gsym rest-dom)]
            [env'' (into (conj syms gsym) inner-syms)])))

      ;; No rest — just head positionals
      :else [env syms])))

;; ════════════════════════════════════════════════════════════════
;; fn-destruct — inverse of multi-branch fn
;; ════════════════════════════════════════════════════════════════

(defn- fn-destruct-branch
  "Try to invert a single fn branch for destructuring.
   Works for branches with symbol/type-wrapper/literal params.
   If body is (and constraint... value), splits constraints from value template.
   Returns [env' bound-syms] or throws on contradiction."
  [env params body pattern-syms seed-sym seed-domain]
  (let [;; Classify params and build substitution
        param-info (mapv (fn [p ps]
                           (cond
                             (symbol? p)
                             {:kind :symbol :raw-sym p :pattern-sym ps}

                             (seq? p)
                             {:kind :type-wrapper :raw-sym (last p) :wrapper (first p)
                              :pattern-sym ps}

                             (or (number? p) (string? p) (keyword? p) (boolean? p))
                             {:kind :literal :value p :pattern-sym ps}

                             ;; Complex patterns (maps, vectors) → can't invert
                             :else
                             (throw (ex-info "Cannot invert branch with complex param pattern"
                                             {:param p}))))
                         params pattern-syms)
        ;; Substitution: symbol/type-wrapper raw-syms → pattern-syms
        substitution (into {} (keep (fn [{:keys [kind raw-sym pattern-sym]}]
                                      (when (#{:symbol :type-wrapper} kind)
                                        [raw-sym pattern-sym])))
                            param-info)
        ;; Split body: if (and ...), intermediates are constraints, last is value
        [body-constraints body-value]
        (if (and (seq? body) (= 'and (first body)))
          [(butlast (rest body)) (last body)]
          [[] body])
        ;; Substitute to create destruct pattern and constraints
        destruct-pattern (substitute-expr body-value substitution)
        destruct-constraints (mapv #(substitute-expr % substitution) body-constraints)
        ;; Type constraints from wrapper params
        param-constraints (keep (fn [{:keys [kind wrapper pattern-sym]}]
                                  (when (= :type-wrapper kind)
                                    [wrapper pattern-sym]))
                                param-info)
        ;; Literal params to bind
        literal-bindings (keep (fn [{:keys [kind value pattern-sym]}]
                                 (when (= :literal kind)
                                   [pattern-sym value]))
                               param-info)
        ;; Destructure seed against inverted body pattern
        [env' bound-syms] (bind-pattern env destruct-pattern seed-sym seed-domain)
        ;; Apply body-level constraints (from and-wrapped body)
        env' (reduce (fn [e c]
                       (let [e' (bind e c)]
                         (dissoc e' :link)))
                     env'
                     destruct-constraints)
        ;; Bind literal params' pattern-syms to their literal values
        env' (reduce (fn [e [ps val]]
                       (let [e' (bind e ps val)]
                         (dissoc e' :link)))
                     env'
                     literal-bindings)
        ;; Apply param-level type constraints
        env' (reduce (fn [e [wrapper-head target-sym]]
                       (let [my-pos (tree/position e)
                             [e' path] (ensure-node-abs e target-sym)
                             wrapper-node (tree/find e' [wrapper-head])
                             wrapper-resolved (when wrapper-node
                                                (narrow/resolve wrapper-node))
                             type-dom (:type-domain wrapper-resolved)]
                         (if type-dom
                           (let [d (narrow/domain-of (tree/root e') path)
                                 narrowed (dom/intersect d type-dom)]
                             (if (dom/void? narrowed)
                               (throw (ex-info "Contradiction: destructuring type constraint"
                                               {:wrapper wrapper-head
                                                :target target-sym}))
                               (let [e'' (narrow/set-domain (tree/root e') path narrowed)]
                                 (or (tree/cd e'' my-pos) e''))))
                           (let [e' (bind e (list wrapper-head target-sym))]
                             (dissoc e' :link)))))
                     env'
                     param-constraints)]
    [env' bound-syms]))

(defn fn-destruct
  "Derive a destructor from a multi-branch fn definition.
   Tries each branch's inverse in order. First satisfiable wins."
  [env {:keys [branches] :as mufl-fn} pattern-syms seed-sym seed-domain]
  (let [n-syms (count pattern-syms)]
    (loop [remaining branches
           last-err nil]
      (if (empty? remaining)
        (throw (or last-err
                   (ex-info "No matching branch for destructuring"
                            {:pattern-syms pattern-syms})))
        (let [{:keys [params body]} (first remaining)]
          (if (not= (count params) n-syms)
            (recur (rest remaining) last-err)
            (let [result (try
                           (fn-destruct-branch env params body pattern-syms
                                               seed-sym seed-domain)
                           (catch #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) e
                             e))]
              (if (instance? #?(:clj Throwable :cljs js/Error) result)
                (recur (rest remaining) result)
                result))))))))

(defn bind-pattern-operator
  "Dispatch an operator pattern like (ks a b), (as m pat), (point x y).
   Checks for :destruct (built-in operators), then :mufl-fn (user functions),
   then domain values (callable domains as type guards).
   Returns [env' bound-syms]."
  [env [head & args] seed-sym seed-domain]
  (let [node (tree/find env [head])
        _ (when-not node
            (throw (ex-info (str "Unresolved pattern head: " head)
                            {:pattern (cons head args)})))
        resolved (narrow/resolve node)]
    (cond
      (:destruct resolved)
      ((:destruct resolved) env args seed-sym seed-domain)

      (:mufl-fn resolved)
      (fn-destruct env (:mufl-fn resolved) args seed-sym seed-domain)

      ;; Domain value as type guard: (intvec x) ≡ (integer x) but for any domain
      ;; Binds inner pattern to seed, then applies domain constraint structurally.
      ;; Always use raw node (pre-resolve): resolve is lossy and collapses
      ;; link+map (and-composition) into plain map nodes.
      ;; resolve-schema-from-tree follows links internally, so pre-resolving is unnecessary.
      :else
      (let [domain (schema/try-resolve-schema-from-tree node)]
        (if domain
          (do
            (when (not= 1 (count args))
              (throw (ex-info (str "Domain pattern '" head "' takes exactly one argument")
                              {:pattern (cons head args) :arg-count (count args)})))
            (let [;; For simple scalar domains (non-composite, non-structural), use fast intersect path
                  scalar? (not (#{:vector-of :tuple :map-of :map-fields :intersection} (:kind domain)))]
              (if scalar?
                ;; Scalar domain: intersect seed domain + recurse (like type-domain-destruct)
                (let [narrowed (dom/intersect (or seed-domain dom/any) domain)]
                  (if (dom/void? narrowed)
                    (throw (ex-info (str "Domain guard contradiction in pattern: " head)
                                    {:domain domain :seed-domain seed-domain}))
                    (bind-pattern env (first args) seed-sym narrowed)))
                ;; Composite/structural: bind inner pattern first, then apply constraint
                (let [inner-pattern (first args)
                      [env' bound-syms] (bind-pattern env inner-pattern seed-sym seed-domain)
                      found (tree/find env' [seed-sym])
                      target (narrow/resolve found)
                      target-path (tree/position target)
                      env'' (schema/apply-domain-constraint env' domain target-path)]
                  [env'' bound-syms]))))
          (throw (ex-info (str "Cannot use '" head "' as pattern: not a function, destructor, or domain")
                          {:pattern (cons head args)})))))))

;; ════════════════════════════════════════════════════════════════
;; Multi-branch fn call support
;; ════════════════════════════════════════════════════════════════

(defn- bind-param
  "Bind a single param pattern against an arg path in the call scope.
   Handles: symbols, type wrappers (integer x), literals, maps, vectors."
  [env param arg-path]
  (cond
    ;; Simple symbol: link to arg
    (symbol? param)
    (let [e (tree/ensure-path env [param])]
      (tree/upd e [param] #(assoc % :link arg-path)))

    ;; Type wrapper: (integer x), (string x), etc.
    (seq? param)
    (let [raw-sym (last param)
          wrapper-head (first param)
          e (tree/ensure-path env [raw-sym])
          e (tree/upd e [raw-sym] #(assoc % :link arg-path))]
      (bind e (list wrapper-head raw-sym)))

    ;; Literal: constrain arg to equal this value
    (or (number? param) (string? param) (keyword? param) (boolean? param))
    (let [d (narrow/domain-of (tree/root env) arg-path)
          literal-dom (dom/finite #{param})
          narrowed (dom/intersect (or d dom/any) literal-dom)]
      (if (dom/void? narrowed)
        (throw (ex-info "Pattern match failed: literal mismatch"
                        {:pattern param :domain d}))
        (let [env' (narrow/set-domain (tree/root env) arg-path narrowed)]
          (or (tree/cd env' (tree/position env)) env'))))

    ;; Map/vector pattern: use bind-pattern with a seed linked to the arg
    (or (map? param) (vector? param))
    (let [gsym (gensym "param_")
          e (tree/ensure-path env [gsym])
          e (tree/upd e [gsym] #(assoc % :link arg-path))
          seed-domain (narrow/domain-of (tree/root e) arg-path)
          [e' _] (bind-pattern e param gsym seed-domain)]
      e')

    :else
    (throw (ex-info "Invalid param pattern" {:param param}))))

(defn- call-branch
  "Execute a single fn branch: bind params to args, evaluate body,
   link caller to result. Returns the caller env with link, or throws
   on contradiction."
  [env-after-args arg-paths params body my-pos fn-name]
  (let [;; Create a call scope as child of current position
        call-name (gensym "call")
        env-with-scope (tree/ensure-path env-after-args [call-name])
        call-env (tree/cd env-with-scope [call-name])
        ;; Bind each param pattern to its arg
        env-bound (reduce (fn [e [param arg-path]]
                            (bind-param e param arg-path))
                          call-env
                          (map vector params arg-paths))
        ;; Create result sub-node and evaluate body there
        result-name (gensym "result_")
        env-with-result (tree/ensure-path env-bound [result-name])
        result-env (tree/cd env-with-result [result-name])
        result (bind result-env body)
        result-pos (tree/position result)]
    ;; Link caller to result
    (let [result-root (tree/root result)
          caller (tree/cd result-root my-pos)
          target-path (or (:link result) result-pos)
          result-node (tree/cd result-root target-path)
          has-value? (or (:domain result-node)
                         (:map result-node)
                         (:vector result-node)
                         (and (:link result-node)
                              (not= (:link result-node) target-path)))]
      (if has-value?
        (assoc caller :link target-path)
        caller))))

(defn- call-fn
  "Call a multi-branch fn. Evaluates args once, then tries each branch
   in order. First satisfiable branch wins. Falls back to default."
  [env {:keys [branches default]} args fn-name]
  (let [my-pos (tree/position env)
        ;; Evaluate all arg expressions in the CALLER's scope first
        [env-after-args arg-paths]
        (reduce (fn [[e paths] arg-expr]
                  (let [[e' path] (ensure-node-abs e arg-expr)]
                    [e' (conj paths path)]))
                [env []]
                args)
        n-args (count args)]
    ;; Try each branch in order
    (loop [remaining branches
           last-err nil]
      (if (empty? remaining)
        ;; No branch matched — try default
        (if default
          (let [result (bind env-after-args default)
                result-root (tree/root result)
                caller (tree/cd result-root my-pos)
                target-path (or (:link result) (tree/position result))
                result-node (tree/cd result-root target-path)
                has-value? (or (:domain result-node)
                               (:map result-node)
                               (:vector result-node)
                               (and (:link result-node)
                                    (not= (:link result-node) target-path)))]
            (if has-value?
              (assoc caller :link target-path)
              caller))
          (throw (or last-err
                     (ex-info (str "No matching branch for fn: " fn-name)
                              {:fn fn-name :n-args n-args}))))
        (let [{:keys [params body]} (first remaining)]
          (if (not= (count params) n-args)
            ;; Arity mismatch — skip this branch
            (recur (rest remaining) last-err)
            ;; Try this branch
            (let [result (try
                           (call-branch env-after-args arg-paths params body my-pos fn-name)
                           (catch #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) e
                             ;; Recursion depth errors propagate immediately
                             (when (contains? (ex-data e) :depth)
                               (throw e))
                             e))]
              (if (instance? #?(:clj Throwable :cljs js/Error) result)
                (recur (rest remaining) result)
                result))))))))

;; ════════════════════════════════════════════════════════════════
;; The bind function
;; ════════════════════════════════════════════════════════════════

(defn bind
  "Bind an expression into the environment tree.

   (bind env expr)           — bind expr at current position
   (bind env sym expr)       — bind expr as child named sym
   (bind env sym expr ...)   — sequential bindings with optional return"

  ([env expr]
   (cond
     ;; Literals → singleton domain
     (nil? expr)     (assoc env :domain (dom/single nil))
     (number? expr)  (assoc env :domain (dom/single expr))
     (string? expr)  (assoc env :domain (dom/single expr))
     (boolean? expr) (assoc env :domain (dom/single expr))
     (keyword? expr) (assoc env :domain (dom/single expr))

     ;; Symbol → lookup and link
     (symbol? expr)
     (if-let [found (tree/find env [expr])]
       (let [found-target (narrow/resolve found)]
         (assoc env :link (tree/position found-target)))
       (throw (ex-info (str "Unresolved symbol: " expr) {:sym expr})))

     ;; List → dispatch on head
     (seq? expr)
     (let [[head & args] expr]
       (cond
         (symbol? head)
         (let [raw-node (tree/find env [head])
               node (if (:link raw-node) (narrow/resolve raw-node) raw-node)]
           (cond
             ;; Built-in form with :construct
             (:construct node)
             ((:construct node) env args)

             ;; User-defined function with :mufl-fn
             (:mufl-fn node)
             (do
               (when (>= *call-depth* max-call-depth)
                 (throw (ex-info (str "Recursion depth limit exceeded (depth=" *call-depth* ")")
                                 {:depth *call-depth* :fn head})))
               (binding [*call-depth* (inc *call-depth*)]
                 (call-fn env (:mufl-fn node) args head)))

             ;; Domain value used as constraint: (intvec x) ≡ (narrow x intvec)
             ;; Works for type constructors (vector-of, tuple, map-of), named defs, etc.
             ;; Always use raw-node (pre-resolve): resolve is lossy and collapses
             ;; link+map (and-composition) into plain map nodes.
             ;; resolve-schema-from-tree follows links internally, so pre-resolving is unnecessary.
             :else
             (if-let [schema (schema/try-resolve-schema-from-tree raw-node)]
               (apply-domain-constraint env schema args)
               (throw (ex-info (str "Cannot call '" head "': not a function, constructor, or domain")
                               {:head head})))))

         ;; Keyword as function: (:key m) → (get m :key)
         (keyword? head)
         (bind env (list 'get (first args) head))

         :else
         (throw (ex-info "Cannot bind list with non-symbol head" {:expr expr}))))

     ;; Vector → bind each element as an indexed child
     (vector? expr)
     (let [env (reduce (fn [e [i sub-expr]]
                         (bind e i sub-expr))
                       (assoc env :vector true)
                       (map-indexed vector expr))]
       env)

     ;; Map → bind each key-value pair
     (map? expr)
     (let [env (reduce (fn [e [k v]]
                         (bind e k v))
                       (assoc env :map true)
                       expr)]
       env)

     :else
     (throw (ex-info "Cannot bind" {:expr expr}))))

  ([env sym expr]
   (let [path [sym]
         env (tree/ensure-path env path)]
     (tree/upd env path #(bind % expr))))

  ([env sym expr & more]
   (let [[pairs return] (pairs&return (list* sym expr more))
         bound (reduce (fn [e [s v]] (bind e s v)) env pairs)]
     (if return
       (bind bound return)
       bound))))
