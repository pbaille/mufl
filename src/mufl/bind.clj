(ns mufl.bind
  "Core bind + constraint registration for mufl.

   bind takes [env expr] and returns a new tree where:
   1. New nodes are created for the expression
   2. Constraint nodes are added
   3. All domains are propagated to fixpoint

   After every bind, the tree is maximally narrowed.

   This namespace re-exports public API from:
   - mufl.narrow  (resolve, domain-of, set-domain, propagate, apply-composite-constraint)
   - mufl.schema  (resolve-domain-schema, apply-domain-constraint)
   - mufl.pattern (bind-pattern, bind-pattern-map, domain-node)"
  (:refer-clojure :exclude [resolve])
  (:require [mufl.tree :as tree]
            [mufl.domain :as dom]
            [mufl.narrow :as narrow]
            [mufl.schema :as schema]
            [mufl.pattern :as pattern]))

;; ════════════════════════════════════════════════════════════════
;; Re-exports — unified facade
;; ════════════════════════════════════════════════════════════════
;;
;; bind serves as the primary API surface for callers (env, core,
;; search, show). Rather than requiring narrow, schema, and pattern
;; individually, callers use bind/ for everything. This keeps import
;; lists short and decouples callers from the internal module split.
;;
;; High-traffic (10+ call sites): resolve, domain-of
;; Medium (2-4 call sites):       set-domain, bind-pattern, resolve-domain-schema,
;;                                apply-domain-constraint, propagate-watchers
;; Low (0-1 call sites):          resolve-at, apply-composite-constraint,
;;                                bind-pattern-map, domain-node, propagate
;;
;; The low-usage re-exports are kept for API consistency — callers
;; shouldn't need to know which sub-namespace owns a function.

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
(def apply-domain-constraint schema/apply-domain-constraint)

;; From mufl.pattern
(def bind-pattern pattern/bind-pattern)
(def bind-pattern-map pattern/bind-pattern-map)
(def domain-node pattern/domain-node)

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

(declare bind)

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

    ;; Expression → create anonymous child, bind the expr there
    (seq? expr)
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
   when expr is a list (creates a temp node via ensure-node-abs).
   For symbol expressions, env' = env.
   Throws if the expression cannot be resolved."
  [env expr op-name]
  (cond
    (symbol? expr)
    (if-let [found (tree/find env [expr])]
      [env (narrow/resolve found)]
      (throw (ex-info (str op-name ": cannot resolve: " expr)
                      {:expr expr})))
    (seq? expr)
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
          [e' _] (pattern/bind-pattern e param gsym seed-domain)]
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
                           (catch clojure.lang.ExceptionInfo e
                             ;; Recursion depth errors propagate immediately
                             (when (contains? (ex-data e) :depth)
                               (throw e))
                             e))]
              (if (instance? Throwable result)
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
               (schema/apply-domain-constraint env schema args)
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
