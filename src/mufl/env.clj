(ns mufl.env
  "Base environment for mufl.

   Populates the tree with built-in forms:
   one-of, let, and, =, !=, <, >, <=, >=, +, -, *, distinct"
  (:require [mufl.tree :as tree]
            [mufl.domain :as dom]
            [mufl.bind :as bind]))

;; ════════════════════════════════════════════════════════════════
;; Base environment construction
;; ════════════════════════════════════════════════════════════════

(defn defprim
  "Add a primitive form to the environment tree."
  [env name fields]
  (-> (tree/ensure-path env [name])
      (tree/put [name] (assoc fields :primitive true))))

(defn defprims
  "Register multiple primitives. specs is a seq of [name fields] pairs."
  [env specs]
  (reduce (fn [e [name fields]] (defprim e name fields))
          env specs))

(defn- filter-pred-bind
  "Build a :bind fn for a domain-filter predicate (even?, odd?, pos?, neg?, zero?)."
  [pred-fn label]
  (fn [env [arg]]
    (let [[env' path] (bind/ensure-node-abs env arg)
          d (bind/domain-of (tree/root env') path)
          narrowed (dom/domain-filter d pred-fn)]
      (if (dom/void? narrowed)
        (throw (ex-info (str "Contradiction: no " label " values") {:arg arg}))
        (let [env' (bind/set-domain (tree/root env') path narrowed)]
          (or (tree/cd env' (tree/position env)) env'))))))

(defn- type-domain-bind
  "Build a :bind fn for a type domain primitive (string, number, etc.).
   Nullary: set own domain. Unary: constrain arg AND link to it.
   (integer x) constrains x to integer AND returns a link to x,
   so the expression evaluates to the constrained value.
   In destructuring: (integer a) binds a to the value, then constrains to integer."
  [type-dom label]
  (fn [env args]
    (if (:destructuring (meta args))
      ;; Destructuring: (integer a value) — bind a to value, constrain to type
      (let [value (last args)
            inner-pattern (first (butlast args))
            ;; First bind the inner pattern to the value
            env' (bind/destructure env inner-pattern value)
            ;; Then constrain the value to the type domain
            [env'' val-path] (bind/ensure-node-abs env' value)
            d (bind/domain-of (tree/root env'') val-path)
            narrowed (dom/intersect d type-dom)]
        (if (dom/void? narrowed)
          (throw (ex-info (str "Contradiction: no " label " values in destructuring") {:value value}))
          (let [env'' (bind/set-domain (tree/root env'') val-path narrowed)]
            (or (tree/cd env'' (tree/position env)) env''))))
      (if (seq args)
        ;; Unary: (string x) → constrain arg to type domain + link to it
        (let [[arg] args
              [env' path] (bind/ensure-node-abs env arg)
              d (bind/domain-of (tree/root env') path)
              narrowed (dom/intersect d type-dom)]
          (if (dom/void? narrowed)
            (throw (ex-info (str "Contradiction: no " label " values") {:arg arg}))
            (let [env' (bind/set-domain (tree/root env') path narrowed)
                  caller (or (tree/cd env' (tree/position env)) env')]
              ;; Link caller to the constrained arg so (integer x) evaluates to x
              (assoc caller :link path))))
        ;; Nullary: (string) → set own domain
        (assoc env :domain type-dom)))))

(defn- resolve-type-expr
  "Resolve a type expression to a domain value.
   Handles:
   - Symbols: look up :type-domain, then bound domain
   - List expressions: bind them and extract :type-domain from the result
   Returns the domain or nil if unresolvable as a domain."
  [env expr]
  (cond
    (symbol? expr)
    (when-let [found (tree/find env [expr])]
      (let [resolved (bind/resolve found)]
        (or (:type-domain resolved)
            ;; For bound values with a domain (e.g., (def SmallInt (between 1 10)))
            (:domain resolved))))

    (seq? expr)
    ;; Bind the expression to get its :type-domain
    (let [tmp (gensym "typeres__")
          env' (bind/bind env tmp expr)
          node (when-let [found (tree/find env' [tmp])]
                 (bind/resolve found))]
      (:type-domain node))

    :else nil))

(defn base-env
  "Build the base environment with all built-in forms."
  []
  (-> {}

      ;; ── one-of: create a finite domain ──────────────────────
      (defprim 'one-of
        {:bind
         (fn [env args]
           (assoc env :domain (dom/finite (set args))))})

      ;; ── let: sequential bindings with body ──────────────────
      ;; Supports symbol bindings (let [x 1] ...) and destructuring
      ;; patterns: {:x a :y b} for maps, [a b c] for vectors.
      (defprim 'let
        {:bind
         (fn [env [bindings-vec & body]]
           (let [pairs (partition 2 bindings-vec)
                 env' (reduce (fn [e [pattern expr]]
                                (if (symbol? pattern)
                                  (bind/bind e pattern expr)
                                  (bind/destructure e pattern expr)))
                              env pairs)]
             (if (= 1 (count body))
               (bind/bind env' (first body))
               (reduce (fn [e expr] (bind/bind e expr)) env' body))))})

      ;; ── and: all constraints must hold, last is return ──────
      (defprim 'and
        {:bind
         (fn [env args]
           (reduce (fn [e arg]
                     (let [result (bind/bind e arg)]
                       ;; If the sub-bind produced a constraint (no domain on current pos),
                       ;; we carry on. If it set a domain, that's the return value intent.
                       result))
                   env args))})

      ;; ── Relational constraints ──────────────────────────────

      (defprims (for [[sym op] [['< :<] ['> :>] ['<= :<=] ['>= :>=] ['= :=] ['!= :!=]]]
                  [sym {:bind (fn [env args] (bind/bind-relational env op args))}]))

      ;; ── Arithmetic ──────────────────────────────────────────

      (defprim '+
        {:bind (fn [env args]
                 (bind/bind-arithmetic env :+ args))})

      (defprim '-
        {:bind (fn [env args]
                 (bind/bind-arithmetic env :- args))})

      (defprim '*
        {:bind (fn [env args]
                 (bind/bind-arithmetic env :* args))})

      ;; ── distinct (all-different) ────────────────────────────
      (defprim 'distinct
        {:bind
         (fn [env [vec-expr]]
           ;; vec-expr should be a vector literal of symbols
           (let [my-pos (tree/position env)
                 paths (mapv (fn [sym]
                               (let [p (bind/resolve-to-path env sym)]
                                 (when-not p
                                   (throw (ex-info (str "Cannot resolve in distinct: " sym)
                                                   {:sym sym})))
                                 p))
                             vec-expr)
                 env-root (bind/add-constraint (tree/root env) :alldiff paths my-pos)]
             (or (and env-root (tree/cd env-root my-pos))
                 (throw (ex-info "Contradiction in distinct" {:args vec-expr})))))})

      ;; ── mod / quot ──────────────────────────────────────────

      (defprim 'mod
        {:bind (fn [env args]
                 (bind/bind-arithmetic env :mod args))})

      (defprim 'quot
        {:bind (fn [env args]
                 (bind/bind-arithmetic env :quot args))})

      ;; ── between: integer range domain ──────────────────────
      (defprim 'between
        {:bind
         (fn [env [lo hi]]
           (assoc env :domain (dom/int-range lo hi)))})

      ;; ── do: sequence of expressions, return last ────────────
      (defprim 'do
        {:bind
         (fn [env args]
           (reduce (fn [e arg] (bind/bind e arg))
                   env args))})

      ;; ── or: disjunction ──────────────────────────────────────
      ;;
      ;; DISJUNCTION SEMANTICS:
      ;;
      ;; mufl has TWO disjunction mechanisms with different trade-offs:
      ;;
      ;; 1. `or` (bind-time, domain union) — FAST but IMPRECISE:
      ;;    Tries each branch on a snapshot. For each variable, unions the
      ;;    per-branch domains. This is sound (never removes valid values)
      ;;    but may be incomplete — it doesn't track WHICH combinations of
      ;;    variable values are valid across branches.
      ;;    Example: (or (= x 1) (= x 2)) → x ∈ {1, 2}  ✓ correct
      ;;    But: (or (and (= x 1) (= y 2)) (and (= x 2) (= y 1)))
      ;;    → x ∈ {1,2}, y ∈ {1,2} — doesn't know x=1↔y=2 correlation.
      ;;
      ;; 2. `if`/`cond` (solve-time, fork) — PRECISE but SLOWER:
      ;;    When both branches are satisfiable, stores a :fork node.
      ;;    At solve time, each branch is explored independently with
      ;;    the correct constraint environment, giving exact results.
      ;;
      ;; When to use which:
      ;; - `or`: for simple domain narrowing, no cross-variable correlation needed
      ;; - `if`/`cond`: for precise branching, especially with correlated variables
      ;;
      ;; Future: `(defdomain Shape (or Circle Rect))` is NOT YET supported.
      ;; Domain-level disjunction (sum types) would need to be added to domain.clj
      ;; as a new :union domain kind with proper intersection/narrowing algebra.
      (defprim 'or
        {:bind
         (fn [env args]
           (if (:destructuring (meta args))
             ;; ── Destructuring context: (or pat1 pat2) against value ──
             ;; Try each pattern, take the first that succeeds.
             (let [value (last args)
                   patterns (butlast args)]
               (loop [[pat & rest-pats] patterns]
                 (if (nil? pat)
                   (throw (ex-info "All or-patterns failed" {:args args}))
                   (if-let [result (try (bind/destructure env pat value)
                                        (catch Exception _ nil))]
                     result
                     (recur rest-pats)))))
             ;; ── Expression context: domain union ──
             (let [pos (tree/position env)
                   ;; Collect successful branch snapshots
                   branch-envs
                   (keep (fn [arg]
                           (try
                             (bind/bind env arg)
                             (catch clojure.lang.ExceptionInfo _ nil)))
                         args)]
               (if (empty? branch-envs)
                 (throw (ex-info "All or-branches contradicted" {:args args}))
                 ;; For each domain-bearing node under current scope,
                 ;; compute the union of its domain across branches
                 (let [;; Get all domain-bearing children in first branch to find var paths
                       all-paths (->> branch-envs
                                      (mapcat (fn [e]
                                                (->> (tree/children e)
                                                     (keep (fn [child]
                                                             (when (and (:domain child)
                                                                        (not (:constraint child))
                                                                        (not (:primitive child)))
                                                               (tree/position child)))))))
                                      distinct)
                       ;; For each path, union the domains across branches
                       env-root (tree/root env)
                       env-root
                       (reduce
                        (fn [env-root vpath]
                          (let [unioned (->> branch-envs
                                             (map (fn [e]
                                                    (let [node (tree/at (tree/root e) vpath)]
                                                      (clojure.core/or (:domain node) dom/any))))
                                             (reduce dom/unite dom/void))
                                current (bind/domain-of env-root vpath)
                                narrowed (dom/intersect current unioned)]
                            (if (dom/void? narrowed)
                              (throw (ex-info "Contradiction in or" {:path vpath}))
                              (bind/set-domain env-root vpath narrowed))))
                        env-root
                        all-paths)]
                   (clojure.core/or (tree/cd env-root pos) env-root))))))})

;; ── not: constraint negation ────────────────────────────
      ;; (not (= x y)) → (!= x y), etc.
      (defprim 'not
        {:bind
         (fn [env [inner]]
           (if-not (seq? inner)
             (throw (ex-info (str "Cannot negate non-list expression: " (pr-str inner))
                             {:inner inner}))
             (let [[op & op-args] inner
                   negated (case op
                             =  '!=
                             != '=
                             <  '>=
                             >  '<=
                             <= '>
                             >= '<
                             (throw (ex-info (str "Cannot negate: " op) {:op op})))]
               (bind/bind env (cons negated op-args)))))})

      ;; ── if: conditional branching ─────────────────────────
      ;; At bind time: tries eager resolution when the test is deterministic
      ;; (one branch contradicts). This enables recursion with ground args.
      ;; Falls back to fork storage for solve-time enumeration when both
      ;; branches are satisfiable (non-ground case).
      (defprim 'if
        {:bind
         (fn [env [test then else]]
           ;; If test is a boolean literal, resolve at bind time
           (cond
             (true? test)  (bind/bind env then)
             (false? test) (bind/bind env (if (some? else) else nil))
             :else
             ;; Try eager resolution: evaluate the test as a constraint
             (let [then-env (try (bind/bind env test) (catch Exception _ nil))
                   else-env (try (bind/bind env (list 'not test)) (catch Exception _ nil))]
               (cond
                 ;; Only then-branch is satisfiable → take it eagerly
                 (and then-env (nil? else-env))
                 (bind/bind then-env then)

                 ;; Only else-branch is satisfiable → take it eagerly
                 (and (nil? then-env) else-env)
                 (bind/bind else-env (if (some? else) else nil))

                 ;; Both branches satisfiable → defer to solve time (non-ground)
                 (and then-env else-env)
                 (assoc env :fork {:kind :if
                                   :test test
                                   :then then
                                   :else else})

                 ;; Neither branch satisfiable → contradiction
                 :else
                 (throw (ex-info "Both branches of if contradicted"
                                 {:test test}))))))})

      ;; ── when: like if but no else branch ────────────────────
      (defprim 'when
        {:bind
         (fn [env [test & body]]
           (bind/bind env (list 'if test (cons 'do body) nil)))})

      ;; ── cond: chained conditionals ──────────────────────────
      ;; Like if, tries eager resolution. If exactly one branch is satisfiable
      ;; and all others contradict, takes it eagerly. Otherwise defers to solve.
      (defprim 'cond
        {:bind
         (fn [env args]
           (let [branches (vec (partition 2 args))
                 ;; Try each branch, collecting [test-env body-expr] for viable ones
                 viable (keep (fn [[test body]]
                                (cond
                                  (= test :else) [env body]
                                  (true? test)   [env body]
                                  (false? test)  nil
                                  :else
                                  (try
                                    [(bind/bind env test) body]
                                    (catch Exception _ nil))))
                              branches)]
             (cond
               ;; Exactly one branch viable → take it eagerly
               (= 1 (count viable))
               (let [[branch-env body] (first viable)]
                 (bind/bind branch-env body))

               ;; No branches viable → no solutions (empty fork)
               (empty? viable)
               (assoc env :fork {:kind :cond :branches branches})

               ;; Multiple branches viable → defer to solve time
               :else
               (assoc env :fork {:kind :cond :branches branches}))))})

      ;; ── Filter predicates: even, odd, pos, neg, zero ───────
      ;; Each narrows a domain by filtering with a Clojure predicate.
      ;; No ? suffix — these are constraints, not boolean predicates.
      (defprims (for [[sym pred-fn label] [['even even? "even"]
                                            ['odd odd? "odd"]
                                            ['pos pos? "positive"]
                                            ['neg neg? "negative"]
                                            ['zero zero? "zero"]]]
                  [sym {:bind (filter-pred-bind pred-fn label)}]))

      ;; ── fn: anonymous function ──────────────────────────────
      ;; (fn [params...] body) — creates a callable node.
      ;; When applied: creates a scope, binds args to params, evaluates body.
      (defprim 'fn
        {:bind
         (fn [env [params & body]]
           ;; Store the function definition — don't evaluate body yet
           (assoc env :mufl-fn {:params params
                                :body (if (= 1 (count body))
                                        (first body)
                                        (cons 'do body))}))})

      ;; ── apply / function call ───────────────────────────────
      ;; Not a named primitive — handled in bind dispatch for list forms
      ;; when head resolves to a node with :mufl-fn

      ;; ── abs: absolute value ─────────────────────────────────
      ;; Implemented as a constraint: |a| = c
      ;; Narrowing: c must be in {|a| for a in A}; a must be in {a : |a| in C}
      (defprim 'abs
        {:bind
         (fn [env [arg]]
           (let [my-pos (tree/position env)
                 [env' a-path] (bind/ensure-node-abs env arg)
                 a-dom (bind/domain-of (tree/root env') a-path)
                 result-dom (if (dom/finite? a-dom)
                              (dom/finite (set (map #(Math/abs (long %)) (dom/members a-dom))))
                              dom/any)
                 env' (assoc env' :domain result-dom)
                 result-path (tree/position env')]
             (bind/add-constraint-and-return env' :abs [a-path result-path] my-pos)))})

      ;; ── min / max on two values ─────────────────────────────
      (defprim 'min
        {:bind
         (fn [env [a-expr b-expr]]
           (let [[env' a-path] (bind/ensure-node-abs env a-expr)
                 [env'' b-path] (bind/ensure-node-abs env' b-expr)
                 a-dom (bind/domain-of (tree/root env'') a-path)
                 b-dom (bind/domain-of (tree/root env'') b-path)]
             (if (and (dom/finite? a-dom) (dom/finite? b-dom))
               (let [result-dom (dom/finite (set (for [a (dom/members a-dom)
                                                       b (dom/members b-dom)]
                                                   (clojure.core/min a b))))]
                 (assoc (or (tree/cd (tree/root env'') (tree/position env)) env'')
                        :domain result-dom))
               (assoc env :domain dom/any))))})

      (defprim 'max
        {:bind
         (fn [env [a-expr b-expr]]
           (let [[env' a-path] (bind/ensure-node-abs env a-expr)
                 [env'' b-path] (bind/ensure-node-abs env' b-expr)
                 a-dom (bind/domain-of (tree/root env'') a-path)
                 b-dom (bind/domain-of (tree/root env'') b-path)]
             (if (and (dom/finite? a-dom) (dom/finite? b-dom))
               (let [result-dom (dom/finite (set (for [a (dom/members a-dom)
                                                       b (dom/members b-dom)]
                                                   (clojure.core/max a b))))]
                 (assoc (or (tree/cd (tree/root env'') (tree/position env)) env'')
                        :domain result-dom))
               (assoc env :domain dom/any))))})

      ;; ── get: map key access ─────────────────────────────────
      ;; (get m :key) navigates to the :key child of map node m.
      ;; Creates a link so constraints propagate back to the child.
      (defprim 'get
        {:bind
         (fn [env [coll-expr key-expr]]
           (let [;; Resolve the collection to its node
                 coll-node (cond
                             (symbol? coll-expr)
                             (when-let [found (tree/find env [coll-expr])]
                               (bind/resolve found))

                             (seq? coll-expr)
                             (let [[env' path] (bind/ensure-node-abs env coll-expr)]
                               (bind/resolve-at (tree/root env') path))

                             :else
                             (throw (ex-info "get: first arg must be a symbol or expression"
                                             {:coll coll-expr})))
                 _ (when-not coll-node
                     (throw (ex-info (str "get: cannot resolve collection: " coll-expr)
                                     {:coll coll-expr})))
                 _ (when-not (:map coll-node)
                     (throw (ex-info (str "get: not a map node: " coll-expr)
                                     {:coll coll-expr})))
                 _ (when-not (keyword? key-expr)
                     (throw (ex-info "get: key must be a literal keyword"
                                     {:key key-expr})))
                 coll-path (tree/position coll-node)
                 child-path (conj coll-path key-expr)
                 child (tree/cd (tree/root env) child-path)]
             (when-not child
               (throw (ex-info (str "get: key not found: " key-expr)
                               {:key key-expr :coll coll-expr})))
             ;; Link current node to the child for constraint propagation
             (let [target (bind/resolve child)
                   target-path (tree/position target)]
               (assoc env :link target-path))))})

      ;; ── nth: vector index access ────────────────────────────
      ;; (nth v i) navigates to the ith child of vector node v.
      ;; Creates a link so constraints propagate back to the child.
      (defprim 'nth
        {:bind
         (fn [env [vec-expr idx-expr]]
           (let [;; Resolve the vector to its node
                 vec-node (cond
                            (symbol? vec-expr)
                            (when-let [found (tree/find env [vec-expr])]
                              (bind/resolve found))

                            (seq? vec-expr)
                            (let [[env' path] (bind/ensure-node-abs env vec-expr)]
                              (bind/resolve-at (tree/root env') path))

                            :else
                            (throw (ex-info "nth: first arg must be a symbol or expression"
                                            {:vec vec-expr})))
                 _ (when-not vec-node
                     (throw (ex-info (str "nth: cannot resolve vector: " vec-expr)
                                     {:vec vec-expr})))
                 _ (when-not (:vector vec-node)
                     (throw (ex-info (str "nth: not a vector node: " vec-expr)
                                     {:vec vec-expr})))
                 _ (when-not (integer? idx-expr)
                     (throw (ex-info "nth: index must be a literal integer"
                                     {:idx idx-expr})))
                 vec-path (tree/position vec-node)
                 child-path (conj vec-path idx-expr)
                 child (tree/cd (tree/root env) child-path)]
             (when-not child
               (throw (ex-info (str "nth: index out of bounds: " idx-expr)
                               {:idx idx-expr :vec vec-expr})))
             ;; Link current node to the child for constraint propagation
             (let [target (bind/resolve child)
                   target-path (tree/position target)]
               (assoc env :link target-path))))})

      ;; ── Type domain primitives ─────────────────────────────
      ;; string, integer, number, keyword, boolean resolve to type domains.
      ;; Used in defdomain specs ({:name string}) and as constraints:
      ;;   Nullary: (string) → sets own domain to string-dom
      ;;   Unary:   (string x) → constrains x to string-dom
      ;; This unifies type domains with type predicates — no need for
      ;; separate string?/number?/keyword? primitives.
      (defprims (for [[sym type-dom label] [['string  dom/string-dom  "string"]
                                             ['integer dom/integer-dom "integer"]
                                             ['number  dom/number-dom  "number"]
                                             ['keyword dom/keyword-dom "keyword"]
                                             ['boolean dom/boolean-dom "boolean"]]]
                  [sym {:bind (type-domain-bind type-dom label)
                        :type-domain type-dom}]))

      ;; ── Type constructors: vector-of, tuple, map-of ────────
      ;; These are bind-time unrolling constraints, consistent with
      ;; how map/filter/reduce already handle collections. They walk
      ;; the structural tree at bind time and constrain each element.

      ;; ── vector-of: constrain all vector elements to a type ──
      ;; (vector-of integer v) — every element of v must be an integer.
      ;; In defdomain: (defdomain IntVec (vector-of integer))
      ;; Walks vector children, intersecting each with the type domain.
      (defprim 'vector-of
        {:type-constructor true
         :bind
         (fn [env args]
           (let [;; Determine arity:
                 ;; Nullary: invalid
                 ;; Unary: (vector-of type) in schema/domain context — set own domain kind
                 ;; Binary: (vector-of type vec-expr) — constraint form
                 _ (when (< (count args) 1)
                     (throw (ex-info "vector-of requires at least 1 argument" {:args args})))

                 type-expr (first args)
                 vec-expr (when (= 2 (count args)) (second args))

                 ;; Resolve the type constraint
                 type-node (tree/find env [type-expr])
                 type-info (when type-node (bind/resolve type-node))
                 type-dom (:type-domain type-info)]

             (if-not vec-expr
               ;; Nullary: (vector-of integer) → produce composite domain value
               (let [elem-dom (or type-dom
                                  (resolve-type-expr env type-expr))]
                 (if elem-dom
                   (let [composite (dom/vector-of-dom elem-dom)]
                     (assoc env :domain composite :type-domain composite))
                   (throw (ex-info (str "vector-of: cannot resolve element type to a domain: " type-expr)
                                   {:type type-expr}))))

               ;; Binary: constrain vector elements
               (let [;; Try to resolve element type to a domain value (covers type-dom + resolve-type-expr)
                     elem-dom (or type-dom (resolve-type-expr env type-expr))
                     ;; Resolve the vector
                     vec-node (cond
                                (symbol? vec-expr)
                                (when-let [found (tree/find env [vec-expr])]
                                  (bind/resolve found))
                                (seq? vec-expr)
                                (let [[env' path] (bind/ensure-node-abs env vec-expr)]
                                  (bind/resolve-at (tree/root env') path))
                                :else nil)
                     _ (when-not vec-node
                         (throw (ex-info "vector-of: cannot resolve vector"
                                         {:vec vec-expr})))
                     _ (when-not (:vector vec-node)
                         (throw (ex-info "vector-of: second argument must be a vector"
                                         {:vec vec-expr})))
                     my-pos (tree/position env)]

                 (if elem-dom
                   ;; Fast path: build composite domain + apply-composite-constraint
                   (let [composite (dom/vector-of-dom elem-dom)
                         target-path (tree/position vec-node)
                         result (bind/apply-composite-constraint (tree/root env) target-path composite)]
                     (if-not result
                       (throw (ex-info "vector-of: contradiction" {:type type-expr :vec vec-expr}))
                       (let [env' (:env result)
                             changed (:changed result)
                             ;; Propagate watchers for all changed paths
                             env' (if (seq changed)
                                    (let [watchers (->> changed
                                                        (mapcat (fn [p]
                                                                  (let [node (tree/at env' p)]
                                                                    (:watched-by node))))
                                                        (distinct)
                                                        vec)]
                                      (if (seq watchers)
                                        (or (bind/propagate env' watchers)
                                            (throw (ex-info "Contradiction during vector-of propagation" {})))
                                        env'))
                                    env')]
                         (or (tree/cd env' my-pos)
                             (throw (ex-info "vector-of: lost position" {:pos my-pos}))))))

                   ;; Slow path: use narrow on each element
                   (let [children (sort-by ::tree/name
                                           (clojure.core/filter #(integer? (::tree/name %))
                                                                (tree/children vec-node)))
                         env-root
                         (reduce
                          (fn [env-root child]
                            (let [idx (::tree/name child)
                                  env-at-pos (tree/cd env-root my-pos)
                                  env-after (bind/bind env-at-pos
                                                       (list 'narrow (list 'nth vec-expr idx) type-expr))]
                              (tree/root env-after)))
                          (tree/root env)
                          children)]
                     (or (tree/cd env-root my-pos)
                         (throw (ex-info "vector-of: lost position" {:pos my-pos})))))))))})

      ;; ── tuple: per-position type constraints ────────────────
      ;; (tuple [integer string boolean] v) — v must be a 3-element vector
      ;; where element 0 is integer, element 1 is string, element 2 is boolean.
      ;; In defdomain: (defdomain Point (tuple [number number]))
      (defprim 'tuple
        {:type-constructor true
         :bind
         (fn [env args]
           (let [_ (when (< (count args) 1)
                     (throw (ex-info "tuple requires at least 1 argument" {:args args})))

                 types-vec (first args)
                 vec-expr (when (= 2 (count args)) (second args))

                 _ (when-not (vector? types-vec)
                     (throw (ex-info "tuple: first argument must be a vector of types"
                                     {:types types-vec})))]

             (if-not vec-expr
               ;; Nullary: (tuple [integer string]) → produce composite domain value
               (let [elem-doms (mapv (fn [type-sym]
                                       (let [d (resolve-type-expr env type-sym)]
                                         (when-not d
                                           (throw (ex-info (str "tuple: cannot resolve element type to a domain: " type-sym)
                                                           {:type type-sym})))
                                         d))
                                     types-vec)
                     composite (dom/tuple-dom elem-doms)]
                 (assoc env :domain composite :type-domain composite))

               ;; Binary: constrain vector to match tuple shape
               (let [;; Try to resolve all element types to domains
                     elem-doms (mapv (fn [type-sym] (resolve-type-expr env type-sym)) types-vec)
                     all-resolved? (every? some? elem-doms)
                     ;; Resolve the vector
                     vec-node (cond
                                (symbol? vec-expr)
                                (when-let [found (tree/find env [vec-expr])]
                                  (bind/resolve found))
                                (seq? vec-expr)
                                (let [[env' path] (bind/ensure-node-abs env vec-expr)]
                                  (bind/resolve-at (tree/root env') path))
                                :else nil)
                     _ (when-not vec-node
                         (throw (ex-info "tuple: cannot resolve vector"
                                         {:vec vec-expr})))
                     _ (when-not (:vector vec-node)
                         (throw (ex-info "tuple: second argument must be a vector"
                                         {:vec vec-expr})))
                     children (sort-by ::tree/name
                                       (clojure.core/filter #(integer? (::tree/name %))
                                                            (tree/children vec-node)))
                     n (clojure.core/count children)
                     expected-n (count types-vec)
                     _ (when (not= n expected-n)
                         (throw (ex-info (str "tuple: vector has " n " elements but tuple specifies " expected-n)
                                         {:actual n :expected expected-n :vec vec-expr})))
                     my-pos (tree/position env)]

                 (if all-resolved?
                   ;; Fast path: build composite domain + apply-composite-constraint
                   (let [composite (dom/tuple-dom elem-doms)
                         target-path (tree/position vec-node)
                         result (bind/apply-composite-constraint (tree/root env) target-path composite)]
                     (if-not result
                       (throw (ex-info "tuple: contradiction" {:types types-vec :vec vec-expr}))
                       (let [env' (:env result)
                             changed (:changed result)
                             ;; Propagate watchers for all changed paths
                             env' (if (seq changed)
                                    (let [watchers (->> changed
                                                        (mapcat (fn [p]
                                                                  (let [node (tree/at env' p)]
                                                                    (:watched-by node))))
                                                        (distinct)
                                                        vec)]
                                      (if (seq watchers)
                                        (or (bind/propagate env' watchers)
                                            (throw (ex-info "Contradiction during tuple propagation" {})))
                                        env'))
                                    env')]
                         (or (tree/cd env' my-pos)
                             (throw (ex-info "tuple: lost position" {:pos my-pos}))))))

                   ;; Slow path: use narrow for non-primitive types
                   (let [env-root
                         (reduce
                          (fn [env-root [idx type-sym]]
                            (let [env-at-pos (tree/cd env-root my-pos)
                                  env-after (bind/bind env-at-pos
                                                       (list 'narrow (list 'nth vec-expr idx) type-sym))]
                              (tree/root env-after)))
                          (tree/root env)
                          (map-indexed vector types-vec))]
                     (or (tree/cd env-root my-pos)
                         (throw (ex-info "tuple: lost position" {:pos my-pos})))))))))})

      ;; ── map-of: constrain all map entries ───────────────────
      ;; (map-of keyword integer m) — all keys must be keywords,
      ;; all values must be integers.
      ;; In defdomain: (defdomain Scores (map-of keyword integer))
      (defprim 'map-of
        {:type-constructor true
         :bind
         (fn [env args]
           (let [_ (when (< (count args) 2)
                     (throw (ex-info "map-of requires at least 2 arguments: (map-of key-type val-type [map])"
                                     {:args args})))

                 key-type-expr (first args)
                 val-type-expr (second args)
                 map-expr (when (= 3 (count args)) (nth args 2))

                 ;; Resolve key type
                 key-node (tree/find env [key-type-expr])
                 key-info (when key-node (bind/resolve key-node))
                 key-dom (:type-domain key-info)

                 ;; Resolve value type
                 val-node (tree/find env [val-type-expr])
                 val-info (when val-node (bind/resolve val-node))
                 val-dom (:type-domain val-info)]

             (if-not map-expr
               ;; Nullary: (map-of keyword integer) → produce composite domain value
               (let [k-dom (or key-dom (resolve-type-expr env key-type-expr))
                     v-dom (or val-dom (resolve-type-expr env val-type-expr))]
                 (if (and k-dom v-dom)
                   (let [composite (dom/map-of-dom k-dom v-dom)]
                     (assoc env :domain composite :type-domain composite))
                   (throw (ex-info (str "map-of: cannot resolve key/val types to domains")
                                   {:key-type key-type-expr :val-type val-type-expr}))))

               ;; Ternary: constrain map entries
               (let [;; Try to resolve both key and value types to domains
                     k-dom (or key-dom (resolve-type-expr env key-type-expr))
                     v-dom (or val-dom (resolve-type-expr env val-type-expr))
                     ;; Resolve the map
                     map-node (cond
                                (symbol? map-expr)
                                (when-let [found (tree/find env [map-expr])]
                                  (bind/resolve found))
                                (seq? map-expr)
                                (let [[env' path] (bind/ensure-node-abs env map-expr)]
                                  (bind/resolve-at (tree/root env') path))
                                :else nil)
                     _ (when-not map-node
                         (throw (ex-info "map-of: cannot resolve map"
                                         {:map map-expr})))
                     _ (when-not (:map map-node)
                         (throw (ex-info "map-of: third argument must be a map"
                                         {:map map-expr})))
                     my-pos (tree/position env)]

                 (if (and k-dom v-dom)
                   ;; Fast path: build composite domain + apply-composite-constraint
                   (let [composite (dom/map-of-dom k-dom v-dom)
                         target-path (tree/position map-node)
                         result (bind/apply-composite-constraint (tree/root env) target-path composite)]
                     (if-not result
                       (throw (ex-info "map-of: contradiction" {:key-type key-type-expr :val-type val-type-expr :map map-expr}))
                       (let [env' (:env result)
                             changed (:changed result)
                             ;; Propagate watchers for all changed paths
                             env' (if (seq changed)
                                    (let [watchers (->> changed
                                                        (mapcat (fn [p]
                                                                  (let [node (tree/at env' p)]
                                                                    (:watched-by node))))
                                                        (distinct)
                                                        vec)]
                                      (if (seq watchers)
                                        (or (bind/propagate env' watchers)
                                            (throw (ex-info "Contradiction during map-of propagation" {})))
                                        env'))
                                    env')]
                         (or (tree/cd env' my-pos)
                             (throw (ex-info "map-of: lost position" {:pos my-pos}))))))

                   ;; Slow path: use narrow for non-primitive value types
                   (let [children (tree/children map-node)
                         env-root
                         (reduce
                          (fn [env-root child]
                            (let [k (::tree/name child)]
                              ;; Check key type
                              (when key-dom
                                (when-not (dom/contains-val? key-dom k)
                                  (throw (ex-info (str "map-of: key " k " does not match key type")
                                                  {:key k :key-type key-type-expr}))))
                              ;; Constrain value via narrow
                              (let [env-at-pos (tree/cd env-root my-pos)
                                    env-after (bind/bind env-at-pos
                                                         (list 'narrow (list 'get map-expr k) val-type-expr))]
                                (tree/root env-after))))
                          (tree/root env)
                          children)]
                     (or (tree/cd env-root my-pos)
                         (throw (ex-info "map-of: lost position" {:pos my-pos})))))))))})

      ;; ── narrow: explicit structural constraint ──────────────
      ;; (narrow target template) — constrains target against template.
      ;; Template is any domain schema expression: a type domain, a map/vector
      ;; of domains, a composite domain, or a named def-bound schema.
      ;; Reuses resolve-domain-schema + apply-domain-constraint.
      (defprim 'narrow
        {:bind
         (fn [env [target-expr template-expr]]
           (let [schema (bind/resolve-domain-schema env template-expr)]
             (bind/apply-domain-constraint env schema [target-expr])))})

      ;; ── isa: shorthand alias for narrow ─────────────────────
      ;; (isa x integer) ≡ (narrow x integer)
      (defprim 'isa
        {:bind
         (fn [env [target-expr template-expr]]
           (let [schema (bind/resolve-domain-schema env template-expr)]
             (bind/apply-domain-constraint env schema [target-expr])))})

      ;; ── def: named definition ────────────────────────────────
      ;; (def Person {:name string :age integer})   → just a named value
      ;; (def SmallInt (between 1 10))              → just a named value
      ;; (def pi 3.14159)                           → just a named value
      ;;
      ;; `def` just names a value. No domain interpretation, no callable behavior.
      ;; Use `(narrow target Schema)` to apply structural constraints.
      (defprim 'def
        {:bind
         (fn [env [def-name value-expr]]
           (let [my-pos (tree/position env)
                 env' (-> (tree/root env)
                          (tree/ensure-path (conj my-pos def-name))
                          (tree/cd (conj my-pos def-name)))]
             (let [result (bind/bind env' value-expr)]
               (-> (tree/root result)
                   (tree/put (conj my-pos def-name) :def-binding true)
                   (tree/cd my-pos)))))})

      ;; ── defn: named constructor / constraint function ────────
      ;; (defn point [x y] {:x (integer x) :y (integer y)})
      ;;
      ;; Expression position (constructor):
      ;;   (point 1 2)  → {:x 1 :y 2}
      ;;   Uses call-scope semantics: creates scope, binds params to args,
      ;;   evaluates body, returns result.
      ;;
      ;; Pattern position (destructor):
      ;;   (let [(point x y) {:x 1 :y 2}] [x y])  → [1 2]
      ;;   Inverts the constructor: uses the body as a destructuring pattern
      ;;   with params as binding targets.
      ;;
      ;; Also works as constraint function:
      ;;   (defn positive [x] (> x 0))
      ;;   (positive n)  — narrows n via constraints in call scope.
      (defprim 'defn
        {:bind
         (fn [env [cname params & body]]
           (let [body-expr (if (= 1 (count body))
                             (first body)
                             (cons 'do body))
                 my-pos (tree/position env)]
             (-> (tree/root env)
                 (tree/ensure-path (conj my-pos cname))
                 (tree/put (conj my-pos cname)
                           {:defn-constructor {:params params
                                               :body body-expr}
                            :bind (fn [call-env args]
                                    (if (:destructuring (meta args))
                                      ;; ── Pattern position: destructor ──
                                      ;; args = [pattern-args... value]
                                      ;; The body is used as a pattern against value,
                                      ;; with params mapped to the pattern-args.
                                      (let [value (last args)
                                            pattern-syms (vec (butlast args))
                                            ;; Parse params: support (type x) wrapped params
                                            raw-params (mapv (fn [p]
                                                               (if (seq? p)
                                                                 (let [sym (last p)]
                                                                   sym)
                                                                 p))
                                                             params)
                                            ;; Build substitution: param → pattern binding target
                                            substitution (zipmap raw-params pattern-syms)
                                            ;; For multi-body (do ...), split into constraints + value-producing expr.
                                            ;; Only the last form is used as the destructuring pattern.
                                            ;; Earlier forms are constraints applied after destructuring.
                                            [body-constraints body-value]
                                            (if (and (seq? body-expr) (= 'do (first body-expr)))
                                              [(butlast (rest body-expr)) (last body-expr)]
                                              [[] body-expr])
                                            ;; Substitute params in the value-producing part to get the destructure pattern
                                            destruct-pattern (bind/substitute-expr body-value substitution)
                                            ;; Substitute params in constraint forms too
                                            destruct-constraints (mapv #(bind/substitute-expr % substitution)
                                                                       body-constraints)
                                            ;; Collect param-level constraints: [(wrapper-head target-sym) ...]
                                            ;; e.g. (defn point [(integer x) (integer y)] {:x x :y y})
                                            ;; produces [[:integer a] [:integer b]]
                                            param-constraints (keep (fn [[p target]]
                                                                      (when (seq? p)
                                                                        [(first p) target]))
                                                                    (map vector params pattern-syms))]
                                        ;; First destructure using the body pattern
                                        (let [env' (bind/destructure call-env destruct-pattern value)
                                              ;; Apply body-level constraints (from multi-body defn)
                                              env' (reduce (fn [e c]
                                                             (let [e' (bind/bind e c)]
                                                               ;; Clear any link set by the constraint
                                                               (dissoc e' :link)))
                                                           env'
                                                           destruct-constraints)]
                                          ;; Then apply param-level type constraints as side-effects only
                                          ;; (no link setting — just narrow the target's domain)
                                          (reduce (fn [e [wrapper-head target-sym]]
                                                    (let [my-pos (tree/position e)
                                                          [e' path] (bind/ensure-node-abs e target-sym)
                                                          ;; Look up the wrapper (e.g. integer) to get its type-domain
                                                          wrapper-node (tree/find e' [wrapper-head])
                                                          wrapper-resolved (when wrapper-node (bind/resolve wrapper-node))
                                                          type-dom (:type-domain wrapper-resolved)]
                                                      (if type-dom
                                                        ;; Apply type domain constraint directly
                                                        (let [d (bind/domain-of (tree/root e') path)
                                                              narrowed (dom/intersect d type-dom)]
                                                          (if (dom/void? narrowed)
                                                            (throw (ex-info (str "Contradiction: destructuring type constraint")
                                                                            {:wrapper wrapper-head :target target-sym}))
                                                            (let [e'' (bind/set-domain (tree/root e') path narrowed)]
                                                              (or (tree/cd e'' my-pos) e''))))
                                                        ;; Not a type domain — fall back to bind (constraint call)
                                                        (let [constraint-expr (list wrapper-head target-sym)
                                                              e' (bind/bind e constraint-expr)]
                                                          ;; Clear any link that was set by the constraint call
                                                          (dissoc e' :link)))))
                                                  env'
                                                  param-constraints)))
                                      ;; ── Expression position: constructor ──
                                      ;; Uses call-scope semantics (like :mufl-fn)
                                      (do
                                        (when (>= bind/*call-depth* bind/max-call-depth)
                                          (throw (ex-info (str "Recursion depth limit exceeded (depth=" bind/*call-depth* ")")
                                                          {:depth bind/*call-depth* :fn cname})))
                                        (binding [bind/*call-depth* (inc bind/*call-depth*)]
                                          (let [_ (when (not= (count params) (count args))
                                                    (throw (ex-info (str "Arity mismatch: expected " (count params)
                                                                         " args, got " (count args))
                                                                    {:params params :args args :fn-name cname})))
                                                caller-pos (tree/position call-env)
                                                ;; Parse params: support (type x) wrapped params
                                                raw-params (mapv (fn [p]
                                                                   (if (seq? p)
                                                                     (let [sym (last p)]
                                                                       sym)
                                                                     p))
                                                                 params)
                                                ;; Evaluate arg expressions in the CALLER's scope first
                                                [env-after-args arg-paths]
                                                (reduce (fn [[e paths] arg-expr]
                                                          (let [[e' path] (bind/ensure-node-abs e arg-expr)]
                                                            [e' (conj paths path)]))
                                                        [call-env []]
                                                        args)
                                                ;; Create a call scope as child of current position
                                                call-name (gensym "defn_call_")
                                                env-with-scope (tree/ensure-path env-after-args [call-name])
                                                call-scope (tree/cd env-with-scope [call-name])
                                                ;; Link each raw param to the pre-evaluated arg node
                                                env-bound (reduce (fn [e [param arg-path]]
                                                                    (let [e (tree/ensure-path e [param])]
                                                                      (tree/upd e [param]
                                                                                #(assoc % :link arg-path))))
                                                                  call-scope
                                                                  (map vector raw-params arg-paths))
                                                ;; Apply param-level constraints if params have wrappers
                                                ;; e.g. (defn point [(integer x) (integer y)] ...)
                                                env-bound (reduce (fn [e [p arg-path]]
                                                                    (if (seq? p)
                                                                      (let [wrapper-head (first p)
                                                                            param-sym (last p)]
                                                                        (bind/bind e (list wrapper-head param-sym)))
                                                                      e))
                                                                  env-bound
                                                                  (map vector params arg-paths))
                                                ;; Create result sub-node and evaluate body there
                                                result-name (gensym "result_")
                                                env-with-result (tree/ensure-path env-bound [result-name])
                                                result-env (tree/cd env-with-result [result-name])
                                                result (bind/bind result-env body-expr)
                                                result-pos (tree/position result)]
                                            ;; The caller links to the result node.
                                            ;; Only set :link if result has a meaningful value
                                            ;; (map, vector, domain, or link to elsewhere).
                                            ;; For constraint-only calls like (> a b), the result
                                            ;; has no value — just side-effects via propagated constraints.
                                            (let [result-root (tree/root result)
                                                  caller (tree/cd result-root caller-pos)
                                                  target-path (or (:link result) result-pos)
                                                  result-node (tree/cd result-root target-path)
                                                  has-value? (or (:domain result-node)
                                                                 (:map result-node)
                                                                 (:vector result-node)
                                                                 (and (:link result-node)
                                                                      (not= (:link result-node) target-path)))]
                                              (if has-value?
                                                (assoc caller :link target-path)
                                                caller)))))))})
                 (tree/cd my-pos))))})

      ;; defdomain and defc are removed.
      ;; Use `def` for domain schemas, `defn` for constructors/constraints.

      ;; ── count: collection length ────────────────────────────
      ;; (count v) returns the number of elements in a vector or map.
      (defprim 'count
        {:bind
         (fn [env [coll-expr]]
           (let [coll-node (cond
                             (symbol? coll-expr)
                             (when-let [found (tree/find env [coll-expr])]
                               (bind/resolve found))

                             (seq? coll-expr)
                             (let [[env' path] (bind/ensure-node-abs env coll-expr)]
                               (bind/resolve-at (tree/root env') path))

                             :else
                             (throw (ex-info "count: arg must be a symbol or expression"
                                             {:coll coll-expr})))
                 _ (when-not coll-node
                     (throw (ex-info (str "count: cannot resolve: " coll-expr)
                                     {:coll coll-expr})))
                 n (cond
                     (:vector coll-node)
                     (clojure.core/count (filter #(integer? (::tree/name %))
                                                 (tree/children coll-node)))

                     (:map coll-node)
                     (clojure.core/count (tree/children coll-node))

                     :else
                     (throw (ex-info (str "count: not a collection: " coll-expr)
                                     {:coll coll-expr})))]
             (assoc env :domain (dom/single n))))})

      ;; ── map: apply function to each element ─────────────────
      ;; (map f coll) — applies f to each element of the vector,
      ;; returns a new vector with the results.
      ;; Unrolls at bind time since collection size is known.
      (defprim 'map
        {:bind
         (fn [env [f-expr coll-expr]]
           (let [;; Bind the function to a temp name so we can refer to it by symbol
                 f-sym (gensym "mapfn__")
                 env (bind/bind env f-sym f-expr)
                 ;; Resolve the collection (use the original coll-expr for lookup)
                 coll-node (cond
                             (symbol? coll-expr)
                             (when-let [found (tree/find env [coll-expr])]
                               (bind/resolve found))
                             (seq? coll-expr)
                             (let [[env' path] (bind/ensure-node-abs env coll-expr)]
                               (bind/resolve-at (tree/root env') path))
                             :else nil)
                 _ (when-not coll-node
                     (throw (ex-info "map: cannot resolve collection"
                                     {:coll coll-expr})))
                 _ (when-not (:vector coll-node)
                     (throw (ex-info "map: second argument must be a vector"
                                     {:coll coll-expr})))]
             ;; Build a vector expression: [(f e0) (f e1) ...]
             ;; where each ei is (nth coll-expr i)
             (let [children (sort-by ::tree/name
                                     (clojure.core/filter #(integer? (::tree/name %))
                                                          (tree/children coll-node)))
                   n (clojure.core/count children)
                   ;; Build the result as a vector of function applications
                   result-exprs (mapv (fn [i]
                                        (list f-sym (list 'nth coll-expr i)))
                                      (clojure.core/range n))]
               (bind/bind env (vec result-exprs)))))})

      ;; ── filter: keep elements satisfying predicate ──────────
      ;; (filter pred coll) — for each element, applies pred.
      ;; At bind time, tries the predicate: if contradiction → exclude,
      ;; if succeeds → include. Conservative: includes on uncertainty.
      ;; Returns a new vector of surviving elements.
      (defprim 'filter
        {:bind
         (fn [env [pred-expr coll-expr]]
           (let [;; Bind the predicate to a temp name
                 pred-sym (gensym "filtfn__")
                 env (bind/bind env pred-sym pred-expr)
                 ;; Resolve the collection
                 coll-node (cond
                             (symbol? coll-expr)
                             (when-let [found (tree/find env [coll-expr])]
                               (bind/resolve found))
                             (seq? coll-expr)
                             (let [[env' path] (bind/ensure-node-abs env coll-expr)]
                               (bind/resolve-at (tree/root env') path))
                             :else nil)
                 _ (when-not coll-node
                     (throw (ex-info "filter: cannot resolve collection"
                                     {:coll coll-expr})))
                 _ (when-not (:vector coll-node)
                     (throw (ex-info "filter: second argument must be a vector"
                                     {:coll coll-expr})))
                 children (sort-by ::tree/name
                                   (clojure.core/filter #(integer? (::tree/name %))
                                                        (tree/children coll-node)))
                 n (clojure.core/count children)]
             ;; For each element, try applying the predicate.
             ;; Include if it doesn't contradict.
             (let [surviving-indices
                   (vec (clojure.core/filter
                         (fn [i]
                           (try
                             (bind/bind env (list pred-sym (list 'nth coll-expr i)))
                             true ;; succeeded — include
                             (catch Exception _
                               false))) ;; contradicted — exclude
                         (clojure.core/range n)))
                   result-exprs (mapv (fn [i] (list 'nth coll-expr i))
                                      surviving-indices)]
               (bind/bind env (vec result-exprs)))))})

      ;; ── reduce: fold over collection ────────────────────────
      ;; (reduce f init coll) — iteratively applies (f acc elem).
      ;; Unrolls at bind time: (f (f (f init e0) e1) e2)
      (defprim 'reduce
        {:bind
         (fn [env [f-expr init-expr coll-expr]]
           (let [;; Bind the function to a temp name
                 f-sym (gensym "redfn__")
                 env (bind/bind env f-sym f-expr)
                 ;; Resolve the collection
                 coll-node (cond
                             (symbol? coll-expr)
                             (when-let [found (tree/find env [coll-expr])]
                               (bind/resolve found))
                             (seq? coll-expr)
                             (let [[env' path] (bind/ensure-node-abs env coll-expr)]
                               (bind/resolve-at (tree/root env') path))
                             :else nil)
                 _ (when-not coll-node
                     (throw (ex-info "reduce: cannot resolve collection"
                                     {:coll coll-expr})))
                 _ (when-not (:vector coll-node)
                     (throw (ex-info "reduce: third argument must be a vector"
                                     {:coll coll-expr})))
                 children (sort-by ::tree/name
                                   (clojure.core/filter #(integer? (::tree/name %))
                                                        (tree/children coll-node)))
                 n (clojure.core/count children)]
             ;; Unroll: (f (f (f init e0) e1) e2)
             (let [folded (clojure.core/reduce
                           (fn [acc-expr i]
                             (list f-sym acc-expr (list 'nth coll-expr i)))
                           init-expr
                           (clojure.core/range n))]
               (bind/bind env folded))))})

      ;; ════════════════════════════════════════════════════════
      ;; Destructuring operators (via `:bind` protocol)
      ;; ════════════════════════════════════════════════════════
      ;;
      ;; These operators work in pattern position within `let` bindings.
      ;; When `destructure` encounters `(head args...)` against a value,
      ;; it calls (:bind node) with (concat pattern-args [value]).
      ;; The :bind function uses (last args) for the value and
      ;; (butlast args) for the pattern elements.

      ;; ── ks: keyword-symbol map destructuring ────────────────
      ;; (ks a b c) against value → bind a to (:a value), b to (:b value), etc.
      (defprim 'ks
        {:bind
         (fn [env args]
           (let [value (last args)
                 syms (butlast args)
                 tmp (gensym "ks__")
                 env (bind/bind env tmp value)]
             (reduce (fn [e sym]
                       (bind/bind e sym (list 'get tmp (keyword sym))))
                     env
                     syms)))})

      ;; ── as: bind whole + inner destructure ──────────────────
      ;; (as m (ks x y)) → bind m to value, then destructure with inner pattern
      (defprim 'as
        {:bind
         (fn [env args]
           (let [value (last args)
                 [name-sym inner-pattern] (butlast args)
                 _ (when-not (symbol? name-sym)
                     (throw (ex-info (str "as: first argument must be a symbol, got: " (pr-str name-sym)
                                          " (type: " (type name-sym) ")")
                                     {:name name-sym :pattern inner-pattern})))
                 tmp (gensym "as__")
                 env (bind/bind env tmp value)
                 env (bind/bind env name-sym tmp)]
             (bind/destructure env inner-pattern tmp)))})

      ;; ── drop: sub-vector from index N onward ───────────────
      ;; (drop n v) → new vector containing elements from index n to end.
      ;; Generalized rest: (drop 1 v) gives elements from index 1 onward.
      ;; n must be a literal integer. Builds [(nth v n) (nth v n+1) ...].
      (defprim 'drop
        {:bind
         (fn [env [n-expr vec-expr]]
           (let [_ (when-not (integer? n-expr)
                     (throw (ex-info "drop: first arg must be a literal integer"
                                     {:n n-expr})))
                 ;; Resolve the vector
                 vec-node (cond
                            (symbol? vec-expr)
                            (when-let [found (tree/find env [vec-expr])]
                              (bind/resolve found))
                            (seq? vec-expr)
                            (let [[env' path] (bind/ensure-node-abs env vec-expr)]
                              (bind/resolve-at (tree/root env') path))
                            :else
                            (throw (ex-info "drop: second arg must be a symbol or expression"
                                            {:vec vec-expr})))
                 _ (when-not vec-node
                     (throw (ex-info (str "drop: cannot resolve: " vec-expr)
                                     {:vec vec-expr})))
                 _ (when-not (:vector vec-node)
                     (throw (ex-info (str "drop: not a vector node: " vec-expr)
                                     {:vec vec-expr})))
                 ;; Get sorted integer children (indices)
                 children (sort-by ::tree/name
                                   (clojure.core/filter #(integer? (::tree/name %))
                                                        (tree/children vec-node)))
                 total (clojure.core/count children)]
             (if (<= total n-expr)
               ;; Nothing left — return empty vector
               (assoc env :vector true)
               ;; Build rest vector: [(nth vec n) (nth vec n+1) ...]
               (let [rest-exprs (mapv (fn [i] (list 'nth vec-expr i))
                                      (clojure.core/range n-expr total))]
                 (bind/bind env (vec rest-exprs))))))})

      ;; ── dissoc: remove keys from a map ─────────────────────
      ;; (dissoc m :k1 :k2 ...) → new map node without the specified keys.
      ;; Builds a new map by linking to remaining children of the original.
      (defprim 'dissoc
        {:bind
         (fn [env [map-expr & key-exprs]]
           (let [;; Resolve the map
                 map-node (cond
                            (symbol? map-expr)
                            (when-let [found (tree/find env [map-expr])]
                              (bind/resolve found))
                            (seq? map-expr)
                            (let [[env' path] (bind/ensure-node-abs env map-expr)]
                              (bind/resolve-at (tree/root env') path))
                            :else
                            (throw (ex-info "dissoc: first arg must be a symbol or expression"
                                            {:map map-expr})))
                 _ (when-not map-node
                     (throw (ex-info (str "dissoc: cannot resolve: " map-expr)
                                     {:map map-expr})))
                 _ (when-not (:map map-node)
                     (throw (ex-info (str "dissoc: not a map node: " map-expr)
                                     {:map map-expr})))
                 remove-keys (set key-exprs)
                 ;; Get the remaining children (those not being removed)
                 remaining-children (clojure.core/filter
                                     (fn [child]
                                       (not (contains? remove-keys (::tree/name child))))
                                     (tree/children map-node))]
             ;; Create a new anonymous map node and link to it from current position.
             ;; This avoids polluting the current scope with keyword children.
             (let [anon (gensym "dissoc__")
                   my-pos (tree/position env)
                   env (tree/ensure-path env [anon])
                   anon-env (tree/cd env [anon])
                   ;; Build the map inside the anonymous node
                   anon-env (assoc anon-env :map true)
                   anon-env (reduce (fn [e child]
                                      (let [k (::tree/name child)
                                            child-target (bind/resolve child)
                                            target-path (tree/position child-target)]
                                        (-> (tree/ensure-path e [k])
                                            (tree/put [k] :link target-path))))
                                    anon-env
                                    remaining-children)
                   ;; Navigate back up to the caller position and link to the anonymous node
                   root (tree/root anon-env)
                   anon-abs-path (conj my-pos anon)
                   result (tree/cd root my-pos)]
               (assoc result :link anon-abs-path))))})))

