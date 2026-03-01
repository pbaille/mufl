(ns mufl.env
  "Base environment for mufl — operator registration and built-in forms.

   Operators are registered via `defop` with explicit capabilities:
   - :construct  — expression-position (forward bind)
   - :destruct   — pattern-position (backward destructuring)
   - :type-domain — type identity domain

   Built-in: one-of, let, and, =, !=, <, >, <=, >=, +, -, *, distinct, etc."
  (:require [mufl.tree :as tree]
            [mufl.domain :as dom]
            [mufl.bind :as bind]
            [mufl.schema :as schema]))

;; ════════════════════════════════════════════════════════════════
;; Operator registration primitives
;; ════════════════════════════════════════════════════════════════

(defn defop
  "Register an operator in the environment tree.

   Operators declare their capabilities via keyword fields:
   - :construct  — expression-position evaluation (forward direction)
   - :destruct   — pattern-position evaluation (backward direction)
   - :type-domain — the type domain this operator represents

   All operators get :primitive true automatically."
  [env name fields]
  (-> (tree/ensure-path env [name])
      (tree/put [name] (assoc fields :primitive true))))

(defn defops
  "Register multiple operators. specs is a seq of [name fields-map] pairs."
  [env specs]
  (reduce (fn [e [name fields]] (defop e name fields))
          env specs))

;; ════════════════════════════════════════════════════════════════
;; Helper functions for operator implementations
;; ════════════════════════════════════════════════════════════════

(defn- filter-pred-bind
  "Build a :construct fn for a domain-filter predicate (even?, odd?, pos?, neg?, zero?)."
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
  "Build a :construct fn for a type domain primitive (string, number, etc.).
   Expression-position only:
   - Nullary: (integer) → create fresh variable with appropriate domain.
     For numeric types (:integer, :number), uses an unbounded range
     so range-based narrowing works. Other types use the type domain.
   - Unary: (string x) → constrain x to string-dom AND link to it
   Pattern-position destructuring is handled by :destruct (see type-domain-destruct)."
  [type-dom label]
  (let [numeric? (#{:integer :number} (:type type-dom))
        ;; For numeric types, the working representation is an unbounded
        ;; range — this enables range-based narrowing by the constraint
        ;; solver. The type domain {:kind :type} is used for intersection
        ;; algebra, but nodes should hold ranges for numeric types.
        ensure-numeric-range
        (fn [d] (if (and numeric? (= :type (:kind d)))
                  (dom/range-dom nil nil)
                  d))]
    (fn [env args]
      (if (seq args)
        ;; Unary: (string x) → constrain arg to type domain + link to it
        (let [[arg] args
              [env' path] (bind/ensure-node-abs env arg)
              d (bind/domain-of (tree/root env') path)
              narrowed (ensure-numeric-range (dom/intersect d type-dom))]
          (if (dom/void? narrowed)
            (throw (ex-info (str "Contradiction: no " label " values") {:arg arg}))
            (let [env' (bind/set-domain (tree/root env') path narrowed)
                  caller (or (tree/cd env' (tree/position env)) env')]
              ;; Link caller to the constrained arg so (integer x) evaluates to x
              (assoc caller :link path))))
        ;; Nullary: (integer) → fresh variable with domain
        (assoc env :domain (if numeric?
                             (dom/range-dom nil nil)
                             type-dom))))))

(defn- type-domain-destruct
  "Build a :destruct fn for type domain primitives.
   (integer x) in pattern position → narrow seed-domain by intersecting with
   the type domain, then recurse bind-pattern on the inner pattern."
  [type-dom label]
  (fn [env [inner-pattern] seed-sym seed-domain]
    (let [narrowed (dom/intersect (or seed-domain dom/any) type-dom)]
      (if (dom/void? narrowed)
        (throw (ex-info (str "Type guard contradiction in pattern: " label)
                        {:type-dom type-dom :seed-domain seed-domain}))
        (bind/bind-pattern env inner-pattern seed-sym narrowed)))))

;; Type resolution for constructors uses schema/resolve-domain-schema directly.
;; This is the single canonical path: source expression → schema map.
;; No separate resolve-type-expr / resolve-type-schema needed.

;; ════════════════════════════════════════════════════════════════
;; Registration groups
;; ════════════════════════════════════════════════════════════════

;; ── Core forms ────────────────────────────────────────────────

(defn- register-core-forms
  "Register core language forms: one-of, let, fresh, and, do, or, not, if, when, cond, fn."
  [env]
  (-> env

      ;; ── one-of: create a finite domain ──────────────────────
      (defop 'one-of
        {:construct
         (fn [env args]
           (assoc env :domain (dom/finite (set args))))})

      ;; ── let: sequential bindings with body ──────────────────
      ;; Supports symbol bindings (let [x 1] ...) and destructuring
      ;; patterns: {:x a :y b} for maps, [a b c] for vectors.
      (defop 'let
        {:construct
         (fn [env [bindings-vec & body]]
           (let [pairs (partition 2 bindings-vec)
                 env' (reduce (fn [e [pattern expr]]
                                (cond
                                  (symbol? pattern)
                                  (bind/bind e pattern expr)

                                  (or (map? pattern) (vector? pattern) (seq? pattern))
                                  ;; Use bind-pattern directly for domain propagation
                                  (let [seed-sym (gensym "seed_")
                                        e' (bind/bind e seed-sym expr)
                                        seed-domain (bind/domain-of e' [seed-sym])
                                        [e'' _bound-syms] (bind/bind-pattern e' pattern seed-sym seed-domain)]
                                    e'')

                                  :else
                                  (throw (ex-info "Invalid destructuring pattern" {:pattern pattern}))))
                              env pairs)]
             (if (= 1 (count body))
               (bind/bind env' (first body))
               (reduce (fn [e expr] (bind/bind e expr)) env' body))))})

      ;; ── fresh: introduce logic variables ─────────────────────
      ;; (fresh [x y] body...)           → free variables, body as and
      ;; (fresh [(integer x) y] body...) → typed + free, body as and
      ;;
      ;; Each declaration is either:
      ;;   - bare symbol   → (free) domain
      ;;   - (type sym)    → (type) domain (uses nullary constructor)
      ;;
      ;; Body expressions are implicit and (last is return value).
      ;; Non-final body expressions are treated as constraints:
      ;; their tree mutations (domain narrows) are kept, but any
      ;; :link they set on the workspace is cleared so subsequent
      ;; expressions start from the same position.
      (defop 'fresh
        {:construct
         (fn [env [decls & body]]
           (let [;; Create nodes: typed declarations use the type's nullary
                 ;; form directly (e.g. (integer) → range(nil,nil));
                 ;; bare symbols use (free).
                 env' (reduce
                       (fn [e decl]
                         (cond
                           (symbol? decl)
                           (bind/bind e decl '(free))

                           (seq? decl)
                           (let [[type-name sym] decl]
                             (bind/bind e sym (list type-name)))

                           :else
                           (throw (ex-info "Invalid fresh declaration"
                                           {:decl decl}))))
                       env decls)
                 my-pos (tree/position env')
                 ;; Non-final body: bind as constraints, restore position
                 env' (reduce (fn [e expr]
                                (let [result (bind/bind e expr)]
                                  (-> (tree/cd (tree/root result) my-pos)
                                      (dissoc :link))))
                              env' (butlast body))]
             ;; Final expression: bind as return value
             (if (seq body)
               (bind/bind env' (last body))
               env')))})

      ;; ── and: all constraints must hold, last is return ──────
      (defop 'and
        {:construct
         (fn [env args]
           (reduce (fn [e arg]
                     (let [result (bind/bind e arg)]
                       ;; If the sub-bind produced a constraint (no domain on current pos),
                       ;; we carry on. If it set a domain, that's the return value intent.
                       result))
                   env args))})

      ;; ── do: sequence of expressions, return last ────────────
      (defop 'do
        {:construct
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
      (defop 'or
        {:destruct
         (fn [env args seed-sym seed-domain]
           ;; Try each pattern in order, take the first that succeeds.
           ;; (or pat1 pat2 ...) against seed → first matching pattern wins.
           (loop [[pat & rest-pats] args]
             (if (nil? pat)
               (throw (ex-info "All or-patterns failed in destructuring"
                               {:args args :seed seed-sym}))
               (if-let [result (try (bind/bind-pattern env pat seed-sym seed-domain)
                                    (catch Exception _ nil))]
                 result
                 (recur rest-pats)))))
         :construct
         (fn [env args]
           ;; Expression context: domain union.
           ;; Pattern-position destructuring is handled by :destruct above.
           (let [pos (tree/position env)
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
               (let [all-paths (->> branch-envs
                                    (mapcat (fn [e]
                                              (->> (tree/children e)
                                                   (keep (fn [child]
                                                           (when (and (:domain child)
                                                                      (not (:constraint child))
                                                                      (not (:primitive child)))
                                                             (tree/position child)))))))
                                    distinct)
                     env-root (tree/root env)
                     env-root
                     (reduce
                      (fn [env-root vpath]
                        (let [unioned (->> branch-envs
                                           (map (fn [e]
                                                  (let [node (tree/at (tree/root e) vpath)]
                                                    (or (:domain node) dom/any))))
                                           (reduce dom/unite dom/void))
                              current (bind/domain-of env-root vpath)
                              narrowed (dom/intersect current unioned)]
                          (if (dom/void? narrowed)
                            (throw (ex-info "Contradiction in or" {:path vpath}))
                            (bind/set-domain env-root vpath narrowed))))
                      env-root
                      all-paths)]
                 (or (tree/cd env-root pos) env-root)))))})

      ;; ── not: constraint negation ────────────────────────────
      ;; (not (= x y)) → (!= x y), etc.
      (defop 'not
        {:construct
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
      (defop 'if
        {:construct
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
      (defop 'when
        {:construct
         (fn [env [test & body]]
           (bind/bind env (list 'if test (cons 'do body) nil)))})

      ;; ── cond: chained conditionals ──────────────────────────
      ;; Like if, tries eager resolution. If exactly one branch is satisfiable
      ;; and all others contradict, takes it eagerly. Otherwise defers to solve.
      (defop 'cond
        {:construct
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

      ;; ── fn: multi-branch function ─────────────────────────────
      ;; Single branch:  (fn [params] body-expr)
      ;; Multi-branch:   (fn [p1] expr1 [p2] expr2 ...)
      ;; With default:   (fn [p1] expr1 [p2] expr2 ... default-expr)
      ;; Params support full pattern language: symbols, type wrappers,
      ;; literals, maps, vectors.
      (defop 'fn
        {:construct
         (fn [env forms]
           ;; Parse forms into branches: each vector starts a new branch,
           ;; followed by exactly one body expression. Optional trailing
           ;; non-vector expression is the default.
           (let [parsed (loop [forms (seq forms)
                               branches []]
                          (cond
                            (nil? forms)
                            {:branches branches :default nil}

                            (vector? (first forms))
                            (if (nil? (next forms))
                              (throw (ex-info "fn branch missing body expression"
                                              {:params (first forms)}))
                              (recur (nnext forms)
                                     (conj branches {:params (first forms)
                                                     :body (second forms)})))

                            ;; Non-vector form — must be the last form (default)
                            (nil? (next forms))
                            {:branches branches :default (first forms)}

                            :else
                            (throw (ex-info "Unexpected form in fn body — expected vector (branch) or single default expression"
                                            {:form (first forms) :remaining (vec forms)}))))]
             (assoc env :mufl-fn parsed)))})))

;; ── Relational operators ──────────────────────────────────────

(defn- register-relational
  "Register relational constraints: <, >, <=, >=, =, !=."
  [env]
  (defops env (for [[sym op] [['< :<] ['> :>] ['<= :<=] ['>= :>=] ['= :=] ['!= :!=]]]
                [sym {:construct (fn [env args] (bind/bind-relational env op args))}])))

;; ── Arithmetic operators ──────────────────────────────────────

(defn- register-arithmetic
  "Register arithmetic operators: +, -, *, mod, quot, between, abs, min, max."
  [env]
  (-> env

      (defop '+
        {:construct (fn [env args]
                      (bind/bind-arithmetic env :+ args))})

      (defop '-
        {:construct (fn [env args]
                      (bind/bind-arithmetic env :- args))})

      (defop '*
        {:construct (fn [env args]
                      (bind/bind-arithmetic env :* args))})

      ;; ── mod / quot ──────────────────────────────────────────

      (defop 'mod
        {:construct (fn [env args]
                      (bind/bind-arithmetic env :mod args))})

      (defop 'quot
        {:construct (fn [env args]
                      (bind/bind-arithmetic env :quot args))})

      ;; ── between: integer range domain ──────────────────────
      (defop 'between
        {:construct
         (fn [env [lo hi]]
           (assoc env :domain (dom/int-range lo hi)))})

      ;; ── abs: absolute value ─────────────────────────────────
      ;; Implemented as a constraint: |a| = c
      ;; Narrowing: c must be in {|a| for a in A}; a must be in {a : |a| in C}
      (defop 'abs
        {:construct
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
      (defop 'min
        {:construct
         (fn [env [a-expr b-expr]]
           (let [[env' a-path] (bind/ensure-node-abs env a-expr)
                 [env'' b-path] (bind/ensure-node-abs env' b-expr)
                 a-dom (bind/domain-of (tree/root env'') a-path)
                 b-dom (bind/domain-of (tree/root env'') b-path)]
             (if (and (dom/finite? a-dom) (dom/finite? b-dom))
               (let [result-dom (dom/finite (set (for [a (dom/members a-dom)
                                                       b (dom/members b-dom)]
                                                   (min a b))))]
                 (assoc (or (tree/cd (tree/root env'') (tree/position env)) env'')
                        :domain result-dom))
               (assoc env :domain dom/any))))})

      (defop 'max
        {:construct
         (fn [env [a-expr b-expr]]
           (let [[env' a-path] (bind/ensure-node-abs env a-expr)
                 [env'' b-path] (bind/ensure-node-abs env' b-expr)
                 a-dom (bind/domain-of (tree/root env'') a-path)
                 b-dom (bind/domain-of (tree/root env'') b-path)]
             (if (and (dom/finite? a-dom) (dom/finite? b-dom))
               (let [result-dom (dom/finite (set (for [a (dom/members a-dom)
                                                       b (dom/members b-dom)]
                                                   (max a b))))]
                 (assoc (or (tree/cd (tree/root env'') (tree/position env)) env'')
                        :domain result-dom))
               (assoc env :domain dom/any))))})))

;; ── Type system operators ─────────────────────────────────────

(defn- register-type-system
  "Register type domain primitives and structural type constructors."
  [env]
  (-> env

      ;; ── Type domain primitives ─────────────────────────────
      ;; string, integer, number, keyword, boolean resolve to type domains.
      ;; Used in defdomain specs ({:name string}) and as constraints:
      ;;   Nullary: (string) → sets own domain to string-dom
      ;;   Unary:   (string x) → constrains x to string-dom
      ;; This unifies type domains with type predicates — no need for
      ;; separate string?/number?/keyword? primitives.
      ;;
      ;; In let-binding position, bare type names create fresh variables:
      ;;   (let [x integer] ...) → x is a fresh integer variable
      ;;   (let [s string] ...)  → s is a fresh string variable
      (defops (for [[sym type-dom label] [['string  dom/string-dom  "string"]
                                          ['integer dom/integer-dom "integer"]
                                          ['number  dom/number-dom  "number"]
                                          ['keyword dom/keyword-dom "keyword"]
                                          ['boolean dom/boolean-dom "boolean"]]]
                [sym {:construct (type-domain-bind type-dom label)
                      :type-domain type-dom
                      :destruct (type-domain-destruct type-dom label)}]))

      ;; ── free: unconstrained variable ────────────────────────
      ;; (let [x free] ...) creates a variable with no type constraint.
      ;; Useful when the type will be narrowed later by constraints.
      ;; In expression context: (free) sets own domain to any.
      (defop 'free
        {:type-domain dom/any
         :construct (fn [env _args] (assoc env :domain dom/any))})

      ;; ── Type constructors: vector-of, tuple, map-of ────────
      ;; These are bind-time unrolling constraints, consistent with
      ;; how map/filter/reduce already handle collections. They walk
      ;; the structural tree at bind time and constrain each element.

      ;; ── vector-of: produce a composite vector domain ──────
      ;; (vector-of integer) → composite domain where every element is integer.
      ;; Use with narrow: (narrow v (vector-of integer))
      ;; In def: (def IntVec (vector-of integer))
      (defop 'vector-of
        {:construct
         (fn [env args]
           (when (not= (count args) 1)
             (throw (ex-info "vector-of requires exactly 1 argument: (vector-of type). Use (narrow v (vector-of type)) to constrain a vector."
                             {:args args})))
           (let [elem-domain (schema/resolve-domain-schema env (first args))
                 composite (dom/vector-of-dom elem-domain)]
             (assoc env :domain composite :type-domain composite)))})

      ;; ── tuple: produce a composite tuple domain ─────────────
      ;; (tuple [integer string]) → composite domain with per-position types.
      ;; Use with narrow: (narrow v (tuple [integer string]))
      ;; In def: (def Point (tuple [number number]))
      (defop 'tuple
        {:construct
         (fn [env args]
           (when (not= (count args) 1)
             (throw (ex-info "tuple requires exactly 1 argument: (tuple [types...]). Use (narrow v (tuple [types...])) to constrain a vector."
                             {:args args})))
           (let [types-vec (first args)
                 _ (when-not (vector? types-vec)
                     (throw (ex-info "tuple: first argument must be a vector of types"
                                     {:types types-vec})))
                 elem-domains (mapv #(schema/resolve-domain-schema env %) types-vec)
                 composite (dom/tuple-dom elem-domains)]
             (assoc env :domain composite :type-domain composite)))})

      ;; ── map-of: produce a composite map domain ─────────────
      ;; (map-of keyword integer) → composite domain for map entries.
      ;; Use with narrow: (narrow m (map-of keyword integer))
      ;; In def: (def Scores (map-of keyword integer))
      (defop 'map-of
        {:construct
         (fn [env args]
           (when (not= (count args) 2)
             (throw (ex-info "map-of requires exactly 2 arguments: (map-of key-type val-type). Use (narrow m (map-of key-type val-type)) to constrain a map."
                             {:args args})))
           (let [key-domain (schema/resolve-domain-schema env (first args))
                 val-domain (schema/resolve-domain-schema env (second args))
                 composite (dom/map-of-dom key-domain val-domain)]
             (assoc env :domain composite :type-domain composite)))})

      ;; ── narrow: explicit structural constraint ──────────────
      ;; (narrow target template) — constrains target against template.
      ;; Template is any domain schema expression: a type domain, a map/vector
      ;; of domains, a composite domain, or a named def-bound schema.
      ;; Reuses resolve-domain-schema + apply-domain-constraint.
      (defop 'narrow
        {:construct
         (fn [env [target-expr template-expr]]
           (let [schema (bind/resolve-domain-schema env template-expr)]
             (bind/apply-domain-constraint env schema [target-expr])))})

      ;; ── isa: shorthand alias for narrow ─────────────────────
      ;; (isa x integer) ≡ (narrow x integer)
      (defop 'isa
        {:construct
         (fn [env [target-expr template-expr]]
           (let [schema (bind/resolve-domain-schema env template-expr)]
             (bind/apply-domain-constraint env schema [target-expr])))})))

;; ── Definition forms ──────────────────────────────────────────

(defn- register-definitions
  "Register def and defn forms."
  [env]
  (-> env

      ;; ── def: named definition ────────────────────────────────
      ;; (def Person {:name string :age integer})   → just a named value
      ;; (def SmallInt (between 1 10))              → just a named value
      ;; (def pi 3.14159)                           → just a named value
      ;;
      ;; `def` just names a value. No domain interpretation, no callable behavior.
      ;; Use `(narrow target Schema)` to apply structural constraints.
      (defop 'def
        {:construct
         (fn [env [def-name value-expr]]
           (let [my-pos (tree/position env)
                 env' (-> (tree/root env)
                          (tree/ensure-path (conj my-pos def-name))
                          (tree/cd (conj my-pos def-name)))]
             (let [result (bind/bind env' value-expr)]
               (-> (tree/root result)
                   (tree/put (conj my-pos def-name) :def-binding true)
                   (tree/cd my-pos)))))})

      ;; ── defn: sugar for (def name (fn [params] body...)) ─────
      ;; (defn point [x y] {:x (integer x) :y (integer y)})
      ;; ≡ (def point (fn [x y] {:x (integer x) :y (integer y)}))
      ;;
      ;; Since fn is bidirectional (call in expression position,
      ;; destructure in pattern position), defn inherits both.
      (defop 'defn
        {:construct
         (fn [env [cname params & body]]
           (bind/bind env (list 'def cname (list* 'fn params body))))})))

      ;; defdomain and defc are removed.
      ;; Use `def` for domain schemas, `defn` for constructors/constraints.

;; ── Collection operators ──────────────────────────────────────

(defn- register-collections
  "Register collection operators: get, nth, count, map, filter, reduce, drop, dissoc, distinct,
   first, rest, take, concat, select-keys, merge, update."
  [env]
  (-> env

      ;; ── distinct (all-different) ────────────────────────────
      (defop 'distinct
        {:construct
         (fn [env [vec-expr]]
           ;; vec-expr should be a vector literal of symbols
           (let [my-pos (tree/position env)
                 paths (mapv (fn [sym]
                               (let [p (when (symbol? sym)
                                         (when-let [found (tree/find env [sym])]
                                           (tree/position (bind/resolve found))))]
                                 (when-not p
                                   (throw (ex-info (str "Cannot resolve in distinct: " sym)
                                                   {:sym sym})))
                                 p))
                             vec-expr)
                 env-root (bind/add-constraint (tree/root env) :alldiff paths my-pos)]
             (or (and env-root (tree/cd env-root my-pos))
                 (throw (ex-info "Contradiction in distinct" {:args vec-expr})))))})

      ;; ── get: map key access ─────────────────────────────────
      ;; (get m :key) navigates to the :key child of map node m.
      ;; Creates a link so constraints propagate back to the child.
      (defop 'get
        {:construct
         (fn [env [coll-expr key-expr]]
           (let [[env coll-node] (bind/resolve-collection env coll-expr :map "get")
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
      (defop 'nth
        {:construct
         (fn [env [vec-expr idx-expr]]
           (let [[env vec-node] (bind/resolve-collection env vec-expr :vector "nth")
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

      ;; ── count: collection length ────────────────────────────
      ;; (count v) returns the number of elements in a vector or map.
      (defop 'count
        {:construct
         (fn [env [coll-expr]]
           (let [[env coll-node] (bind/resolve-to-node env coll-expr "count")
                 n (cond
                     (:vector coll-node)
                     (count (tree/int-children coll-node))

                     (:map coll-node)
                     (count (tree/children coll-node))

                     :else
                     (throw (ex-info (str "count: not a collection: " coll-expr)
                                     {:coll coll-expr})))]
             (assoc env :domain (dom/single n))))})

      ;; ── map: apply function to each element ─────────────────
      ;; (map f coll) — applies f to each element of the vector,
      ;; returns a new vector with the results.
      ;; Unrolls at bind time since collection size is known.
      (defop 'map
        {:construct
         (fn [env [f-expr coll-expr]]
           (let [;; Bind the function to a temp name so we can refer to it by symbol
                 f-sym (gensym "mapfn__")
                 env (bind/bind env f-sym f-expr)
                 ;; Resolve the collection (use the original coll-expr for lookup)
                 [env coll-node] (bind/resolve-collection env coll-expr :vector "map")]
             ;; Build a vector expression: [(f e0) (f e1) ...]
             ;; where each ei is (nth coll-expr i)
             (let [children (tree/int-children coll-node)
                   n (count children)
                   ;; Build the result as a vector of function applications
                   result-exprs (mapv (fn [i]
                                        (list f-sym (list 'nth coll-expr i)))
                                      (range n))]
               (bind/bind env (vec result-exprs)))))})

      ;; ── filter: keep elements satisfying predicate ──────────
      ;; (filter pred coll) — for each element, applies pred.
      ;; At bind time, tries the predicate: if contradiction → exclude,
      ;; if succeeds → include. Conservative: includes on uncertainty.
      ;; Returns a new vector of surviving elements.
      (defop 'filter
        {:construct
         (fn [env [pred-expr coll-expr]]
           (let [;; Bind the predicate to a temp name
                 pred-sym (gensym "filtfn__")
                 env (bind/bind env pred-sym pred-expr)
                 ;; Resolve the collection
                 [env coll-node] (bind/resolve-collection env coll-expr :vector "filter")
                 children (tree/int-children coll-node)
                 n (count children)]
             ;; For each element, try applying the predicate.
             ;; Include if it doesn't contradict.
             (let [surviving-indices
                   (vec (filter
                         (fn [i]
                           (try
                             (bind/bind env (list pred-sym (list 'nth coll-expr i)))
                             true ;; succeeded — include
                             (catch Exception _
                               false))) ;; contradicted — exclude
                         (range n)))
                   result-exprs (mapv (fn [i] (list 'nth coll-expr i))
                                      surviving-indices)]
               (bind/bind env (vec result-exprs)))))})

      ;; ── reduce: fold over collection ────────────────────────
      ;; (reduce f init coll) — iteratively applies (f acc elem).
      ;; Unrolls at bind time: (f (f (f init e0) e1) e2)
      (defop 'reduce
        {:construct
         (fn [env [f-expr init-expr coll-expr]]
           (let [;; Bind the function to a temp name
                 f-sym (gensym "redfn__")
                 env (bind/bind env f-sym f-expr)
                 ;; Resolve the collection
                 [env coll-node] (bind/resolve-collection env coll-expr :vector "reduce")
                 children (tree/int-children coll-node)
                 n (count children)]
             ;; Unroll: (f (f (f init e0) e1) e2)
             (let [folded (reduce
                           (fn [acc-expr i]
                             (list f-sym acc-expr (list 'nth coll-expr i)))
                           init-expr
                           (range n))]
               (bind/bind env folded))))})

      ;; ── drop: sub-vector from index N onward ───────────────
      ;; (drop n v) → new vector containing elements from index n to end.
      ;; Generalized rest: (drop 1 v) gives elements from index 1 onward.
      ;; n must be a literal integer. Builds [(nth v n) (nth v n+1) ...].
      (defop 'drop
        {:construct
         (fn [env [n-expr vec-expr]]
           (let [_ (when-not (integer? n-expr)
                     (throw (ex-info "drop: first arg must be a literal integer"
                                     {:n n-expr})))
                 ;; Resolve the vector
                 [env vec-node] (bind/resolve-collection env vec-expr :vector "drop")
                 children (tree/int-children vec-node)
                 total (count children)]
             (if (<= total n-expr)
               ;; Nothing left — return empty vector
               (assoc env :vector true)
               ;; Build rest vector: [(nth vec n) (nth vec n+1) ...]
               (let [rest-exprs (mapv (fn [i] (list 'nth vec-expr i))
                                      (range n-expr total))]
                 (bind/bind env (vec rest-exprs))))))})

      ;; ── dissoc: remove keys from a map ─────────────────────
      ;; (dissoc m :k1 :k2 ...) → new map node without the specified keys.
      ;; Builds a new map by linking to remaining children of the original.
      (defop 'dissoc
        {:construct
         (fn [env [map-expr & key-exprs]]
           (let [[env map-node] (bind/resolve-collection env map-expr :map "dissoc")
                 remove-keys (set key-exprs)
                 ;; Get the remaining children (those not being removed)
                 remaining-children (filter
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
               (assoc result :link anon-abs-path))))})

      ;; ── first: first element of a vector ────────────────────
      ;; (first v) ≡ (nth v 0) — sugar for the common case.
      (defop 'first
        {:construct
         (fn [env [vec-expr]]
           (bind/bind env (list 'nth vec-expr 0)))})

      ;; ── rest: all but first element ─────────────────────────
      ;; (rest v) ≡ (drop 1 v) — returns vector from index 1 onward.
      (defop 'rest
        {:construct
         (fn [env [vec-expr]]
           (bind/bind env (list 'drop 1 vec-expr)))})

      ;; ── take: first N elements ──────────────────────────────
      ;; (take n v) → vector of first n elements. Complement to `drop`.
      ;; n must be a literal integer. Builds [(nth v 0) ... (nth v n-1)].
      (defop 'take
        {:construct
         (fn [env [n-expr vec-expr]]
           (when-not (integer? n-expr)
             (throw (ex-info "take: first arg must be a literal integer"
                             {:n n-expr})))
           (let [[env vec-node] (bind/resolve-collection env vec-expr :vector "take")
                 total (count (tree/int-children vec-node))
                 actual-n (max 0 (min n-expr total))]
             (if (zero? actual-n)
               (assoc env :vector true)
               (let [result-exprs (mapv (fn [i] (list 'nth vec-expr i))
                                        (range actual-n))]
                 (bind/bind env (vec result-exprs))))))})

      ;; ── concat: join two vectors ────────────────────────────
      ;; (concat v1 v2) → new vector with all elements of v1 then v2.
      ;; Unrolls both sources: [(nth v1 0) ... (nth v1 n1-1) (nth v2 0) ... (nth v2 n2-1)].
      (defop 'concat
        {:construct
         (fn [env [v1-expr v2-expr]]
           (let [[env v1-node] (bind/resolve-collection env v1-expr :vector "concat")
                 [env v2-node] (bind/resolve-collection env v2-expr :vector "concat")
                 n1 (count (tree/int-children v1-node))
                 n2 (count (tree/int-children v2-node))
                 v1-elems (mapv (fn [i] (list 'nth v1-expr i)) (range n1))
                 v2-elems (mapv (fn [i] (list 'nth v2-expr i)) (range n2))
                 result (vec (clojure.core/concat v1-elems v2-elems))]
             (bind/bind env result)))})

      ;; ── select-keys: keep specified keys from a map ─────────
      ;; (select-keys m :k1 :k2 ...) → new map with only the specified keys.
      ;; Like dissoc but inverted. Uses the anonymous-node-with-links pattern.
      (defop 'select-keys
        {:construct
         (fn [env [map-expr & key-exprs]]
           (let [[env map-node] (bind/resolve-collection env map-expr :map "select-keys")
                 keep-keys (set key-exprs)
                 _ (doseq [k key-exprs]
                     (when-not (keyword? k)
                       (throw (ex-info "select-keys: keys must be literal keywords"
                                       {:key k}))))
                 kept-children (filter
                                (fn [child]
                                  (contains? keep-keys (::tree/name child)))
                                (tree/children map-node))]
             (let [anon (gensym "selkeys__")
                   my-pos (tree/position env)
                   env (tree/ensure-path env [anon])
                   anon-env (tree/cd env [anon])
                   anon-env (assoc anon-env :map true)
                   anon-env (reduce (fn [e child]
                                      (let [k (::tree/name child)
                                            child-target (bind/resolve child)
                                            target-path (tree/position child-target)]
                                        (-> (tree/ensure-path e [k])
                                            (tree/put [k] :link target-path))))
                                    anon-env
                                    kept-children)
                   root (tree/root anon-env)
                   anon-abs-path (conj my-pos anon)
                   result (tree/cd root my-pos)]
               (assoc result :link anon-abs-path))))})

      ;; ── merge: combine two maps (right-biased) ─────────────
      ;; (merge m1 m2) → new map with keys from both; m2 wins on conflicts.
      ;; All keys from m1 not in m2, plus all keys from m2.
      (defop 'merge
        {:construct
         (fn [env [m1-expr m2-expr]]
           (let [[env m1-node] (bind/resolve-collection env m1-expr :map "merge")
                 [env m2-node] (bind/resolve-collection env m2-expr :map "merge")
                 m1-children (tree/kw-children m1-node)
                 m2-children (tree/kw-children m2-node)
                 m2-keys (set (map ::tree/name m2-children))
                 ;; m1 children not overridden by m2
                 m1-only (filter #(not (contains? m2-keys (::tree/name %)))
                                 m1-children)]
             (let [anon (gensym "merge__")
                   my-pos (tree/position env)
                   env (tree/ensure-path env [anon])
                   anon-env (tree/cd env [anon])
                   anon-env (assoc anon-env :map true)
                   ;; Add m1-only children (link to m1)
                   anon-env (reduce (fn [e child]
                                      (let [k (::tree/name child)
                                            target-path (tree/position (bind/resolve child))]
                                        (-> (tree/ensure-path e [k])
                                            (tree/put [k] :link target-path))))
                                    anon-env
                                    m1-only)
                   ;; Add all m2 children (link to m2, overrides m1)
                   anon-env (reduce (fn [e child]
                                      (let [k (::tree/name child)
                                            target-path (tree/position (bind/resolve child))]
                                        (-> (tree/ensure-path e [k])
                                            (tree/put [k] :link target-path))))
                                    anon-env
                                    m2-children)
                   root (tree/root anon-env)
                   anon-abs-path (conj my-pos anon)
                   result (tree/cd root my-pos)]
               (assoc result :link anon-abs-path))))})

      ;; ── last: last element of a vector ──────────────────────
      ;; (last v) ≡ (nth v (dec (count children))) — sugar for last element.
      (defop 'last
        {:construct
         (fn [env [vec-expr]]
           (let [[env vec-node] (bind/resolve-collection env vec-expr :vector "last")
                 children (tree/int-children vec-node)
                 n (count children)]
             (when (zero? n)
               (throw (ex-info "last: empty vector" {:vec vec-expr})))
             (bind/bind env (list 'nth vec-expr (dec n)))))})

      ;; ── conj: append element to a vector ────────────────────
      ;; (conj v elem) → new vector with elem appended at the end.
      ;; Unrolls: [(nth v 0) ... (nth v n-1) elem]
      (defop 'conj
        {:construct
         (fn [env [vec-expr elem-expr]]
           (let [[env vec-node] (bind/resolve-collection env vec-expr :vector "conj")
                 children (tree/int-children vec-node)
                 n (count children)
                 existing-elems (mapv (fn [i] (list 'nth vec-expr i)) (range n))
                 result (conj existing-elems elem-expr)]
             (bind/bind env result)))})

      ;; ── reverse: reverse a vector ───────────────────────────
      ;; (reverse v) → new vector with elements in reverse order.
      ;; Unrolls: [(nth v n-1) ... (nth v 0)]
      (defop 'reverse
        {:construct
         (fn [env [vec-expr]]
           (let [[env vec-node] (bind/resolve-collection env vec-expr :vector "reverse")
                 children (tree/int-children vec-node)
                 n (count children)
                 result-exprs (mapv (fn [i] (list 'nth vec-expr i))
                                    (range (dec n) -1 -1))]
             (bind/bind env (vec result-exprs))))})

      ;; ── zip: pair two vectors element-wise ──────────────────
      ;; (zip v1 v2) → [[e1_0 e2_0] [e1_1 e2_1] ...]
      ;; Vectors must have the same length.
      (defop 'zip
        {:construct
         (fn [env [v1-expr v2-expr]]
           (let [[env v1-node] (bind/resolve-collection env v1-expr :vector "zip")
                 [env v2-node] (bind/resolve-collection env v2-expr :vector "zip")
                 n1 (count (tree/int-children v1-node))
                 n2 (count (tree/int-children v2-node))]
             (when (not= n1 n2)
               (throw (ex-info (str "zip: vectors must have same length, got " n1 " and " n2)
                               {:v1 v1-expr :v2 v2-expr :n1 n1 :n2 n2})))
             (let [result-exprs (mapv (fn [i]
                                        [(list 'nth v1-expr i)
                                         (list 'nth v2-expr i)])
                                      (range n1))]
               (bind/bind env (vec result-exprs)))))})

      ;; ── keys: extract key names from a map as a vector ──────
      ;; (keys m) → [:k1 :k2 ...] — vector of keyword singletons.
      ;; Keys are structural, so this always produces concrete values.
      (defop 'keys
        {:construct
         (fn [env [map-expr]]
           (let [[env map-node] (bind/resolve-collection env map-expr :map "keys")
                 kw-kids (tree/kw-children map-node)
                 key-exprs (mapv (fn [child] (::tree/name child)) kw-kids)]
             (bind/bind env (vec key-exprs))))})

      ;; ── vals: extract values from a map as a vector ─────────
      ;; (vals m) → [v1 v2 ...] — vector of values, linked to originals.
      ;; Uses (get m :k) for each key so links enable bidirectional propagation.
      (defop 'vals
        {:construct
         (fn [env [map-expr]]
           (let [[env map-node] (bind/resolve-collection env map-expr :map "vals")
                 kw-kids (tree/kw-children map-node)
                 val-exprs (mapv (fn [child]
                                   (list 'get map-expr (::tree/name child)))
                                 kw-kids)]
             (bind/bind env (vec val-exprs))))})

      ;; ── entries: extract [key value] pairs from a map ───────
      ;; (entries m) → [[:k1 v1] [:k2 v2] ...] — vector of 2-element vectors.
      ;; Keys are keyword literals, values use (get m :k) for link propagation.
      (defop 'entries
        {:construct
         (fn [env [map-expr]]
           (let [[env map-node] (bind/resolve-collection env map-expr :map "entries")
                 kw-kids (tree/kw-children map-node)
                 pair-exprs (mapv (fn [child]
                                    (let [k (::tree/name child)]
                                      [k (list 'get map-expr k)]))
                                  kw-kids)]
             (bind/bind env (vec pair-exprs))))})

      ;; ── map-vals: apply function to every value in a map ────
      ;; (map-vals m f) → new map where each value is (f old-val).
      ;; Like update but for all keys. Anonymous-node-with-links pattern.
      (defop 'map-vals
        {:construct
         (fn [env [map-expr f-expr]]
           (let [[env map-node] (bind/resolve-collection env map-expr :map "map-vals")
                 all-children (tree/kw-children map-node)
                 ;; Bind the function to a temp name
                 f-sym (gensym "mvfn__")
                 env (bind/bind env f-sym f-expr)]
             (let [anon (gensym "mapvals__")
                   my-pos (tree/position env)
                   env (tree/ensure-path env [anon])
                   anon-env (tree/cd env [anon])
                   anon-env (assoc anon-env :map true)
                   ;; For each child: bind (f (get m k))
                   anon-env (reduce (fn [e child]
                                      (let [k (::tree/name child)
                                            e (tree/ensure-path e [k])
                                            e (tree/cd e [k])
                                            e (bind/bind e (list f-sym (list 'get map-expr k)))]
                                        (tree/cd (tree/root e) (conj my-pos anon))))
                                    anon-env
                                    all-children)
                   root (tree/root anon-env)
                   anon-abs-path (conj my-pos anon)
                   result (tree/cd root my-pos)]
               (assoc result :link anon-abs-path))))})

      ;; ── has-key?: check if a key exists in a map ────────────
      ;; (has-key? m :k) → true or false (singleton boolean domain).
      ;; Keys are structural/static, so always decidable at bind time.
      (defop 'has-key?
        {:construct
         (fn [env [map-expr key-expr]]
           (let [_ (when-not (keyword? key-expr)
                     (throw (ex-info "has-key?: key must be a literal keyword"
                                     {:key key-expr})))
                 [env map-node] (bind/resolve-collection env map-expr :map "has-key?")
                 kw-kids (tree/kw-children map-node)
                 key-names (set (map ::tree/name kw-kids))
                 result (contains? key-names key-expr)]
             (assoc env :domain (dom/single result))))})

      ;; ── update: apply function to value at key ──────────────
      ;; (update m :k f) → new map where (:k m) is replaced by (f (:k m)).
      ;; All other keys linked to original. Key must be a literal keyword.
      (defop 'update
        {:construct
         (fn [env [map-expr key-expr f-expr]]
           (let [_ (when-not (keyword? key-expr)
                     (throw (ex-info "update: key must be a literal keyword"
                                     {:key key-expr})))
                 [env map-node] (bind/resolve-collection env map-expr :map "update")
                 all-children (tree/kw-children map-node)
                 ;; Verify the key exists
                 _ (when-not (some #(= key-expr (::tree/name %)) all-children)
                     (throw (ex-info (str "update: key not found: " key-expr)
                                     {:key key-expr :map map-expr})))
                 ;; Bind the function to a temp name
                 f-sym (gensym "updfn__")
                 env (bind/bind env f-sym f-expr)]
             (let [anon (gensym "update__")
                   my-pos (tree/position env)
                   env (tree/ensure-path env [anon])
                   anon-env (tree/cd env [anon])
                   anon-env (assoc anon-env :map true)
                   ;; For each child: if it's the updated key, bind (f (get m k));
                   ;; otherwise, link to original.
                   anon-env (reduce (fn [e child]
                                      (let [k (::tree/name child)]
                                        (if (= k key-expr)
                                          ;; Updated key: bind (f (get m k))
                                          (let [e (tree/ensure-path e [k])
                                                e (tree/cd e [k])
                                                e (bind/bind e (list f-sym (list 'get map-expr key-expr)))]
                                            (tree/cd (tree/root e) (conj my-pos anon)))
                                          ;; Other keys: link to original
                                          (let [target-path (tree/position (bind/resolve child))]
                                            (-> (tree/ensure-path e [k])
                                                (tree/put [k] :link target-path))))))
                                    anon-env
                                    all-children)
                   root (tree/root anon-env)
                   anon-abs-path (conj my-pos anon)
                   result (tree/cd root my-pos)]
               (assoc result :link anon-abs-path))))})))

;; ── Destructuring operators ───────────────────────────────────

(defn- register-destructuring
  "Register destructuring operators: ks, as."
  [env]
  (-> env

      ;; ════════════════════════════════════════════════════════
      ;; Destructuring operators (via `:destruct` protocol)
      ;; ════════════════════════════════════════════════════════
      ;;
      ;; These operators work in pattern position within `let` bindings.
      ;; When bind-pattern encounters `(head args...)`, it calls
      ;; (:destruct node) with (env args seed-sym seed-domain) → [env' bound-syms].

      ;; ── ks: keyword-symbol map destructuring ────────────────
      ;; (ks a b c) against value → bind a to (:a value), b to (:b value), etc.
      (defop 'ks
        {:destruct
         (fn [env args seed-sym seed-domain]
           ;; (ks a b c) → same as {:a a :b b :c c}
           ;; Handles nested forms like (ks (as m {:a x})) where arg is a list.
           (let [map-pattern (into {}
                                   (map (fn [s]
                                          (if (symbol? s)
                                            [(keyword s) s]
                                            ;; Nested: (ks (as m {:a x})) → key from main sym
                                            (let [main (if (seq? s)
                                                         (second s) ;; (as m ...) → m
                                                         s)]
                                              [(keyword main) s]))))
                                   args)]
             (bind/bind-pattern-map env map-pattern seed-sym seed-domain)))})

      ;; ── as: bind whole + inner destructure ──────────────────
      ;; (as m (ks x y)) → bind m to value, then destructure with inner pattern
      (defop 'as
        {:destruct
         (fn [env [name-sym inner-pattern] seed-sym seed-domain]
           ;; (as m (ks x y)) → bind m to seed with full domain, then recurse
           (when-not (symbol? name-sym)
             (throw (ex-info (str "as: first argument must be a symbol, got: " (pr-str name-sym)
                                  " (type: " (type name-sym) ")")
                             {:name name-sym :pattern inner-pattern})))
           (let [;; m gets the full seed domain
                 env' (bind/domain-node env name-sym seed-domain seed-sym)
                 ;; Recurse on inner pattern with same seed
                 [env'' inner-syms] (bind/bind-pattern env' inner-pattern seed-sym seed-domain)]
             [env'' (into [name-sym] inner-syms)]))})))

;; ── Predicate operators ───────────────────────────────────────

(defn- register-predicates
  "Register filter predicates: even, odd, pos, neg, zero."
  [env]
  ;; Each narrows a domain by filtering with a Clojure predicate.
  ;; No ? suffix — these are constraints, not boolean predicates.
  (defops env (for [[sym pred-fn label] [['even even? "even"]
                                         ['odd odd? "odd"]
                                         ['pos pos? "positive"]
                                         ['neg neg? "negative"]
                                         ['zero zero? "zero"]]]
                [sym {:construct (filter-pred-bind pred-fn label)}])))

;; ════════════════════════════════════════════════════════════════
;; Base environment assembly
;; ════════════════════════════════════════════════════════════════

(defn base-env
  "Build the base environment with all built-in forms."
  []
  (-> {}
      (register-core-forms)
      (register-relational)
      (register-arithmetic)
      (register-type-system)
      (register-definitions)
      (register-collections)
      (register-destructuring)
      (register-predicates)))
