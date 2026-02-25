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
      (defprim 'let
        {:bind
         (fn [env [bindings-vec & body]]
           (let [pairs (partition 2 bindings-vec)
                 env' (reduce (fn [e [sym expr]]
                                (bind/bind e sym expr))
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

      (defprim '<
        {:bind (fn [env args] (bind/bind-relational env :< args))})

      (defprim '>
        {:bind (fn [env args] (bind/bind-relational env :> args))})

      (defprim '<=
        {:bind (fn [env args] (bind/bind-relational env :<= args))})

      (defprim '>=
        {:bind (fn [env args] (bind/bind-relational env :>= args))})

      (defprim '=
        {:bind (fn [env args] (bind/bind-relational env := args))})

      (defprim '!=
        {:bind (fn [env args] (bind/bind-relational env :!= args))})

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
           (let [paths (mapv (fn [sym]
                               (let [p (bind/resolve-to-path env sym)]
                                 (when-not p
                                   (throw (ex-info (str "Cannot resolve in distinct: " sym)
                                                   {:sym sym})))
                                 p))
                             vec-expr)
                 env-root (bind/add-constraint (tree/root env) :alldiff paths)]
             (or (and env-root (tree/cd env-root (tree/position env)))
                 (throw (ex-info "Contradiction in distinct" {:args vec-expr})))))})

      ;; ── mod / quot ──────────────────────────────────────────

      (defprim 'mod
        {:bind (fn [env args]
                 (bind/bind-arithmetic env :mod args))})

      (defprim 'quot
        {:bind (fn [env args]
                 (bind/bind-arithmetic env :quot args))})

      ;; ── range: integer range domain ─────────────────────────
      (defprim 'range
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
      ;; Tries each branch on a snapshot. Collects the set of variable
      ;; domains from each successful branch, then constrains each variable
      ;; to the union of its per-branch domains. This is sound — it only
      ;; removes values that are impossible in ALL branches.
      (defprim 'or
        {:bind
         (fn [env args]
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
      ;; Stores branches for solve to enumerate.
      ;; At bind time: no constraint narrowing (that would lose information).
      ;; At solve time: each branch is tried with its test constraint.
      (defprim 'if
        {:bind
         (fn [env [test then else]]
           ;; If test is a boolean literal, resolve at bind time
           (cond
             (true? test)  (bind/bind env then)
             (false? test) (bind/bind env (if (some? else) else nil))
             :else (assoc env :fork {:kind :if
                                     :test test
                                     :then then
                                     :else else})))})

      ;; ── when: like if but no else branch ────────────────────
      (defprim 'when
        {:bind
         (fn [env [test & body]]
           (bind/bind env (list 'if test (cons 'do body) nil)))})

      ;; ── cond: chained conditionals ──────────────────────────
      (defprim 'cond
        {:bind
         (fn [env args]
           (assoc env :fork {:kind :cond
                             :branches (vec (partition 2 args))}))})

      ;; ── even? / odd? — domain predicates as constraints ────
      (defprim 'even?
        {:bind
         (fn [env [arg]]
           (let [[env' path] (bind/ensure-node-abs env arg)
                 d (bind/domain-of (tree/root env') path)
                 narrowed (dom/domain-filter d even?)]
             (if (dom/void? narrowed)
               (throw (ex-info "Contradiction: no even values" {:arg arg}))
               (let [env' (bind/set-domain (tree/root env') path narrowed)]
                 (or (tree/cd env' (tree/position env)) env')))))})

      (defprim 'odd?
        {:bind
         (fn [env [arg]]
           (let [[env' path] (bind/ensure-node-abs env arg)
                 d (bind/domain-of (tree/root env') path)
                 narrowed (dom/domain-filter d odd?)]
             (if (dom/void? narrowed)
               (throw (ex-info "Contradiction: no odd values" {:arg arg}))
               (let [env' (bind/set-domain (tree/root env') path narrowed)]
                 (or (tree/cd env' (tree/position env)) env')))))})

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
               (assoc env :domain dom/any))))})))
