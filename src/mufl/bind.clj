(ns mufl.bind
  "Core bind + constraint propagation for mufl.

   bind takes [env expr] and returns a new tree where:
   1. New nodes are created for the expression
   2. Constraint nodes are added
   3. All domains are propagated to fixpoint

   After every bind, the tree is maximally narrowed."
  (:refer-clojure :exclude [resolve])
  (:require [mufl.tree :as tree]
            [mufl.domain :as dom]))

;; ════════════════════════════════════════════════════════════════
;; Helpers
;; ════════════════════════════════════════════════════════════════

(defn resolve
  "Follow :link chains to the target node."
  [env]
  (if-let [link (:link env)]
    (resolve (tree/at env link))
    env))

(defn resolve-at
  "Navigate to path from root, then resolve links."
  [env path]
  (resolve (tree/at env path)))

(defn domain-of
  "Get the domain of a node (following links)."
  [env path]
  (or (:domain (resolve-at env path))
      dom/any))

(defn set-domain
  "Set the domain of a node at path (from root). Returns updated tree rooted env."
  [env path domain]
  (let [target (resolve-at env path)
        target-path (tree/position target)]
    (tree/put (tree/root env) target-path :domain domain)))

;; ════════════════════════════════════════════════════════════════
;; Constraint narrowing functions
;; ════════════════════════════════════════════════════════════════
;;
;; Each returns {:env env' :changed [paths...]} or nil (contradiction).
;; ════════════════════════════════════════════════════════════════

(defn- apply-narrowings
  "Apply a seq of [path new-domain] narrowings.
   Returns {:env env' :changed [changed-paths]} or nil on contradiction."
  [env narrowings]
  (loop [env env
         [[path new-dom] & rest] narrowings
         changed []]
    (if (nil? path)
      {:env env :changed changed}
      (let [old-dom (domain-of env path)]
        (let [narrowed (dom/intersect old-dom new-dom)]
          (cond
            (dom/void? narrowed)
            nil ;; contradiction

            (= narrowed old-dom)
            (recur env rest changed)

            :else
            (recur (set-domain env path narrowed)
                   rest
                   (conj changed path))))))))

(defn narrow-lt
  "Narrow for a < b constraint."
  [env [a-path b-path]]
  (let [a-dom (domain-of env a-path)
        b-dom (domain-of env b-path)]
    (if (and (dom/finite? a-dom) (dom/finite? b-dom))
      (let [b-max (dom/domain-max b-dom)
            a-min (dom/domain-min a-dom)
            a-new (dom/domain-below a-dom b-max)
            b-new (dom/domain-above b-dom a-min)]
        (apply-narrowings env [[a-path a-new] [b-path b-new]]))
      {:env env :changed []})))

(defn narrow-gt
  "Narrow for a > b constraint."
  [env [a-path b-path]]
  ;; a > b is the same as b < a
  (narrow-lt env [b-path a-path]))

(defn narrow-lte
  "Narrow for a <= b."
  [env [a-path b-path]]
  (let [a-dom (domain-of env a-path)
        b-dom (domain-of env b-path)]
    (if (and (dom/finite? a-dom) (dom/finite? b-dom))
      (let [b-max (dom/domain-max b-dom)
            a-min (dom/domain-min a-dom)
            a-new (dom/domain-at-most a-dom b-max)
            b-new (dom/domain-at-least b-dom a-min)]
        (apply-narrowings env [[a-path a-new] [b-path b-new]]))
      {:env env :changed []})))

(defn narrow-gte
  "Narrow for a >= b."
  [env [a-path b-path]]
  (narrow-lte env [b-path a-path]))

(defn narrow-eq
  "Narrow for a = b (unification). Both domains must intersect."
  [env [a-path b-path]]
  (let [a-dom (domain-of env a-path)
        b-dom (domain-of env b-path)
        shared (dom/intersect a-dom b-dom)]
    (if (dom/void? shared)
      nil
      (apply-narrowings env [[a-path shared] [b-path shared]]))))

(defn narrow-neq
  "Narrow for a != b. Only narrows when one side is singleton."
  [env [a-path b-path]]
  (let [a-dom (domain-of env a-path)
        b-dom (domain-of env b-path)]
    (cond
      ;; a is singleton — remove it from b
      (dom/singleton? a-dom)
      (apply-narrowings env [[b-path (dom/subtract b-dom a-dom)]])

      ;; b is singleton — remove it from a
      (dom/singleton? b-dom)
      (apply-narrowings env [[a-path (dom/subtract a-dom b-dom)]])

      :else
      {:env env :changed []})))

(defn narrow-plus
  "Narrow for a + b = c constraint. refs = [a-path b-path c-path]"
  [env [a-path b-path c-path]]
  (let [a-dom (domain-of env a-path)
        b-dom (domain-of env b-path)
        c-dom (domain-of env c-path)]
    (if (and (dom/finite? a-dom) (dom/finite? b-dom) (dom/finite? c-dom))
      (let [;; c must be in {a+b for a in A, b in B}
            possible-c (dom/domain-add a-dom b-dom)
            c-new (dom/intersect c-dom possible-c)
            ;; a must be in {c-b for c in C', b in B}
            possible-a (dom/domain-sub c-new b-dom)
            a-new (dom/intersect a-dom possible-a)
            ;; b must be in {c-a for c in C', a in A'}
            possible-b (dom/domain-sub c-new a-new)
            b-new (dom/intersect b-dom possible-b)]
        (apply-narrowings env [[a-path a-new] [b-path b-new] [c-path c-new]]))
      {:env env :changed []})))

(defn narrow-times
  "Narrow for a * b = c constraint."
  [env [a-path b-path c-path]]
  (let [a-dom (domain-of env a-path)
        b-dom (domain-of env b-path)
        c-dom (domain-of env c-path)]
    (if (and (dom/finite? a-dom) (dom/finite? b-dom) (dom/finite? c-dom))
      (let [possible-c (dom/domain-mul a-dom b-dom)
            c-new (dom/intersect c-dom possible-c)
            ;; filter a to values where there exists a b giving a valid c
            c-vals (dom/members c-new)
            b-vals (dom/members b-dom)
            a-new (dom/domain-filter a-dom
                                     (fn [a] (some #(c-vals (* a %)) b-vals)))
            a-vals (dom/members a-new)
            b-new (dom/domain-filter b-dom
                                     (fn [b] (some #(c-vals (* % b)) a-vals)))]
        (apply-narrowings env [[a-path a-new] [b-path b-new] [c-path c-new]]))
      {:env env :changed []})))

(defn narrow-minus
  "Narrow for a - b = c constraint."
  [env [a-path b-path c-path]]
  ;; a - b = c  ⟺  a = c + b  ⟺  c = a - b  ⟺  b = a - c
  (let [a-dom (domain-of env a-path)
        b-dom (domain-of env b-path)
        c-dom (domain-of env c-path)]
    (if (and (dom/finite? a-dom) (dom/finite? b-dom) (dom/finite? c-dom))
      (let [possible-c (dom/domain-sub a-dom b-dom)
            c-new (dom/intersect c-dom possible-c)
            possible-a (dom/domain-add c-new b-dom)
            a-new (dom/intersect a-dom possible-a)
            possible-b (dom/domain-sub a-new c-new)
            b-new (dom/intersect b-dom possible-b)]
        (apply-narrowings env [[a-path a-new] [b-path b-new] [c-path c-new]]))
      {:env env :changed []})))

(defn narrow-mod
  "Narrow for (mod a b) = c constraint."
  [env [a-path b-path c-path]]
  (let [a-dom (domain-of env a-path)
        b-dom (domain-of env b-path)
        c-dom (domain-of env c-path)]
    (if (and (dom/finite? a-dom) (dom/finite? b-dom) (dom/finite? c-dom))
      (let [c-vals (dom/members c-dom)
            b-vals (disj (dom/members b-dom) 0)
            ;; c must be achievable
            possible-c (dom/domain-mod a-dom (dom/finite b-vals))
            c-new (dom/intersect c-dom possible-c)
            c-vals-new (dom/members c-new)
            ;; a: keep values where (mod a b) ∈ c-new for some b
            a-new (dom/domain-filter a-dom
                                     (fn [a] (some #(c-vals-new (mod a %)) b-vals)))
            ;; b: keep values where (mod a b) ∈ c-new for some a
            b-new (dom/domain-filter (dom/finite b-vals)
                                     (fn [b] (some #(c-vals-new (mod % b)) (dom/members a-new))))]
        (apply-narrowings env [[a-path a-new] [b-path b-new] [c-path c-new]]))
      {:env env :changed []})))

(defn narrow-quot
  "Narrow for (quot a b) = c constraint."
  [env [a-path b-path c-path]]
  (let [a-dom (domain-of env a-path)
        b-dom (domain-of env b-path)
        c-dom (domain-of env c-path)]
    (if (and (dom/finite? a-dom) (dom/finite? b-dom) (dom/finite? c-dom))
      (let [c-vals (dom/members c-dom)
            b-vals (disj (dom/members b-dom) 0)
            possible-c (dom/domain-div a-dom (dom/finite b-vals))
            c-new (dom/intersect c-dom possible-c)
            c-vals-new (dom/members c-new)
            a-new (dom/domain-filter a-dom
                                     (fn [a] (some #(c-vals-new (quot a %)) b-vals)))
            b-new (dom/domain-filter (dom/finite b-vals)
                                     (fn [b] (some #(c-vals-new (quot % b)) (dom/members a-new))))]
        (apply-narrowings env [[a-path a-new] [b-path b-new] [c-path c-new]]))
      {:env env :changed []})))

(defn- alldiff-pass
  "One pass of alldiff narrowing: for each singleton, remove its value from others.
   Returns {:env :changed :any-change} or nil on contradiction."
  [env refs]
  (let [singletons (into {}
                         (keep (fn [p]
                                 (let [d (domain-of env p)]
                                   (when (dom/singleton? d)
                                     [p (dom/singleton-val d)]))))
                         refs)
        others (remove (set (keys singletons)) refs)]
    (reduce
     (fn [acc [_path val]]
       (reduce
        (fn [{:keys [env changed any-change]} other-path]
          (let [other-dom (domain-of env other-path)
                narrowed (dom/subtract other-dom (dom/single val))]
            (cond
              (dom/void? narrowed) (reduced nil)
              (= narrowed other-dom) {:env env :changed changed :any-change any-change}
              :else {:env (set-domain env other-path narrowed)
                     :changed (conj changed other-path)
                     :any-change true})))
        acc
        others))
     {:env env :changed [] :any-change false}
     singletons)))

(defn narrow-alldiff
  "Narrow for all-different constraint. Repeats until no progress."
  [env refs]
  (loop [env env
         all-changed []]
    (let [result (alldiff-pass env refs)]
      (cond
        (nil? result) nil
        (not (:any-change result)) {:env (:env result) :changed all-changed}
        :else (recur (:env result) (into all-changed (:changed result)))))))

(defn narrow-abs
  "Narrow for |a| = c constraint. refs = [a-path c-path]"
  [env [a-path c-path]]
  (let [a-dom (domain-of env a-path)
        c-dom (domain-of env c-path)]
    (if (and (dom/finite? a-dom) (dom/finite? c-dom))
      (let [;; c must be achievable from a
            possible-c (dom/finite (set (map #(Math/abs (long %)) (dom/members a-dom))))
            c-new (dom/intersect c-dom possible-c)
            ;; a must produce a value in c-new
            c-vals (dom/members c-new)
            a-new (dom/domain-filter a-dom (fn [a] (c-vals (Math/abs (long a)))))]
        (apply-narrowings env [[a-path a-new] [c-path c-new]]))
      {:env env :changed []})))

(def narrowing-fns
  {:< narrow-lt
   :> narrow-gt
   :<= narrow-lte
   :>= narrow-gte
   := narrow-eq
   :!= narrow-neq
   :+ narrow-plus
   :- narrow-minus
   :* narrow-times
   :mod narrow-mod
   :quot narrow-quot
   :abs narrow-abs
   :alldiff narrow-alldiff})

;; ════════════════════════════════════════════════════════════════
;; Propagation engine
;; ════════════════════════════════════════════════════════════════

(defn- collect-constraints
  "Find all constraint nodes in the current scope."
  [env]
  (let [root (tree/root env)]
    (->> (tree/children root)
         (keep (fn [child]
                 (when (:constraint child)
                   (tree/position child))))
         vec)))

(defn propagate
  "Run all constraints in pending set to fixpoint.
   Returns updated env (rooted), or nil on contradiction."
  [env pending]
  (if (empty? pending)
    env
    (let [[c-path & rest-pending] pending
          c-node (tree/at (tree/root env) c-path)
          narrow-fn (get narrowing-fns (:constraint c-node))]
      (if-not narrow-fn
        (recur env (vec rest-pending))
        (let [result (narrow-fn (tree/root env) (:refs c-node))]
          (cond
            ;; Contradiction
            (nil? result)
            nil

            ;; No changes — move on
            (empty? (:changed result))
            (recur (:env result) (vec rest-pending))

            ;; Progress — enqueue constraints watching changed nodes
            :else
            (let [env' (:env result)
                  new-pending (->> (:changed result)
                                   (mapcat (fn [changed-path]
                                             (let [node (tree/at env' changed-path)]
                                               (:watched-by node))))
                                   (remove #{c-path})
                                   (remove (set rest-pending))
                                   (into (vec rest-pending)))]
              (recur env' new-pending))))))))

;; ════════════════════════════════════════════════════════════════
;; Constraint registration
;; ════════════════════════════════════════════════════════════════

(defn- gensym-constraint-name []
  (keyword (gensym "c")))

(defn add-constraint
  "Add a constraint node to the tree and register watches.
   Returns the updated tree (rooted) with the constraint propagated to fixpoint,
   or nil on contradiction."
  [env constraint-op refs]
  (let [env (tree/root env)
        c-name (gensym-constraint-name)
        c-path [c-name]
        ;; Create constraint node
        env (-> env
                (tree/ensure-path c-path)
                (tree/put c-path {:constraint constraint-op
                                  :refs refs}))
        ;; Register watches on referenced nodes
        env (reduce (fn [e ref-path]
                      ;; Follow links to get actual target
                      (let [target (resolve-at e ref-path)
                            target-path (tree/position target)]
                        (tree/put e target-path :watched-by
                                  (conj (or (:watched-by (tree/at e target-path)) #{})
                                        c-path))))
                    env refs)]
    ;; Propagate
    (propagate env [c-path])))

;; ════════════════════════════════════════════════════════════════
;; The bind function
;; ════════════════════════════════════════════════════════════════

(defn- pairs&return
  "Split a sequence into pairs and optional trailing return expression."
  [xs]
  (if (odd? (count xs))
    [(partition 2 (butlast xs)) (last xs)]
    [(partition 2 xs) nil]))

(defn resolve-to-path
  "Given an expression that refers to a name, resolve it to its path in the tree."
  [env expr]
  (cond
    (symbol? expr)
    (when-let [found (tree/find env [expr])]
      (tree/position (resolve found)))

    :else nil))

(declare bind)

(defn ensure-node
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
      (let [target (resolve found)]
        [env (tree/position target)])
      (throw (ex-info (str "Unresolved symbol: " expr) {:sym expr})))

    ;; Literal → create anonymous child with singleton domain
    (or (number? expr) (string? expr) (boolean? expr) (nil? expr) (keyword? expr))
    (let [anon (gensym "lit")
          env' (bind env anon expr)]
      [env' [anon]])

    ;; Expression → create anonymous child, bind the expr there
    (seq? expr)
    (let [anon (gensym "expr")
          env' (bind env anon expr)]
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
   Returns the env at `return-to` position, or throws on contradiction."
  [env op paths return-to]
  (let [env-root (tree/root env)
        result (add-constraint env-root op paths)]
    (if result
      (or (tree/cd result return-to) result)
      (throw (ex-info "Contradiction detected" {:op op :paths paths})))))

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
        a-dom (domain-of (tree/root env'') a-path)
        b-dom (domain-of (tree/root env'') b-path)
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
       (let [found-target (resolve found)]
         (assoc env
                :link (tree/position found-target)
                :domain (:domain found-target)))
       (throw (ex-info (str "Unresolved symbol: " expr) {:sym expr})))

     ;; List → dispatch on head
     (seq? expr)
     (let [[head & args] expr]
       (cond
         (symbol? head)
         (let [raw-node (tree/find env [head])
               node (if (:link raw-node) (resolve raw-node) raw-node)]
           (cond
             ;; Built-in form with :bind
             (:bind node)
             ((:bind node) env args)

             ;; User-defined function with :mufl-fn
             (:mufl-fn node)
             (let [{:keys [params body]} (:mufl-fn node)
                   my-pos (tree/position env)
                   ;; Create a call scope as child of current position
                   call-name (gensym "call")
                   env-with-scope (tree/ensure-path env [call-name])
                   call-env (tree/cd env-with-scope [call-name])
                   ;; Bind each param to corresponding arg expression
                   env-bound (reduce (fn [e [param arg-expr]]
                                       (bind e param arg-expr))
                                     call-env
                                     (map vector params args))
                   ;; Evaluate body in the call scope — this creates
                   ;; the result node with domain/link in the call scope
                   result (bind env-bound body)
                   result-pos (tree/position result)]
               ;; The caller links to the result node in the call scope.
               ;; This way constraints on the caller propagate through.
               (let [result-root (tree/root result)
                     ;; Navigate back to caller position
                     caller (tree/cd result-root my-pos)
                     ;; If result has a link, follow it for the final target
                     target-path (or (:link result) result-pos)]
                 (assoc caller
                        :link target-path
                        :domain (:domain (resolve-at result-root target-path)))))

             :else
             (throw (ex-info (str "No :bind for form: " head) {:head head}))))

         :else
         (throw (ex-info "Cannot bind list with non-symbol head" {:expr expr}))))

     ;; Vector → bind each element as an indexed child
     (vector? expr)
     (let [env (reduce (fn [e [i sub-expr]]
                         (bind e i sub-expr))
                       (assoc env :vector true)
                       (map-indexed vector expr))]
       env)

     ;; Map → bind each key-value pair (TODO: full support)
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
