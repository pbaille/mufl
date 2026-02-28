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
;; Recursion depth tracking
;; ════════════════════════════════════════════════════════════════

(def ^:dynamic *call-depth*
  "Current function call depth. Incremented on each :mufl-fn application."
  0)

(def max-call-depth
  "Maximum allowed call depth before throwing an error."
  100)

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
    (if (or (and (dom/finite? a-dom) (dom/finite? b-dom))
            (dom/range? a-dom) (dom/range? b-dom))
      (let [b-max (dom/domain-max b-dom)
            a-min (dom/domain-min a-dom)
            a-new (if b-max (dom/domain-below a-dom b-max) a-dom)
            b-new (if a-min (dom/domain-above b-dom a-min) b-dom)]
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
    (if (or (and (dom/finite? a-dom) (dom/finite? b-dom))
            (dom/range? a-dom) (dom/range? b-dom))
      (let [b-max (dom/domain-max b-dom)
            a-min (dom/domain-min a-dom)
            a-new (if b-max (dom/domain-at-most a-dom b-max) a-dom)
            b-new (if a-min (dom/domain-at-least b-dom a-min) b-dom)]
        (apply-narrowings env [[a-path a-new] [b-path b-new]]))
      {:env env :changed []})))

(defn narrow-gte
  "Narrow for a >= b."
  [env [a-path b-path]]
  (narrow-lte env [b-path a-path]))

(def ^:private composite-kinds
  "Set of composite domain kinds."
  #{:vector-of :tuple :map-of})

(declare apply-composite-constraint)

(defn- narrow-children
  "Narrow a sequence of [child elem-dom] pairs against their respective domains.
   Returns {:env env' :changed [paths...]} or nil on contradiction.
   Each child is a tree node; elem-dom is the target domain to intersect with."
  [env child-dom-pairs]
  (reduce (fn [{:keys [env changed]} [child elem-dom]]
            (let [resolved-child (resolve child)
                  child-path (tree/position resolved-child)
                  child-dom (or (:domain resolved-child) dom/any)
                  narrowed (dom/intersect child-dom elem-dom)]
              (cond
                (dom/void? narrowed)
                (reduced nil)

                (= narrowed child-dom)
                ;; No change, but check if child is a collection node
                ;; and the element domain is composite (nested case)
                (if (and (composite-kinds (:kind elem-dom))
                         (or (:vector resolved-child) (:map resolved-child)))
                  (let [inner-result (apply-composite-constraint env child-path elem-dom)]
                    (if (nil? inner-result)
                      (reduced nil)
                      {:env (:env inner-result)
                       :changed (into changed (:changed inner-result))}))
                  {:env env :changed changed})

                :else
                (let [env' (set-domain env child-path narrowed)
                      ;; Recurse for nested composites
                      env' (if (and (composite-kinds (:kind elem-dom))
                                    (or (:vector resolved-child) (:map resolved-child)))
                             (let [inner-result (apply-composite-constraint env' child-path elem-dom)]
                               (if (nil? inner-result)
                                 (reduced nil)
                                 (:env inner-result)))
                             env')]
                  (if (reduced? env')
                    env' ;; propagate reduced nil
                    {:env env' :changed (conj changed child-path)})))))
          {:env env :changed []}
          child-dom-pairs))

(defn apply-composite-constraint
  "Apply a composite domain's structural constraints to a collection node.
   Returns {:env env' :changed [paths...]} or nil on contradiction.
   
   For :vector-of — intersects each child's domain with the element domain.
   For :tuple — checks length, intersects each position's domain.
   For :map-of — validates keys, intersects each value's domain."
  [env target-path composite-dom]
  (let [target-node (resolve-at env target-path)]
    (case (:kind composite-dom)

      :vector-of
      (if-not (:vector target-node)
        nil ;; contradiction: composite domain on non-vector
        (let [children (sort-by ::tree/name
                                (filter #(integer? (::tree/name %))
                                        (tree/children target-node)))
              elem-dom (:element composite-dom)]
          (narrow-children env (map (fn [c] [c elem-dom]) children))))

      :tuple
      (if-not (:vector target-node)
        nil ;; contradiction: tuple on non-vector
        (let [children (sort-by ::tree/name
                                (filter #(integer? (::tree/name %))
                                        (tree/children target-node)))
              elem-doms (:elements composite-dom)
              n (count children)
              expected (count elem-doms)]
          (if (not= n expected)
            nil ;; contradiction: length mismatch
            (narrow-children env (map vector children elem-doms)))))

      :map-of
      (if-not (:map target-node)
        nil ;; contradiction: map-of on non-map
        (let [children (tree/children target-node)
              key-dom (:key composite-dom)
              val-dom (:value composite-dom)]
          ;; Validate keys
          (if (some (fn [child]
                      (not (dom/contains-val? key-dom (::tree/name child))))
                    children)
            nil ;; contradiction: key doesn't match
            (narrow-children env (map (fn [c] [c val-dom]) children)))))

      ;; Unknown composite kind — no-op
      {:env env :changed []})))

(defn narrow-eq
  "Narrow for a = b (unification). Both domains must intersect.
   When one side has a composite domain (vector-of, tuple, map-of) and
   the other side is a collection node, applies the composite domain's
   structural constraints to the collection's children."
  [env [a-path b-path]]
  (let [a-dom (domain-of env a-path)
        b-dom (domain-of env b-path)
        a-node (resolve-at env a-path)
        b-node (resolve-at env b-path)
        a-composite? (composite-kinds (:kind a-dom))
        b-composite? (composite-kinds (:kind b-dom))
        a-collection? (or (:vector a-node) (:map a-node))
        b-collection? (or (:vector b-node) (:map b-node))]
    (cond
      ;; Composite on a-side, collection on b-side → apply a's domain to b's structure
      (and a-composite? b-collection?)
      (apply-composite-constraint env (tree/position b-node) a-dom)

      ;; Composite on b-side, collection on a-side → apply b's domain to a's structure
      (and b-composite? a-collection?)
      (apply-composite-constraint env (tree/position a-node) b-dom)

      ;; Both composite (no collection nodes) → standard domain intersection
      ;; Default: existing scalar path
      :else
      (let [shared (dom/intersect a-dom b-dom)]
        (if (dom/void? shared)
          nil
          (apply-narrowings env [[a-path shared] [b-path shared]]))))))

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
        c-dom (domain-of env c-path)
        has-range? (or (dom/range? a-dom) (dom/range? b-dom) (dom/range? c-dom))]
    (if (or (and (dom/finite? a-dom) (dom/finite? b-dom) (dom/finite? c-dom))
            has-range?)
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
    (cond
      ;; All materializable — use members for precise narrowing
      (and (dom/materializable? a-dom) (dom/materializable? b-dom) (dom/materializable? c-dom))
      (let [possible-c (dom/domain-mul a-dom b-dom)
            c-new (dom/intersect c-dom possible-c)
            c-vals (dom/members c-new)
            b-vals (dom/members b-dom)
            a-new (dom/domain-filter a-dom
                                     (fn [a] (some #(c-vals (* a %)) b-vals)))
            a-vals (dom/members a-new)
            b-new (dom/domain-filter b-dom
                                     (fn [b] (some #(c-vals (* % b)) a-vals)))]
        (apply-narrowings env [[a-path a-new] [b-path b-new] [c-path c-new]]))

      ;; Range involved but not materializable — use bounds-based narrowing
      (or (dom/range? a-dom) (dom/range? b-dom) (dom/range? c-dom))
      (let [possible-c (dom/domain-mul a-dom b-dom)
            c-new (dom/intersect c-dom possible-c)]
        (apply-narrowings env [[c-path c-new]]))

      :else
      {:env env :changed []})))

(defn narrow-minus
  "Narrow for a - b = c constraint."
  [env [a-path b-path c-path]]
  ;; a - b = c  ⟺  a = c + b  ⟺  c = a - b  ⟺  b = a - c
  (let [a-dom (domain-of env a-path)
        b-dom (domain-of env b-path)
        c-dom (domain-of env c-path)
        has-range? (or (dom/range? a-dom) (dom/range? b-dom) (dom/range? c-dom))]
    (if (or (and (dom/finite? a-dom) (dom/finite? b-dom) (dom/finite? c-dom))
            has-range?)
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
    (if (and (dom/materializable? a-dom) (dom/materializable? b-dom) (dom/materializable? c-dom))
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
    (if (and (dom/materializable? a-dom) (dom/materializable? b-dom) (dom/materializable? c-dom))
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
    (if (and (dom/materializable? a-dom) (dom/materializable? c-dom))
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

(declare bind)
(declare apply-domain-constraint)
(declare bind-pattern)

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

;; ════════════════════════════════════════════════════════════════
;; Domain schema resolution (narrow support)
;; ════════════════════════════════════════════════════════════════

(defn resolve-schema-from-tree
  "Interpret a bound tree node as a domain schema.
   Walks the tree structure to reconstruct a schema map.
   Handles: type-domains, scalar domains, composite domains,
   map nodes, vector nodes, links, and link+map (and-composition)."
  [node]
  (cond
    ;; Type domain primitive (string, integer, etc.)
    (:type-domain node)
    {:kind :type-constraint :domain (:type-domain node)}

    ;; Link AND map → and-composition (e.g., from (and Person {:company string}))
    (and (:link node) (:map node))
    (let [linked-node (resolve (tree/at node (:link node)))
          linked-schema (resolve-schema-from-tree linked-node)
          local-children (clojure.core/filter #(keyword? (::tree/name %)) (tree/children node))
          local-fields (into {} (map (fn [child]
                                       [(::tree/name child)
                                        (resolve-schema-from-tree (resolve child))])
                                     local-children))]
      {:kind :and-schema
       :schemas [linked-schema {:kind :map-schema :fields local-fields}]})

    ;; Link only → follow it
    (:link node)
    (resolve-schema-from-tree (resolve (tree/at node (:link node))))

    ;; Domain value (between, one-of, composite vector-of/tuple/map-of, etc.)
    (:domain node)
    {:kind :type-constraint :domain (:domain node)}

    ;; Map node → structural map schema
    (:map node)
    (let [children (clojure.core/filter #(keyword? (::tree/name %)) (tree/children node))
          fields (into {} (map (fn [child]
                                 [(::tree/name child)
                                  (resolve-schema-from-tree (resolve child))])
                               children))]
      {:kind :map-schema :fields fields})

    ;; Vector node → tuple schema
    (:vector node)
    (let [children (sort-by ::tree/name
                            (clojure.core/filter #(integer? (::tree/name %)) (tree/children node)))
          elem-schemas (mapv #(resolve-schema-from-tree (resolve %)) children)]
      (if (every? #(= :type-constraint (:kind %)) elem-schemas)
        {:kind :type-constraint
         :domain (dom/tuple-dom (mapv :domain elem-schemas))}
        {:kind :tuple-schema
         :element-schemas elem-schemas}))

    :else
    (throw (ex-info "Cannot interpret node as domain schema"
                    {:node (select-keys node [:domain :link :map :vector ::tree/name])}))))

(defn resolve-domain-schema
  "Resolve a domain schema expression into a schema map.
   
   Schema forms:
   - `string`, `integer`, etc. → type domain reference
   - `(range lo hi)` → int-range domain
   - `(one-of a b c)` → finite domain
   - `{:key1 type1 :key2 type2}` → map schema (structural domain)
   - `[type1 type2 ...]` → tuple schema (sugar for (tuple [...]))
   - `(and DomainName {:extra-key type})` → composed domain (intersection)
   - `DomainName` → reference to another defined domain"
  [env schema-expr]
  (cond
    ;; Symbol → resolve to type domain or bound value
    (symbol? schema-expr)
    (if-let [found (tree/find env [schema-expr])]
      (resolve-schema-from-tree found)
      (throw (ex-info (str "Unresolved domain type: " schema-expr)
                      {:sym schema-expr})))

    ;; Vector literal → tuple schema (sugar for (tuple [t1 t2 ...]))
    (vector? schema-expr)
    (let [elem-schemas (mapv #(resolve-domain-schema env %) schema-expr)]
      (if (every? #(= :type-constraint (:kind %)) elem-schemas)
        {:kind :type-constraint
         :domain (dom/tuple-dom (mapv :domain elem-schemas))}
        {:kind :tuple-schema
         :element-schemas elem-schemas}))

    ;; Map literal → structural (map) domain
    (map? schema-expr)
    {:kind :map-schema
     :fields (into {} (map (fn [[k v]]
                             [k (resolve-domain-schema env v)])
                           schema-expr))}

    ;; List → dispatch on head
    (seq? schema-expr)
    (let [[head & args] schema-expr]
      (case head
        between (let [[lo hi] args]
                  {:kind :type-constraint :domain (dom/int-range lo hi)})
        one-of {:kind :type-constraint :domain (dom/finite (set args))}
        and {:kind :and-schema
             :schemas (mapv #(resolve-domain-schema env %) args)}

        ;; Type constructor schemas for collections
        ;; When all element schemas are :type-constraint, produce composite domains directly.
        ;; Otherwise, fall back to schema kinds for apply-domain-constraint to handle.
        vector-of (let [[elem-type] args
                         elem-schema (resolve-domain-schema env elem-type)]
                    (if (= :type-constraint (:kind elem-schema))
                      {:kind :type-constraint
                       :domain (dom/vector-of-dom (:domain elem-schema))}
                      {:kind :vector-of-schema
                       :element-schema elem-schema}))

        tuple (let [[types-vec] args]
                (when-not (vector? types-vec)
                  (throw (ex-info "tuple schema requires a vector of types"
                                  {:form schema-expr})))
                (let [elem-schemas (mapv #(resolve-domain-schema env %) types-vec)]
                  (if (every? #(= :type-constraint (:kind %)) elem-schemas)
                    {:kind :type-constraint
                     :domain (dom/tuple-dom (mapv :domain elem-schemas))}
                    {:kind :tuple-schema
                     :element-schemas elem-schemas})))

        map-of (let [[key-type val-type] args
                      key-schema (resolve-domain-schema env key-type)
                      val-schema (resolve-domain-schema env val-type)]
                 (if (and (= :type-constraint (:kind key-schema))
                          (= :type-constraint (:kind val-schema)))
                   {:kind :type-constraint
                    :domain (dom/map-of-dom (:domain key-schema) (:domain val-schema))}
                   {:kind :map-of-schema
                    :key-schema key-schema
                    :value-schema val-schema}))

        (throw (ex-info (str "Unknown domain schema form: " head)
                        {:form schema-expr}))))

    :else
    (throw (ex-info "Invalid domain schema expression" {:expr schema-expr}))))

(defn apply-domain-constraint
  "Apply a domain schema as a constraint to an argument.
   (narrow target Schema) constrains target to match the schema.
   Returns the env at the same position it was called from."
  [env schema args]
  (let [my-pos (tree/position env)
        target-expr (first args)]
    (when (not= 1 (count args))
      (throw (ex-info "Domain constraint takes exactly one argument"
                      {:args args})))
    (letfn [(apply-schema [env target-path schema]
              (case (:kind schema)
                ;; Simple type/value constraint → intersect domain
                ;; For composite domains, delegate to apply-composite-constraint
                :type-constraint
                (let [constraint-dom (:domain schema)]
                  (if (composite-kinds (:kind constraint-dom))
                    ;; Composite domain → apply structural constraints
                    (let [env-root (tree/root env)
                          result (apply-composite-constraint env-root target-path constraint-dom)]
                      (if-not result
                        (throw (ex-info "Domain constraint contradiction"
                                        {:path target-path :domain constraint-dom}))
                        (let [env' (:env result)
                              changed (:changed result)]
                          ;; Propagate watchers for all changed paths
                          (reduce (fn [e path]
                                    (let [node (tree/cd e path)
                                          watchers (:watched-by node)]
                                      (if (seq watchers)
                                        (or (propagate e (vec watchers))
                                            (throw (ex-info "Contradiction during domain propagation"
                                                            {:path path})))
                                        e)))
                                  env'
                                  changed))))
                    ;; Scalar domain → existing intersect logic
                    (let [env-root (tree/root env)
                          current-dom (domain-of env-root target-path)
                          narrowed (dom/intersect current-dom constraint-dom)]
                      (if (dom/void? narrowed)
                        (throw (ex-info "Domain constraint contradiction"
                                        {:path target-path :domain constraint-dom
                                         :current current-dom}))
                        (let [env' (set-domain env-root target-path narrowed)]
                          ;; Propagate any watchers
                          (let [node (tree/cd env' target-path)
                                watchers (:watched-by node)]
                            (if (seq watchers)
                              (or (propagate env' (vec watchers))
                                  (throw (ex-info "Contradiction during domain propagation"
                                                  {:path target-path})))
                              env')))))))

                ;; Map schema → ensure target is a map, constrain each field
                :map-schema
                (let [env-root (tree/root env)
                      target-node (resolve-at env-root target-path)]
                  (when-not (:map target-node)
                    (throw (ex-info "Domain constraint: expected a map"
                                    {:path target-path})))
                  (let [target-resolved-path (tree/position target-node)]
                    (reduce (fn [env-root [field-key field-schema]]
                              (let [child-path (conj target-resolved-path field-key)
                                    child (tree/cd env-root child-path)]
                                (if child
                                  ;; Field exists → constrain it
                                  (let [resolved-child (resolve (tree/cd env-root child-path))
                                        resolved-path (tree/position resolved-child)]
                                    (apply-schema (tree/cd env-root my-pos) resolved-path field-schema))
                                  ;; Field doesn't exist → create it and constrain
                                  ;; (For now, require field to exist)
                                  (throw (ex-info (str "Domain constraint: missing field " field-key)
                                                  {:field field-key :path target-path})))))
                            env-root
                            (:fields schema))))

                ;; And-schema → apply each sub-schema in sequence
                :and-schema
                (reduce (fn [env-root sub-schema]
                          (apply-schema (tree/cd env-root my-pos) target-path sub-schema))
                        (tree/root env)
                        (:schemas schema))

                ;; Vector-of schema → ensure target is a vector, constrain each element
                :vector-of-schema
                (let [env-root (tree/root env)
                      target-node (resolve-at env-root target-path)]
                  (when-not (:vector target-node)
                    (throw (ex-info "Domain constraint: expected a vector for vector-of"
                                    {:path target-path})))
                  (let [target-resolved-path (tree/position target-node)
                        children (sort-by ::tree/name
                                          (clojure.core/filter #(integer? (::tree/name %))
                                                               (tree/children target-node)))
                        elem-schema (:element-schema schema)]
                    (reduce (fn [env-root child]
                              (let [resolved-child (resolve child)
                                    resolved-path (tree/position resolved-child)]
                                (apply-schema (tree/cd env-root my-pos) resolved-path elem-schema)))
                            env-root
                            children)))

                ;; Tuple schema → ensure vector, check length, constrain per position
                :tuple-schema
                (let [env-root (tree/root env)
                      target-node (resolve-at env-root target-path)]
                  (when-not (:vector target-node)
                    (throw (ex-info "Domain constraint: expected a vector for tuple"
                                    {:path target-path})))
                  (let [target-resolved-path (tree/position target-node)
                        children (sort-by ::tree/name
                                          (clojure.core/filter #(integer? (::tree/name %))
                                                               (tree/children target-node)))
                        elem-schemas (:element-schemas schema)
                        n (count children)
                        expected (count elem-schemas)]
                    (when (not= n expected)
                      (throw (ex-info (str "tuple domain: vector has " n " elements but tuple specifies " expected)
                                      {:actual n :expected expected})))
                    (reduce (fn [env-root [idx elem-schema]]
                              (let [child (nth children idx)
                                    resolved-child (resolve child)
                                    resolved-path (tree/position resolved-child)]
                                (apply-schema (tree/cd env-root my-pos) resolved-path elem-schema)))
                            env-root
                            (map-indexed vector elem-schemas))))

                ;; Map-of schema → ensure target is a map, constrain all entries
                :map-of-schema
                (let [env-root (tree/root env)
                      target-node (resolve-at env-root target-path)]
                  (when-not (:map target-node)
                    (throw (ex-info "Domain constraint: expected a map for map-of"
                                    {:path target-path})))
                  (let [children (tree/children target-node)
                        key-schema (:key-schema schema)
                        val-schema (:value-schema schema)
                        key-dom (when (= :type-constraint (:kind key-schema))
                                  (:domain key-schema))]
                    ;; Validate keys
                    (doseq [child children]
                      (let [k (::tree/name child)]
                        (when (and key-dom (not (dom/contains-val? key-dom k)))
                          (throw (ex-info (str "map-of domain: key " k " does not match key type")
                                          {:key k})))))
                    ;; Constrain values
                    (reduce (fn [env-root child]
                              (let [resolved-child (resolve child)
                                    resolved-path (tree/position resolved-child)]
                                (apply-schema (tree/cd env-root my-pos) resolved-path val-schema)))
                            env-root
                            children)))))]

      ;; Resolve the target argument
      (let [[env' target-path] (ensure-node-abs env target-expr)
            ;; Follow links to the real target
            resolved-target (resolve-at (tree/root env') target-path)
            resolved-path (tree/position resolved-target)
            result-root (apply-schema env' resolved-path schema)]
        (or (tree/cd result-root my-pos) result-root)))))

;; ════════════════════════════════════════════════════════════════
;; Multi-branch fn call support
;; ════════════════════════════════════════════════════════════════

(declare bind call-fn)

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
    (let [d (domain-of (tree/root env) arg-path)
          literal-dom (dom/finite #{param})
          narrowed (dom/intersect (or d dom/any) literal-dom)]
      (if (dom/void? narrowed)
        (throw (ex-info "Pattern match failed: literal mismatch"
                        {:pattern param :domain d}))
        (let [env' (set-domain (tree/root env) arg-path narrowed)]
          (or (tree/cd env' (tree/position env)) env'))))

    ;; Map/vector pattern: use bind-pattern with a seed linked to the arg
    (or (map? param) (vector? param))
    (let [gsym (gensym "param_")
          e (tree/ensure-path env [gsym])
          e (tree/upd e [gsym] #(assoc % :link arg-path))
          seed-domain (domain-of (tree/root e) arg-path)
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
                           (catch clojure.lang.ExceptionInfo e
                             ;; Recursion depth errors propagate immediately
                             (when (contains? (ex-data e) :depth)
                               (throw e))
                             e))]
              (if (instance? Throwable result)
                (recur (rest remaining) result)
                result))))))))

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
         (assoc env :link (tree/position found-target)))
       (throw (ex-info (str "Unresolved symbol: " expr) {:sym expr})))

     ;; List → dispatch on head
     (seq? expr)
     (let [[head & args] expr]
       (cond
         (symbol? head)
         (let [raw-node (tree/find env [head])
               node (if (:link raw-node) (resolve raw-node) raw-node)]
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

             :else
             (throw (ex-info (str "No :construct for form: " head) {:head head}))))

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

;; ════════════════════════════════════════════════════════════════
;; bind-pattern — domain-propagating destructuring
;; ════════════════════════════════════════════════════════════════
;;
;; The primary destructuring path. Recursively decomposes patterns
;; (symbols, maps, vectors, operator forms) against a seed binding,
;; propagating domain information to every bound symbol.
;;
;; Protocol: (fn [env pattern seed-sym seed-domain] → [env' bound-syms])
;;   env         — tree node at current scope position
;;   pattern     — unevaluated syntax (symbol, map, vector, or list)
;;   seed-sym    — symbol already bound in env to the value being destructured
;;   seed-domain — domain of the seed (dom/any when unknown)
;;   Returns [env' bound-syms] — updated env + ordered vector of all symbols bound
;; ════════════════════════════════════════════════════════════════

(declare bind-pattern bind-pattern-map bind-pattern-vec bind-pattern-operator)

(defn- extract-field-domain
  "Extract the domain for a specific map field key from a seed.
   Checks the seed node's tree children first (structural maps),
   then falls back to composite seed-domain info (:map-of), then dom/any."
  [env seed-sym key seed-domain]
  (or
   ;; Try tree children — the common case for structural maps.
   ;; When you (bind env :x 42) inside a map, the tree gets a child at [:x]
   ;; with domain single(42). This reads that directly.
   (when-let [found (tree/find env [seed-sym])]
     (let [seed-node (resolve found)]
       (when (:map seed-node)
         (when-let [child (tree/cd seed-node [key])]
           (let [target (resolve child)]
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
     (let [seed-node (resolve found)]
       (when (:vector seed-node)
         (when-let [child (tree/cd seed-node [i])]
           (let [target (resolve child)]
             (:domain target))))))
   ;; Fallback
   dom/any))

(defn domain-node
  "Create a child binding with a known domain.
   Uses the existing `bind` to create the node (which handles get/nth dispatch,
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
            target (resolve found)
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
            target (resolve found)
            target-path (tree/position target)
            d (or (:domain target) dom/any)
            literal-dom (dom/finite #{pattern})
            narrowed (dom/intersect d literal-dom)]
        (if (dom/void? narrowed)
          (throw (ex-info "Literal pattern mismatch"
                          {:pattern pattern :domain d}))
          (let [env' (set-domain (tree/root env) target-path narrowed)
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
  (let [dot-idx (.indexOf ^java.util.List pattern '.)
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
                        (resolve found))
            _ (when-not (and seed-node (:vector seed-node))
                (throw (ex-info "Cannot resolve vector for tail positionals"
                                {:pattern pattern :seed seed-sym})))
            children (sort-by ::tree/name
                              (clojure.core/filter #(integer? (::tree/name %))
                                                   (tree/children seed-node)))
            total (clojure.core/count children)
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
                               (clojure.core/range middle-start middle-end))
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
                                                (resolve wrapper-node))
                             type-dom (:type-domain wrapper-resolved)]
                         (if type-dom
                           (let [d (domain-of (tree/root e') path)
                                 narrowed (dom/intersect d type-dom)]
                             (if (dom/void? narrowed)
                               (throw (ex-info "Contradiction: destructuring type constraint"
                                               {:wrapper wrapper-head
                                                :target target-sym}))
                               (let [e'' (set-domain (tree/root e') path narrowed)]
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
                           (catch clojure.lang.ExceptionInfo e
                             e))]
              (if (instance? Throwable result)
                (recur (rest remaining) result)
                result))))))))

(defn bind-pattern-operator
  "Dispatch an operator pattern like (ks a b), (as m pat), (point x y).
   Checks for :destruct (built-in operators), then :mufl-fn (user functions).
   Returns [env' bound-syms]."
  [env [head & args] seed-sym seed-domain]
  (let [node (tree/find env [head])
        _ (when-not node
            (throw (ex-info (str "Unresolved pattern head: " head)
                            {:pattern (cons head args)})))
        resolved (resolve node)]
    (cond
      (:destruct resolved)
      ((:destruct resolved) env args seed-sym seed-domain)

      (:mufl-fn resolved)
      (fn-destruct env (:mufl-fn resolved) args seed-sym seed-domain)

      :else
      (throw (ex-info (str "No :destruct or :mufl-fn for pattern head: " head)
                      {:pattern (cons head args)})))))
