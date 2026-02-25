(ns mufl.bind
  "Core bind + constraint propagation for mufl.

   bind takes [env expr] and returns a new tree where:
   1. New nodes are created for the expression
   2. Constraint nodes are added
   3. All domains are propagated to fixpoint

   After every bind, the tree is maximally narrowed."
  (:refer-clojure :exclude [resolve destructure])
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

(defn apply-composite-constraint
  "Apply a composite domain's structural constraints to a collection node.
   Returns {:env env' :changed [paths...]} or nil on contradiction.
   
   For :vector-of — intersects each child's domain with the element domain.
   For :tuple — checks length, intersects each position's domain.
   For :map-of — validates keys, intersects each value's domain."
  [env target-path composite-dom]
  (let [target-node (resolve-at env target-path)
        target-resolved-path (tree/position target-node)]
    (case (:kind composite-dom)

      :vector-of
      (if-not (:vector target-node)
        nil ;; contradiction: composite domain on non-vector
        (let [children (sort-by ::tree/name
                                (filter #(integer? (::tree/name %))
                                        (tree/children target-node)))
              elem-dom (:element composite-dom)]
          (reduce (fn [{:keys [env changed]} child]
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
                  children)))

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
            (reduce (fn [{:keys [env changed]} [idx elem-dom]]
                      (let [child (nth children idx)
                            resolved-child (resolve child)
                            child-path (tree/position resolved-child)
                            child-dom (or (:domain resolved-child) dom/any)
                            narrowed (dom/intersect child-dom elem-dom)]
                        (cond
                          (dom/void? narrowed)
                          (reduced nil)

                          (= narrowed child-dom)
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
                                env' (if (and (composite-kinds (:kind elem-dom))
                                              (or (:vector resolved-child) (:map resolved-child)))
                                       (let [inner-result (apply-composite-constraint env' child-path elem-dom)]
                                         (if (nil? inner-result)
                                           (reduced nil)
                                           (:env inner-result)))
                                       env')]
                            (if (reduced? env')
                              env'
                              {:env env' :changed (conj changed child-path)})))))
                    {:env env :changed []}
                    (map-indexed vector elem-doms)))))

      :map-of
      (if-not (:map target-node)
        nil ;; contradiction: map-of on non-map
        (let [children (tree/children target-node)
              key-dom (:key composite-dom)
              val-dom (:value composite-dom)]
          ;; Validate keys
          (if (some (fn [child]
                      (let [k (::tree/name child)]
                        (not (dom/contains-val? key-dom k))))
                    children)
            nil ;; contradiction: key doesn't match
            (reduce (fn [{:keys [env changed]} child]
                      (let [resolved-child (resolve child)
                            child-path (tree/position resolved-child)
                            child-dom (or (:domain resolved-child) dom/any)
                            narrowed (dom/intersect child-dom val-dom)]
                        (cond
                          (dom/void? narrowed)
                          (reduced nil)

                          (= narrowed child-dom)
                          (if (and (composite-kinds (:kind val-dom))
                                   (or (:vector resolved-child) (:map resolved-child)))
                            (let [inner-result (apply-composite-constraint env child-path val-dom)]
                              (if (nil? inner-result)
                                (reduced nil)
                                {:env (:env inner-result)
                                 :changed (into changed (:changed inner-result))}))
                            {:env env :changed changed})

                          :else
                          (let [env' (set-domain env child-path narrowed)
                                env' (if (and (composite-kinds (:kind val-dom))
                                              (or (:vector resolved-child) (:map resolved-child)))
                                       (let [inner-result (apply-composite-constraint env' child-path val-dom)]
                                         (if (nil? inner-result)
                                           (reduced nil)
                                           (:env inner-result)))
                                       env')]
                            (if (reduced? env')
                              env'
                              {:env env' :changed (conj changed child-path)})))))
                    {:env env :changed []}
                    children))))

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

(defn- collect-constraints
  "Find all constraint nodes under the ::constraints subtree of the given scope."
  ([env]
   (collect-constraints env []))
  ([env scope-path]
   (let [root (tree/root env)
         constraints-path (conj (vec scope-path) ::constraints)
         constraints-node (tree/cd root constraints-path)]
     (if constraints-node
       (->> (tree/children constraints-node)
            (keep (fn [child]
                    (when (:constraint child)
                      (tree/position child))))
            vec)
       []))))

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

(defn resolve-to-path
  "Given an expression that refers to a name, resolve it to its path in the tree."
  [env expr]
  (cond
    (symbol? expr)
    (when-let [found (tree/find env [expr])]
      (tree/position (resolve found)))

    :else nil))

(declare bind)
(declare apply-domain-constraint)

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

(defn destructure
  "Destructure a pattern against an expression in a let binding.
   
   Dispatches through `:bind` on tree nodes when the pattern is a list
   form `(head args...)`. The head symbol is looked up via `tree/find`,
   and its `:bind` function is called with the pattern args plus the
   value expression appended as the last arg.
   
   Map patterns:  {:key1 pat1 :key2 pat2}       → binds pat1 to (get expr :key1), etc.
                  {:key1 pat1 . rest}            → rest gets remaining keys via dissoc.
   
   Vector patterns: [a b c]                      → binds a to (nth expr 0), etc.
                    [a b . xs]                   → xs gets (drop 2 expr).
   
   List patterns:  (ks a b c)                    → destructuring operator dispatch
                   (as name pattern)             → bind whole + inner destructure
                   (DomainName inner-pattern)    → domain constraint + destructure
   
   Returns the env with all bindings established."
  [env pattern expr]
  (cond
    ;; Symbol pattern: bind sym to expr (base case for nested destructuring)
    (symbol? pattern)
    (bind env pattern expr)

    ;; Map destructuring: {:key1 pat1 :key2 pat2 . rest}
    ;; General patterns + optional dot rest. No :keys or :as sugar.
    ;; Use (as m {:key pat}) operator for whole-value capture.
    (map? pattern)
    (let [dot-val    (get pattern '.)
          ;; The actual key-pattern entries (everything except .)
          entries    (dissoc pattern '.)
          ;; Helpful error for legacy {:keys [...]} usage
          _ (when (:keys pattern)
              (throw (ex-info (str "The {:keys [...]} form is not supported. "
                                   "Use {:x x :y y} or (ks x y) instead.")
                              {:pattern pattern})))
          ;; Validate that all keys are keywords
          _ (doseq [k (keys entries)]
              (when-not (keyword? k)
                (throw (ex-info (str "Map destructuring keys must be keywords, got: " (pr-str k)
                                     " (type: " (type k) ")")
                                {:pattern pattern :key k}))))
          ;; Bind the whole expression to a temp name
          tmp (gensym "destruct__")
          env (bind env tmp expr)
          ;; For each key-pattern pair, bind via get
          env (reduce (fn [e [k v-pattern]]
                        (let [get-expr (list 'get tmp k)]
                          (if (symbol? v-pattern)
                            (bind e v-pattern get-expr)
                            (destructure e v-pattern get-expr))))
                      env
                      entries)]
      ;; Handle dot rest: bind remaining keys (dissoc the named keys)
      (if dot-val
        (let [remove-keys (keys entries)
              dissoc-expr (apply list 'dissoc tmp remove-keys)]
          (if (symbol? dot-val)
            (bind env dot-val dissoc-expr)
            (destructure env dot-val dissoc-expr)))
        env))

    ;; [a b c] — positional vector destructuring
    ;; Supports: [a b . xs] (dot rest), [a b .] (bare dot)
    ;; Extended: [a . mid c] (head/middle/last decomposition)
    ;;   - 1 element after dot = rest binding (backward compatible)
    ;;   - 2+ elements after dot = first is rest/middle, remaining are tail positionals
    ;; No :as in vectors. Use (as v [a b c]) operator instead.
    (vector? pattern)
    (let [;; Check for dot syntax: [a b . xs]
          dot-idx (.indexOf ^java.util.List pattern '.)
          has-dot? (>= dot-idx 0)
          ;; Elements before dot → positional bindings from start
          positional (if has-dot?
                       (subvec pattern 0 dot-idx)
                       pattern)
          ;; Elements after dot: partition into rest-pattern + tail-positionals
          ;; 0 after dot = bare dot (no rest, no tail)
          ;; 1 after dot = rest binding only (backward compatible)
          ;; 2+ after dot = first is rest/middle, rest are tail positionals
          after-dot (when has-dot?
                      (subvec pattern (inc dot-idx)))
          rest-pattern (when (and has-dot? (seq after-dot))
                         (first after-dot))
          tail-positionals (when (and has-dot? (> (count after-dot) 1))
                             (subvec after-dot 1))
          ;; Bind the whole expression to a temp name
          tmp (gensym "destruct__")
          env (bind env tmp expr)
          ;; Bind head positional elements via (nth tmp i)
          env (reduce (fn [e [i pat]]
                        (if (symbol? pat)
                          (bind e pat (list 'nth tmp i))
                          ;; Nested destructuring
                          (destructure e pat (list 'nth tmp i))))
                      env
                      (map-indexed vector positional))]
      (if tail-positionals
        ;; Head/middle/tail decomposition: resolve tmp to get total count
        (let [;; Resolve the vector node to count children (same approach as drop)
              vec-node (when-let [found (tree/find env [tmp])]
                         (resolve found))
              _ (when-not vec-node
                  (throw (ex-info (str "Cannot resolve vector for tail positionals: " tmp)
                                  {:pattern pattern})))
              _ (when-not (:vector vec-node)
                  (throw (ex-info (str "Not a vector node for tail positionals: " tmp)
                                  {:pattern pattern})))
              children (sort-by ::tree/name
                                (clojure.core/filter #(integer? (::tree/name %))
                                                     (tree/children vec-node)))
              total (clojure.core/count children)
              head-count (count positional)
              tail-count (count tail-positionals)
              min-required (+ head-count tail-count)
              _ (when (> min-required total)
                  (throw (ex-info (str "Vector has " total " elements but pattern requires at least "
                                       min-required " (head: " head-count ", tail: " tail-count ")")
                                  {:pattern pattern :total total :required min-required})))
              middle-start head-count
              middle-end (- total tail-count)
              ;; Bind tail positionals via (nth tmp concrete-index) from the end
              env (reduce (fn [e [i pat]]
                            (let [idx (+ middle-end i)]
                              (if (symbol? pat)
                                (bind e pat (list 'nth tmp idx))
                                (destructure e pat (list 'nth tmp idx)))))
                          env
                          (map-indexed vector tail-positionals))
              ;; Bind middle/rest as a slice: build vector of (nth tmp i) for range [middle-start, middle-end)
              middle-exprs (mapv (fn [i] (list 'nth tmp i))
                                 (clojure.core/range middle-start middle-end))]
          (if (symbol? rest-pattern)
            (bind env rest-pattern (vec middle-exprs))
            (destructure env rest-pattern (vec middle-exprs))))
        ;; Simple rest (0 or 1 element after dot) — original behavior
        (if rest-pattern
          (let [drop-n (count positional)
                rest-expr (list 'drop drop-n tmp)]
            (if (symbol? rest-pattern)
              (bind env rest-pattern rest-expr)
              (destructure env rest-pattern rest-expr)))
          env)))

    ;; (head args...) — look up head, dispatch via :bind with value appended
    ;; Convention: destructure calls (:bind node) with (concat pattern-args [value])
    ;; as a vector with ^:destructuring metadata. The :bind function uses
    ;; (last args) to get the value being destructured and (butlast args)
    ;; for the pattern elements.
    (seq? pattern)
    (let [[head & args] pattern
          node (tree/find env [head])
          _ (when-not node
              (throw (ex-info (str "Unresolved pattern head: " head)
                              {:pattern pattern})))
          resolved (resolve node)]
      (cond
        ;; Node has :bind — call it with pattern args + value appended
        (:bind resolved)
        ((:bind resolved) env (with-meta (vec (concat args [expr]))
                                {:destructuring true}))

        :else
        (throw (ex-info (str "No :bind for pattern head: " head)
                        {:pattern pattern}))))

    :else
    (throw (ex-info "Invalid destructuring pattern" {:pattern pattern}))))

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
;; Domain definitions (defdomain support)
;; ════════════════════════════════════════════════════════════════

(defn resolve-domain-schema
  "Resolve a domain schema expression into a domain-def map.
   
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
    ;; Symbol → resolve to type domain or domain-def reference
    (symbol? schema-expr)
    (if-let [found (tree/find env [schema-expr])]
      (let [resolved (resolve found)]
        (cond
          ;; Type domain primitive (string, integer, etc.)
          (:type-domain resolved)
          {:kind :type-constraint :domain (:type-domain resolved)}

          ;; Reference to another domain def
          (:domain-def resolved)
          (:domain-def resolved)

          :else
          (throw (ex-info (str "Symbol is not a domain type: " schema-expr)
                          {:sym schema-expr}))))
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
  "Apply a domain definition as a constraint to an argument.
   (DomainName arg) constrains arg to match the domain's schema.
   Returns the env at the same position it was called from."
  [env domain-def args]
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
            result-root (apply-schema env' resolved-path domain-def)]
        (or (tree/cd result-root my-pos) result-root)))))

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
             ;; Built-in form with :bind
             (:bind node)
             ((:bind node) env args)

             ;; User-defined function with :mufl-fn
             (:mufl-fn node)
             (do
               (when (>= *call-depth* max-call-depth)
                 (throw (ex-info (str "Recursion depth limit exceeded (depth=" *call-depth* ")")
                                 {:depth *call-depth* :fn head})))
               (binding [*call-depth* (inc *call-depth*)]
                 (let [{:keys [params body]} (:mufl-fn node)
                       _ (when (not= (count params) (count args))
                           (throw (ex-info (str "Arity mismatch: expected " (count params)
                                                " args, got " (count args))
                                           {:params params :args args :fn-name head})))
                       my-pos (tree/position env)
                       ;; Evaluate arg expressions in the CALLER's scope first.
                       ;; This ensures (f (- n 1)) evaluates (- n 1) using the
                       ;; caller's n, not the callee's param n which shadows it.
                       [env-after-args arg-paths]
                       (reduce (fn [[e paths] arg-expr]
                                 (let [[e' path] (ensure-node-abs e arg-expr)]
                                   [e' (conj paths path)]))
                               [env []]
                               args)
                       ;; Create a call scope as child of current position
                       call-name (gensym "call")
                       env-with-scope (tree/ensure-path env-after-args [call-name])
                       call-env (tree/cd env-with-scope [call-name])
                       ;; Link each param to the pre-evaluated arg node
                       env-bound (reduce (fn [e [param arg-path]]
                                           (let [e (tree/ensure-path e [param])]
                                             (tree/upd e [param]
                                                       #(assoc % :link arg-path))))
                                         call-env
                                         (map vector params arg-paths))
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
                     (assoc caller :link target-path)))))

             :else
             (throw (ex-info (str "No :bind for form: " head) {:head head}))))

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
