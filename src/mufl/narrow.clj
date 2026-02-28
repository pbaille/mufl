(ns mufl.narrow
  "Constraint narrowing functions and propagation engine for mufl.

   Each narrow-* function takes [env refs] and returns
   {:env env' :changed [paths...]} or nil (contradiction).

   Also provides tree navigation helpers (resolve, domain-of, set-domain)
   used across the system."
  (:refer-clojure :exclude [resolve])
  (:require [mufl.tree :as tree]
            [mufl.domain :as dom]))

;; ════════════════════════════════════════════════════════════════
;; Tree navigation helpers
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
