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

(defn- narrow-lt
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

(defn- narrow-gt
  "Narrow for a > b constraint."
  [env [a-path b-path]]
  ;; a > b is the same as b < a
  (narrow-lt env [b-path a-path]))

(defn- narrow-lte
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

(defn- narrow-gte
  "Narrow for a >= b."
  [env [a-path b-path]]
  (narrow-lte env [b-path a-path]))

(def composite-kinds
  "Set of composite domain kinds (including structural domains)."
  #{:vector-of :tuple :map-of :map-fields :intersection})

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
        (let [children (tree/int-children target-node)
              elem-dom (:element composite-dom)]
          (narrow-children env (map (fn [c] [c elem-dom]) children))))

      :tuple
      (if-not (:vector target-node)
        nil ;; contradiction: tuple on non-vector
        (let [children (tree/int-children target-node)
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

      :map-fields
      (if-not (:map target-node)
        nil ;; contradiction: map-fields on non-map
        (let [fields (:fields composite-dom)]
          (reduce (fn [{:keys [env changed]} [field-key field-dom]]
                    (let [target-resolved-path (tree/position target-node)
                          child-path (conj target-resolved-path field-key)
                          child (tree/cd env child-path)]
                      (if child
                        (let [resolved-child (resolve child)
                              child-path (tree/position resolved-child)
                              child-dom (or (:domain resolved-child) dom/any)
                              narrowed (dom/intersect child-dom field-dom)]
                          (cond
                            (dom/void? narrowed)
                            (reduced nil)

                            (= narrowed child-dom)
                            {:env env :changed changed}

                            :else
                            {:env (set-domain env child-path narrowed)
                             :changed (conj changed child-path)}))
                        ;; Field not present — not necessarily a contradiction
                        ;; (the map might not have this field yet)
                        {:env env :changed changed})))
                  {:env env :changed []}
                  fields)))

      :intersection
      ;; Apply each sub-domain in sequence
      (reduce (fn [{:keys [env changed]} sub-dom]
                (let [result (apply-composite-constraint env target-path sub-dom)]
                  (if (nil? result)
                    (reduced nil)
                    {:env (:env result)
                     :changed (into changed (:changed result))})))
              {:env env :changed []}
              (:domains composite-dom))

      ;; Unknown composite kind — no-op
      {:env env :changed []})))

(defn- narrow-eq
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

(defn- narrow-neq
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

(defn- narrow-plus
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

(defn- narrow-times
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

(defn- narrow-minus
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

(defn- narrow-mod
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

(defn- narrow-quot
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
   Also detects duplicate singletons and pigeonhole violations.
   Returns {:env :changed :any-change} or nil on contradiction."
  [env refs]
  (let [singletons (into {}
                         (keep (fn [p]
                                 (let [d (domain-of env p)]
                                   (when (dom/singleton? d)
                                     [p (dom/singleton-val d)]))))
                         refs)
        ;; Check for duplicate singleton values — two vars can't both be the same value
        singleton-vals (vals singletons)
        has-dup-singletons? (and (seq singleton-vals)
                                 (not (apply distinct? singleton-vals)))]
    (if has-dup-singletons?
      nil ;; contradiction: two variables share the same concrete value
      ;; Pigeonhole: if number of variables exceeds the domain union size, impossible
      (let [all-doms (mapv #(domain-of env %) refs)
            dom-union (reduce dom/unite dom/void all-doms)
            union-size (dom/size dom-union)]
        (if (and union-size (> (count refs) union-size))
          nil ;; contradiction: more variables than possible values
          (let [others (remove (set (keys singletons)) refs)]
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
             singletons)))))))

(defn- narrow-alldiff
  "Narrow for all-different constraint. Repeats until no progress."
  [env refs]
  (loop [env env
         all-changed []]
    (let [result (alldiff-pass env refs)]
      (cond
        (nil? result) nil
        (not (:any-change result)) {:env (:env result) :changed all-changed}
        :else (recur (:env result) (into all-changed (:changed result)))))))

(defn- narrow-abs
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

(defn- narrow-contains
  "Narrow membership constraint: ∃i: (nth v i) = x.
   refs: [v-path x-path & elem-paths] where v is a vector node."
  [env [v-path x-path & elem-paths]]
  (let [x-dom (domain-of env x-path)
        ;; x must be in the union of element domains
        elem-union (reduce dom/unite dom/void
                           (map #(domain-of env %) elem-paths))
        x-narrowed (dom/intersect x-dom elem-union)]
    (if (dom/void? x-narrowed)
      nil ;; contradiction — no element can match x
      (apply-narrowings env
                        (cons [x-path x-narrowed]
                              ;; Also: if x is finite, narrow each element to
                              ;; at least overlap with x (but only if element
                              ;; domain would actually change — conservative)
                              (when (dom/finite? x-narrowed)
                                ;; Don't remove elements from vector, just ensure
                                ;; the constraint is satisfiable
                                []))))))

(defn- narrow-index-of
  "Narrow for index-of constraint.
   refs: [v-path x-path result-path & elem-paths]"
  [env [v-path x-path result-path & elem-paths]]
  (let [x-dom (domain-of env x-path)
        result-dom (domain-of env result-path)
        ;; Find positions where element domain intersects x
        matching-positions
        (set (keep-indexed
              (fn [idx elem-path]
                (let [elem-dom (domain-of env elem-path)]
                  (when-not (dom/void? (dom/intersect elem-dom x-dom))
                    idx)))
              elem-paths))
        ;; Narrow result to valid positions
        new-result (dom/intersect result-dom (dom/finite matching-positions))]
    (if (dom/void? new-result)
      nil ;; contradiction
      (let [;; If result is singleton, narrow x to that element's domain
            x-target-dom (if (dom/singleton? new-result)
                           (let [idx (dom/singleton-val new-result)]
                             (domain-of env (nth elem-paths idx)))
                           ;; Narrow x to union of matching element domains
                           (reduce dom/unite dom/void
                                   (keep (fn [idx]
                                           (when (dom/contains-val? new-result idx)
                                             (domain-of env (nth elem-paths idx))))
                                         (range (count elem-paths)))))
            x-narrowed (dom/intersect x-dom x-target-dom)]
        (if (dom/void? x-narrowed)
          nil
          (apply-narrowings env [[result-path new-result] [x-path x-narrowed]]))))))

(defn- narrow-min-of
  "Narrow for min-of constraint: result = min(elements).
   refs: [result-path & elem-paths]
   - result ≤ every element
   - result ∈ union of element domains
   - each element ≥ result"
  [env [result-path & elem-paths]]
  (let [result-dom (domain-of env result-path)
        elem-doms (mapv #(domain-of env %) elem-paths)
        ;; result must be in the union of element domains
        elem-union (reduce dom/unite dom/void elem-doms)
        ;; result can't exceed the smallest maximum among elements
        upper-bound (reduce (fn [acc d]
                              (let [mx (dom/domain-max d)]
                                (if (and acc mx) (min acc mx) (or acc mx))))
                            nil elem-doms)
        result-candidate (dom/intersect result-dom elem-union)
        result-new (if upper-bound
                     (dom/intersect result-candidate (dom/range-dom nil upper-bound))
                     result-candidate)]
    (if (dom/void? result-new)
      nil ;; contradiction
      ;; Each element must be ≥ min(result domain)
      (let [result-min (dom/domain-min result-new)
            elem-narrowings (when result-min
                              (keep (fn [elem-path]
                                      (let [elem-dom (domain-of env elem-path)
                                            narrowed (dom/domain-at-least elem-dom result-min)]
                                        (when-not (= narrowed elem-dom)
                                          [elem-path narrowed])))
                                    elem-paths))]
        (apply-narrowings env (cons [result-path result-new]
                                    elem-narrowings))))))

(defn- narrow-max-of
  "Narrow for max-of constraint: result = max(elements).
   refs: [result-path & elem-paths]
   - result ≥ every element
   - result ∈ union of element domains
   - each element ≤ result"
  [env [result-path & elem-paths]]
  (let [result-dom (domain-of env result-path)
        elem-doms (mapv #(domain-of env %) elem-paths)
        ;; result must be in the union of element domains
        elem-union (reduce dom/unite dom/void elem-doms)
        ;; result can't be less than the largest minimum among elements
        lower-bound (reduce (fn [acc d]
                              (let [mn (dom/domain-min d)]
                                (if (and acc mn) (max acc mn) (or acc mn))))
                            nil elem-doms)
        result-candidate (dom/intersect result-dom elem-union)
        result-new (if lower-bound
                     (dom/intersect result-candidate (dom/range-dom lower-bound nil))
                     result-candidate)]
    (if (dom/void? result-new)
      nil ;; contradiction
      ;; Each element must be ≤ max(result domain)
      (let [result-max (dom/domain-max result-new)
            elem-narrowings (when result-max
                              (keep (fn [elem-path]
                                      (let [elem-dom (domain-of env elem-path)
                                            narrowed (dom/domain-at-most elem-dom result-max)]
                                        (when-not (= narrowed elem-dom)
                                          [elem-path narrowed])))
                                    elem-paths))]
        (apply-narrowings env (cons [result-path result-new]
                                    elem-narrowings))))))

(defn- narrow-sorted-perm
  "Narrow for sorted-permutation constraint.
   refs: [input-path-0 ... input-path-(n-1) output-path-0 ... output-path-(n-1)]
   Length is always 2*n (equal number of input and output paths).

   Enforces: output is a sorted permutation of input.
   - Forward:  each output[i] ∈ union(input domains), bounded by ordering
   - Backward: each input[j] ∈ union(output domains)
   - Singleton counting: if a value appears exactly k times as input singletons,
     then exactly k output positions must hold that value
   - Ground output: if all inputs are singleton, compute sorted order directly"
  [env refs]
  (let [refs (vec refs)
        n (/ (count refs) 2)
        input-paths (subvec refs 0 n)
        output-paths (subvec refs n)

        input-doms (mapv #(domain-of env %) input-paths)
        output-doms (mapv #(domain-of env %) output-paths)

        ;; Check if all inputs are ground — if so, compute sorted result directly
        input-singletons (mapv #(when (dom/singleton? %) (dom/singleton-val %)) input-doms)
        all-input-ground? (every? some? input-singletons)]

    (if all-input-ground?
      ;; All inputs ground: set each output to its exact sorted value
      (let [sorted-vals (vec (sort input-singletons))]
        (apply-narrowings env
                          (keep-indexed
                           (fn [i opath]
                             (let [target (dom/single (nth sorted-vals i))
                                   od (nth output-doms i)]
                               (when-not (= target od)
                                 [opath target])))
                           output-paths)))

      ;; General case: propagate via domain bounds
      (let [;; Union of all input domains — every output element must come from here
            input-union (reduce dom/unite dom/void input-doms)
            ;; Union of all output domains — every input element must appear somewhere
            output-union (reduce dom/unite dom/void output-doms)

            ;; Global bounds from input
            global-min (reduce (fn [acc d]
                                 (let [m (dom/domain-min d)]
                                   (if (and acc m) (min acc m) (or acc m))))
                               nil input-doms)
            global-max (reduce (fn [acc d]
                                 (let [m (dom/domain-max d)]
                                   (if (and acc m) (max acc m) (or acc m))))
                               nil input-doms)

            ;; Singleton counting from inputs: {value → count}
            ;; Values that are definitely in the input (singleton domains)
            input-val-counts (reduce (fn [acc d]
                                       (if (dom/singleton? d)
                                         (update acc (dom/singleton-val d) (fnil inc 0))
                                         acc))
                                     {} input-doms)

            ;; Forward narrowing: output[i] ⊆ input-union, with positional bounds
            output-narrowings
            (keep-indexed
             (fn [i opath]
               (let [od (nth output-doms i)
                     ;; Intersect with input union
                     narrowed (dom/intersect od input-union)
                     ;; output[0] ≥ global min
                     narrowed (if (and (zero? i) global-min)
                                (dom/intersect narrowed (dom/range-dom global-min nil))
                                narrowed)
                     ;; output[n-1] ≤ global max
                     narrowed (if (and (= i (dec n)) global-max)
                                (dom/intersect narrowed (dom/range-dom nil global-max))
                                narrowed)
                     ;; Ordering bounds from neighbors:
                     ;; output[i] ≥ min(output[i-1]) and output[i] ≤ max(output[i+1])
                     narrowed (if (pos? i)
                                (let [prev-min (dom/domain-min (nth output-doms (dec i)))]
                                  (if prev-min
                                    (dom/intersect narrowed (dom/range-dom prev-min nil))
                                    narrowed))
                                narrowed)
                     narrowed (if (< i (dec n))
                                (let [next-max (dom/domain-max (nth output-doms (inc i)))]
                                  (if next-max
                                    (dom/intersect narrowed (dom/range-dom nil next-max))
                                    narrowed))
                                narrowed)]
                 (when-not (= narrowed od)
                   [opath narrowed])))
             output-paths)

            ;; Backward narrowing: input[j] ⊆ output-union
            input-narrowings
            (keep-indexed
             (fn [j ipath]
               (let [id (nth input-doms j)
                     narrowed (dom/intersect id output-union)]
                 (when-not (= narrowed id)
                   [ipath narrowed])))
             input-paths)

            ;; Singleton-count narrowing:
            ;; If value v appears k times in input singletons, and we know which
            ;; output positions could hold v (sorted order), we can narrow.
            ;; Specifically: if only k output positions have v in their domain,
            ;; those positions MUST be v.
            singleton-narrowings
            (mapcat
             (fn [[v cnt]]
               ;; Find output positions whose domain contains v
               (let [candidate-indices (keep-indexed
                                        (fn [i od]
                                          (when (dom/contains-val? od v) i))
                                        output-doms)]
                 ;; If exactly cnt positions can hold v, all must be v
                 (when (= (count candidate-indices) cnt)
                   (keep (fn [i]
                           (let [od (nth output-doms i)]
                             (when-not (dom/singleton? od)
                               [(nth output-paths i) (dom/single v)])))
                         candidate-indices))))
             input-val-counts)]

        (apply-narrowings env (concat output-narrowings
                                      input-narrowings
                                      singleton-narrowings))))))

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
   :alldiff narrow-alldiff
   :contains narrow-contains
   :index-of narrow-index-of
   :min-of narrow-min-of
   :max-of narrow-max-of
   :sorted-perm narrow-sorted-perm})

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
;; Watcher propagation helper
;; ════════════════════════════════════════════════════════════════

(defn propagate-watchers
  "Look up watchers on the node at path, propagate if any.
   Returns the updated env, or nil on contradiction."
  [env path]
  (let [node (tree/cd env path)
        watchers (:watched-by node)]
    (if (seq watchers)
      (propagate env (vec watchers))
      env)))
