(ns mufl.show
  "Tree-to-code serialization: show : tree → mufl-code.

   Reconstructs a mufl expression from a (possibly non-ground) constraint tree.
   Ground values become literals, non-ground domains become constructors, and
   when sharing or cross-constraints exist, the output wraps in a let expression
   with named variables."
  (:require [mufl.tree :as tree]
            [mufl.domain :as dom]
            [mufl.bind :as bind]))

;; ════════════════════════════════════════════════════════════════
;; Domain → mufl constructor expression
;; ════════════════════════════════════════════════════════════════

(defn show-domain
  "Convert a domain to its mufl constructor expression.
   Returns a mufl expression (symbol, list, vector, or literal).
   Half-bounded ranges return nil (they require named variables)."
  [d]
  (case (:kind d)
    :void   nil
    :any    '(free)
    :single (:value d)
    :finite (apply list 'one-of (sort (:values d)))
    :type   (list (symbol (name (:type d))))
    :range  (let [{:keys [lo hi]} d]
              (cond
                (and (nil? lo) (nil? hi)) '(integer)
                (and lo hi)               (list 'between lo hi)
                :else                     nil))
    :spiral '(integer)
    :vector-of (list 'vector-of (show-domain (:element d)))
    :tuple     (mapv show-domain (:elements d))
    :map-of    (list 'map-of (show-domain (:key d)) (show-domain (:value d)))
    nil))

;; ════════════════════════════════════════════════════════════════
;; Helpers
;; ════════════════════════════════════════════════════════════════

;; ════════════════════════════════════════════════════════════════
;; Leaf collection (for sharing/constraint detection)
;; ════════════════════════════════════════════════════════════════

(defn- collect-leaves
  "Walk the output tree, collecting non-ground leaf nodes.
   Returns [{:output-pos [...] :resolved-path [...] :domain d} ...]"
  [env node output-pos visited]
  (let [resolved (bind/resolve node)
        rpath (tree/position resolved)
        d (:domain resolved)]
    (cond
      (contains? visited rpath) []

      (:vector resolved)
      (vec (mapcat (fn [c]
                     (collect-leaves env c
                                    (conj output-pos (::tree/name c))
                                    (conj visited rpath)))
                   (tree/int-children resolved)))

      (:map resolved)
      (vec (mapcat (fn [c]
                     (collect-leaves env c
                                    (conj output-pos (::tree/name c))
                                    (conj visited rpath)))
                   (tree/kw-children resolved)))

      (and d (dom/singleton? d)) []

      d [{:output-pos output-pos
          :resolved-path rpath
          :domain d}]

      :else [])))

;; ════════════════════════════════════════════════════════════════
;; Sharing detection
;; ════════════════════════════════════════════════════════════════

(defn- find-shared
  "Find resolved paths that appear more than once in the leaves."
  [leaves]
  (let [freqs (frequencies (map :resolved-path leaves))]
    (set (keep (fn [[path cnt]] (when (> cnt 1) path)) freqs))))

;; ════════════════════════════════════════════════════════════════
;; Half-bounded range detection
;; ════════════════════════════════════════════════════════════════

(defn- half-bounded-range?
  "Is this domain a half-bounded range (needs variable + constraint)?"
  [d]
  (and (= :range (:kind d))
       (or (and (:lo d) (nil? (:hi d)))
           (and (nil? (:lo d)) (:hi d)))))

;; ════════════════════════════════════════════════════════════════
;; Residual constraint extraction
;; ════════════════════════════════════════════════════════════════

(defn- collect-residual-constraints
  "Find residual constraints relevant to the output leaves.
   A constraint is residual if ≥2 of its refs are non-ground and in the leaf set."
  [env leaves]
  (let [leaf-paths (set (map :resolved-path leaves))
        constraint-paths (->> leaf-paths
                              (mapcat (fn [p]
                                        (let [node (tree/cd env p)]
                                          (when node (:watched-by node)))))
                              distinct)
        seen (atom #{})]
    (->> constraint-paths
         (keep (fn [c-path]
                 (when-not (contains? @seen c-path)
                   (swap! seen conj c-path)
                   (let [c-node (tree/cd env c-path)]
                     (when (and c-node (:constraint c-node))
                       (let [refs (:refs c-node)
                             non-ground-in-output
                             (filter (fn [rp]
                                       (and (contains? leaf-paths rp)
                                            (not (dom/singleton? (bind/domain-of env rp)))))
                                     refs)]
                         (when (>= (count non-ground-in-output) 2)
                           {:op (:constraint c-node)
                            :refs (vec refs)})))))))
         vec)))

;; ════════════════════════════════════════════════════════════════
;; Name assignment
;; ════════════════════════════════════════════════════════════════

(defn- user-name?
  "Is this a user-provided name (not a gensym)?"
  [sym]
  (and (symbol? sym)
       (not (re-find #"__\d+$" (str sym)))
       (not (re-find #"^\d" (str sym)))
       (not (re-find #"^(lit|expr|call|result_|seed_|param_|ws)" (str sym)))))

(defn- assign-names
  "Assign human-readable names to paths that need them.
   Prefers the node's ::tree/name if it's a user-provided symbol."
  [env paths]
  (let [used (atom #{})
        gen-counter (atom 0)]
    (into {}
          (map (fn [path]
                 (let [node (tree/cd env path)
                       nm (when node (::tree/name node))
                       chosen
                       (if (and (user-name? nm) (not (contains? @used nm)))
                         nm
                         (loop []
                           (let [idx @gen-counter
                                 s (symbol (str (char (+ (int \a) (mod idx 26)))
                                                (when (>= idx 26) (quot idx 26))))]
                             (swap! gen-counter inc)
                             (if (contains? @used s)
                               (recur)
                               s))))]
                   (swap! used conj chosen)
                   [path chosen])))
          paths)))

;; ════════════════════════════════════════════════════════════════
;; Let binding RHS generation
;; ════════════════════════════════════════════════════════════════

(defn- domain->let-rhs
  "Convert a domain to the RHS of a let binding + extra constraints.
   Returns {:rhs <expr> :constraints [<constraint-exprs>]}."
  [d name-sym]
  (case (:kind d)
    :any    {:rhs '(free) :constraints []}
    :single {:rhs (:value d) :constraints []}
    :finite {:rhs (apply list 'one-of (sort (:values d))) :constraints []}
    :type   {:rhs (list (symbol (name (:type d)))) :constraints []}
    :range  (let [{:keys [lo hi]} d]
              (cond
                (and (nil? lo) (nil? hi))
                {:rhs '(integer) :constraints []}

                (and lo hi)
                {:rhs (list 'between lo hi) :constraints []}

                lo
                {:rhs '(integer) :constraints [(list '>= name-sym lo)]}

                hi
                {:rhs '(integer) :constraints [(list '<= name-sym hi)]}))
    :spiral {:rhs '(integer) :constraints []}
    :vector-of {:rhs (list 'vector-of (show-domain (:element d))) :constraints []}
    :tuple     {:rhs (vec (mapv show-domain (:elements d))) :constraints []}
    :map-of    {:rhs (list 'map-of (show-domain (:key d)) (show-domain (:value d))) :constraints []}
    {:rhs '(free) :constraints []}))

;; ════════════════════════════════════════════════════════════════
;; Constraint → mufl expression
;; ════════════════════════════════════════════════════════════════

(defn- ref->expr
  "Convert a constraint ref path to a mufl expression.
   Named paths use the name; ground paths use the literal."
  [env path names]
  (if-let [nm (get names path)]
    nm
    (let [d (bind/domain-of env path)]
      (if (dom/singleton? d)
        (dom/singleton-val d)
        path))))

(defn- constraint->expr
  "Convert a residual constraint to a mufl expression."
  [env {:keys [op refs]} names]
  (let [exprs (mapv #(ref->expr env % names) refs)]
    (case op
      :=       (list '= (nth exprs 0) (nth exprs 1))
      :!=      (list '!= (nth exprs 0) (nth exprs 1))
      :<       (list '< (nth exprs 0) (nth exprs 1))
      :>       (list '> (nth exprs 0) (nth exprs 1))
      :<=      (list '<= (nth exprs 0) (nth exprs 1))
      :>=      (list '>= (nth exprs 0) (nth exprs 1))
      :+       (list '= (nth exprs 2) (list '+ (nth exprs 0) (nth exprs 1)))
      :-       (list '= (nth exprs 2) (list '- (nth exprs 0) (nth exprs 1)))
      :*       (list '= (nth exprs 2) (list '* (nth exprs 0) (nth exprs 1)))
      :mod     (list '= (nth exprs 2) (list 'mod (nth exprs 0) (nth exprs 1)))
      :quot    (list '= (nth exprs 2) (list 'quot (nth exprs 0) (nth exprs 1)))
      :alldiff (list 'distinct (vec exprs))
      :abs     (list '= (nth exprs 1) (list 'abs (nth exprs 0)))
      (list 'constraint op exprs))))

;; ════════════════════════════════════════════════════════════════
;; Fork emission
;; ════════════════════════════════════════════════════════════════

(defn- show-fork
  "Emit a fork as if/cond expression."
  [fork]
  (case (:kind fork)
    :if   (if (some? (:else fork))
            (list 'if (:test fork) (:then fork) (:else fork))
            (list 'if (:test fork) (:then fork)))
    :cond (apply list 'cond (mapcat identity (:branches fork)))))

;; ════════════════════════════════════════════════════════════════
;; Bare emission (no variable names needed)
;; ════════════════════════════════════════════════════════════════

(defn- show-bare
  "Emit a node as bare expression without variable names."
  [env node]
  (let [resolved (bind/resolve node)
        d (:domain resolved)]
    (cond
      (:vector resolved)
      (mapv #(show-bare env %) (tree/int-children resolved))

      (:map resolved)
      (into {} (map (fn [c] [(::tree/name c) (show-bare env c)]))
            (tree/kw-children resolved))

      (:fork resolved)
      (show-fork (:fork resolved))

      (and d (dom/singleton? d))
      (dom/singleton-val d)

      d
      (show-domain d)

      :else nil)))

;; ════════════════════════════════════════════════════════════════
;; Named emission (with variable substitution)
;; ════════════════════════════════════════════════════════════════

(defn- show-with-names
  "Emit the output structure, substituting variable names where needed."
  [env node names]
  (let [resolved (bind/resolve node)
        rpath (tree/position resolved)
        d (:domain resolved)]
    (cond
      ;; Named non-structural node → use variable name
      (and (contains? names rpath)
           (not (:vector resolved))
           (not (:map resolved)))
      (get names rpath)

      ;; Vector
      (:vector resolved)
      (mapv #(show-with-names env % names) (tree/int-children resolved))

      ;; Map
      (:map resolved)
      (into {} (map (fn [c] [(::tree/name c) (show-with-names env c names)]))
            (tree/kw-children resolved))

      ;; Fork
      (:fork resolved)
      (show-fork (:fork resolved))

      ;; Ground singleton
      (and d (dom/singleton? d))
      (dom/singleton-val d)

      ;; Non-ground without name → bare domain constructor
      d
      (or (show-domain d) '(free))

      :else nil)))

;; ════════════════════════════════════════════════════════════════
;; Main entry point
;; ════════════════════════════════════════════════════════════════

(defn show
  "Reconstruct a mufl expression from a (possibly non-ground) constraint tree.

   (show env path) → mufl s-expression

   Ground values become literals. Non-ground domains become constructor
   expressions. When sharing or cross-constraints exist, the output wraps
   in a let expression with named variables and constraint body."
  [env path]
  (let [env (tree/root env)
        node (tree/cd env path)]
    (when node
      (let [leaves (collect-leaves env node [] #{})
            shared (find-shared leaves)
            residual (collect-residual-constraints env leaves)

            ;; Collect all paths that need variable names
            leaf-path-set (set (map :resolved-path leaves))
            paths-needing-names
            (vec (distinct
                  (concat
                   ;; Shared paths
                   (keep (fn [l] (when (contains? shared (:resolved-path l))
                                   (:resolved-path l)))
                         leaves)
                   ;; Half-bounded range paths
                   (keep (fn [l] (when (half-bounded-range? (:domain l))
                                   (:resolved-path l)))
                         leaves)
                   ;; Paths in residual constraints
                   (mapcat (fn [{:keys [refs]}]
                             (filter (fn [p]
                                       (and (contains? leaf-path-set p)
                                            (not (dom/singleton? (bind/domain-of env p)))))
                                     refs))
                           residual))))]

        (if (empty? paths-needing-names)
          ;; Simple: bare emission
          (show-bare env node)

          ;; Complex: let wrapper with names + constraints
          (let [names (assign-names env paths-needing-names)

                binding-info
                (mapv (fn [p]
                        (let [d (bind/domain-of env p)
                              nm (get names p)]
                          (assoc (domain->let-rhs d nm) :sym nm)))
                      paths-needing-names)

                let-bindings (vec (mapcat (fn [{:keys [sym rhs]}] [sym rhs])
                                         binding-info))

                extra-constraints (vec (mapcat :constraints binding-info))
                residual-exprs (mapv #(constraint->expr env % names) residual)
                all-constraints (concat extra-constraints residual-exprs)

                body (show-with-names env node names)]

            (if (empty? all-constraints)
              (list 'let let-bindings body)
              (apply list 'let let-bindings
                     (concat all-constraints [body])))))))))
