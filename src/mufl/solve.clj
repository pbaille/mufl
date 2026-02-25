(ns mufl.solve
  "Search/enumeration for mufl.

   After bind has propagated domains to fixpoint, solve enumerates
   concrete solutions by forking on non-singleton domain nodes.

   Uses backtracking search with smallest-domain-first heuristic."
  (:require [mufl.tree :as tree]
            [mufl.domain :as dom]
            [mufl.bind :as bind]))

;; ════════════════════════════════════════════════════════════════
;; Finding non-ground variables
;; ════════════════════════════════════════════════════════════════

(defn- derived-name?
  "Returns true if the node name looks like a gensym-generated intermediate name.
   These are created by ensure-node for anonymous expression/literal nodes
   and should not be independently labeled during search — they derive their
   ground values from the variables they depend on via constraint propagation."
  [sym]
  (when (symbol? sym)
    (let [s (name sym)]
      (or (.startsWith s "expr")
          (.startsWith s "lit")
          (.startsWith s "call")))))

(defn collect-non-ground
  "Walk the tree under `scope-path` and collect paths of nodes with
   non-singleton finite domains (candidates for labeling).
   Skips: constraint nodes, primitive nodes, link nodes (aliases),
   vector/map container nodes, and derived/intermediate computation nodes."
  [env scope-path]
  (let [scope (tree/cd env scope-path)]
    (when scope
      (->> (tree/children scope)
           (keep (fn [child]
                   (let [d (:domain child)
                         child-name (::tree/name child)]
                     (when (and d
                                (not (dom/singleton? d))
                                (not (dom/void? d))
                                (dom/finite? d)
                                (not (:constraint child))
                                (not (:primitive child))
                                (not (:link child))
                                (not (:vector child))
                                (not (:map child))
                                (not (derived-name? child-name)))
                       (tree/position child)))))
           vec))))

(defn- pick-variable
  "Choose the variable with the smallest domain (fail-first heuristic)."
  [env paths]
  (when (seq paths)
    (apply min-key (fn [p] (dom/size (bind/domain-of env p))) paths)))

;; ════════════════════════════════════════════════════════════════
;; Search
;; ════════════════════════════════════════════════════════════════

(defn- try-assign
  "Try assigning value v to the node at path.
   Sets domain to (single v), then re-propagates all constraints watching it.
   Returns the propagated env, or nil on contradiction."
  [env path v]
  (let [env (tree/put env path :domain (dom/single v))
        ;; Find all constraints watching this node and propagate
        node (tree/cd env path)
        watchers (:watched-by node)]
    (if (seq watchers)
      (bind/propagate env (vec watchers))
      env)))

(defn- expand-fork
  "Expand a fork node (if/cond) into a list of branch envs.
   Each branch is: bind test constraint + bind body.
   Returns seq of [env' body-expr] for successful branches.
   Handles literal boolean tests: true → then only, false → else only."
  [env fork]
  (case (:kind fork)
    :if
    (let [{:keys [test then else]} fork]
      (cond
        ;; Literal boolean tests — no constraint needed
        (true? test)  [[env then]]
        (false? test) [[env else]]

        ;; Normal constraint test
        :else
        (let [branches
              [(try
                 [(bind/bind env test) then]
                 (catch clojure.lang.ExceptionInfo _ nil))
               (try
                 [(bind/bind env (list 'not test)) else]
                 (catch clojure.lang.ExceptionInfo _ nil))]]
          (keep identity branches))))

    :cond
    (keep (fn [[test expr]]
            (try
              (cond
                (= test :else)  [env expr]
                (true? test)    [env expr]
                (false? test)   nil
                :else           [(bind/bind env test) expr])
              (catch clojure.lang.ExceptionInfo _ nil)))
          (:branches fork))))

(defn solve
  "Enumerate all solutions from a propagated tree.
   scope-path: the path to the scope containing the variables.
   Returns a seq of envs, each with all domain nodes ground (singleton)."
  [env scope-path]
  (let [env (tree/root env)
        scope-node (tree/cd env scope-path)]
    ;; First check for fork nodes (if/cond) — these need branch expansion
    (if-let [fork (:fork scope-node)]
      ;; Expand each branch, solve each one
      (let [branches (expand-fork scope-node fork)]
        (mapcat (fn [[branch-env body-expr]]
                  ;; Bind the body in this branch, then solve
                  (let [branch-env' (try
                                      (bind/bind branch-env body-expr)
                                      (catch clojure.lang.ExceptionInfo _ nil))]
                    (when branch-env'
                      ;; Write branch result back to scope and solve
                      (let [;; Check if body binding produced a NEW fork (nested if/cond)
                            ;; vs inheriting the old fork from the scope node
                            new-fork (let [bf (:fork branch-env')]
                                       (when (and bf (not= bf fork))
                                         bf))
                            result-root (tree/root
                                         (tree/upd (tree/root branch-env')
                                                   scope-path
                                                   (fn [n]
                                                     (-> n
                                                         (dissoc :fork)
                                                         (merge (select-keys branch-env'
                                                                             [:domain :link :vector :map]))
                                                         (cond-> new-fork (assoc :fork new-fork))))))]
                        (solve result-root scope-path)))))
                branches))

      ;; No fork — standard variable labeling
      (let [;; Check if scope result is already determined
            scope-dom (:domain scope-node)
            scope-ground? (or (and scope-dom (dom/singleton? scope-dom))
                              (:link scope-node)
                              (:vector scope-node)
                              (:map scope-node))
            ;; Collect vars that the result depends on
            result-deps (when (:link scope-node)
                          ;; Result links to a variable — that variable must be grounded
                          #{(:link scope-node)})
            result-vec-deps (when (:vector scope-node)
                              ;; Result is a vector — all children with links must be grounded
                              (->> (tree/children scope-node)
                                   (keep :link)
                                   set))
            all-result-deps (into (or result-deps #{})
                                  (or result-vec-deps #{}))
            ;; Find non-ground vars that the result depends on
            non-ground (collect-non-ground env scope-path)
            relevant-non-ground (if (and scope-ground? (not (:link scope-node)) (not (:vector scope-node)))
                                  ;; Result is a literal singleton — no vars needed
                                  []
                                  non-ground)
            scope-non-ground? (and scope-dom
                                   (not (:link scope-node))
                                   (not (:vector scope-node))
                                   (dom/finite? scope-dom)
                                   (not (dom/singleton? scope-dom))
                                   (not (dom/void? scope-dom)))]
        (cond
          ;; Result is a ground singleton with no dependencies — done
          (and scope-ground? (empty? relevant-non-ground) (not scope-non-ground?))
          [env]

          ;; Children need labeling
          (seq relevant-non-ground)
          (let [var-path (pick-variable env relevant-non-ground)
                domain (bind/domain-of env var-path)
                values (dom/members domain)]
            (mapcat (fn [v]
                      (when-let [env' (try-assign env var-path v)]
                        (solve env' scope-path)))
                    (sort values)))

          ;; Scope node is non-ground (enum its domain)
          scope-non-ground?
          (map (fn [v]
                 (tree/put env scope-path :domain (dom/single v)))
               (sort (dom/members scope-dom))))))))

;; ════════════════════════════════════════════════════════════════
;; Result extraction
;; ════════════════════════════════════════════════════════════════

(defn extract-value
  "Extract the concrete value from a node at path.
   Handles singletons, vectors (composite nodes), and maps."
  [env path]
  (let [node (tree/cd env path)]
    (when node
      (let [resolved (bind/resolve node)]
        (cond
          ;; Vector node — recursively extract indexed children
          (:vector resolved)
          (let [rpos (tree/position resolved)
                children (sort-by ::tree/name
                                  (filter #(integer? (::tree/name %))
                                          (tree/children resolved)))]
            (mapv (fn [child]
                    (extract-value env (tree/position child)))
                  children))

          ;; Map node — recursively extract key-value children
          (:map resolved)
          (let [children (tree/children resolved)]
            (into {} (map (fn [child]
                            [(::tree/name child)
                             (extract-value env (tree/position child))]))
                  children))

          ;; Singleton domain
          (and (:domain resolved) (dom/singleton? (:domain resolved)))
          (dom/singleton-val (:domain resolved))

          ;; Link — follow it
          (:link resolved)
          (extract-value env (:link resolved))

          :else (:domain resolved))))))
