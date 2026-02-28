(ns mufl.search
  "Search primitives for mufl: grounded?, split, validate-ground,
   extract-value, extract-bindings.

   These are the building blocks for search strategies.
   They operate on immutable propagated trees and produce new trees
   rather than imperatively advancing search state."
  (:require [mufl.tree :as tree]
            [mufl.domain :as dom]
            [mufl.bind :as bind]))

;; ════════════════════════════════════════════════════════════════
;; grounded?
;; ════════════════════════════════════════════════════════════════

(defn grounded?
  "Is every value-bearing node at or reachable from path fully determined?
   Follows links, recurses into vectors/maps, cycle-safe via seen-set."
  ([env path] (grounded? env path #{}))
  ([env path seen]
   (if (seen path)
     true  ;; cycle — treat as already-ground (not a real value-bearing path)
     (let [node  (bind/resolve-at env path)
           rpath (tree/position node)
           seen' (conj seen rpath)]
       (cond
         ;; Fork → definitely not ground (unresolved branching)
         (:fork node) false

         ;; Vector — groundedness is determined by children, not any type-domain.
         ;; (A vector node can carry a :domain type-constraint while being concretely
         ;; ground if all its children are singleton.)
         (:vector node)
         (every? #(grounded? env (tree/position %) seen')
                 (filter #(integer? (::tree/name %))
                         (tree/children node)))

         ;; Map — groundedness determined by children, not type-domain
         (:map node)
         (every? #(grounded? env (tree/position %) seen')
                 (tree/children node))

         ;; Void domain → failed, not ground
         (and (:domain node) (dom/void? (:domain node))) false

         ;; Singleton domain → ground
         (and (:domain node) (dom/singleton? (:domain node))) true

         ;; Any other domain (range, finite, type, any …) → not ground
         (:domain node) false

         ;; No value-bearing attribute — structural/vacuous node
         :else true)))))

;; ════════════════════════════════════════════════════════════════
;; Variable collection — which nodes are candidates for splitting
;; ════════════════════════════════════════════════════════════════

(defn- collect-splittable
  "Walk direct children of scope-path and collect paths of nodes with
   non-singleton steppable domains (candidates for splitting).
   Skips: constraints, primitives, links, vectors, maps, derived, def-bindings."
  [env scope-path]
  (let [scope (tree/cd env scope-path)]
    (when scope
      (->> (tree/children scope)
           (keep (fn [child]
                   (let [d (:domain child)]
                     (when (and d
                                (not (dom/singleton? d))
                                (not (dom/void? d))
                                (dom/steppable? d)
                                (not (:constraint child))
                                (not (:primitive child))
                                (not (:link child))
                                (not (:vector child))
                                (not (:map child))
                                (not (:derived child))
                                (not (:def-binding child)))
                       (tree/position child)))))
           vec))))

(defn- collect-splittable-deps
  "Follow links and recurse into vectors/maps to find all non-ground variables
   the result at scope-path depends on."
  [env scope-path]
  (let [root      (tree/root env)
        scope-node (tree/cd root scope-path)]
    (when scope-node
      (letfn [(collect [node seen]
                (let [pos (tree/position node)]
                  (if (seen pos)
                    #{}
                    (let [seen     (conj seen pos)
                          resolved (bind/resolve node)
                          rpos     (tree/position resolved)]
                      (cond
                        ;; Vector/map container — recurse into children
                        (or (:vector resolved) (:map resolved))
                        (reduce (fn [deps child]
                                  (into deps (collect child seen)))
                                #{}
                                (tree/children resolved))

                        ;; Non-ground steppable domain — this is a dependency
                        (let [d (:domain resolved)]
                          (and d
                               (not (dom/singleton? d))
                               (not (dom/void? d))
                               (dom/steppable? d)
                               (not (:derived resolved))))
                        #{rpos}

                        ;; Link — follow it explicitly
                        (:link node)
                        (let [target (tree/cd root (:link node))]
                          (if target (collect target seen) #{}))

                        :else #{})))))]
        (vec (collect scope-node #{}))))))

(defn- pick-split-variable
  "Choose a variable to split: prefer the one with the smallest domain
   (fail-first heuristic). Infinite domains get a sentinel size."
  [env scope-path]
  (let [direct     (collect-splittable env scope-path)
        deps       (collect-splittable-deps env scope-path)
        candidates (vec (distinct (concat direct deps)))]
    (when (seq candidates)
      (apply min-key
             (fn [p]
               (or (dom/size (bind/domain-of env p)) Long/MAX_VALUE))
             candidates))))

;; ════════════════════════════════════════════════════════════════
;; try-narrow — generalized try-assign
;; ════════════════════════════════════════════════════════════════

(defn- try-narrow
  "Narrow the domain of the node at path to new-dom, then propagate watchers.
   Returns the updated (rooted) env, or nil on contradiction."
  [env path new-dom]
  (let [env     (tree/put env path :domain new-dom)
        node    (tree/cd env path)
        watchers (:watched-by node)]
    (if (seq watchers)
      (bind/propagate env (vec watchers))
      env)))

;; ════════════════════════════════════════════════════════════════
;; Fork expansion helpers (mirrors expand-fork in solve.clj)
;; ════════════════════════════════════════════════════════════════

(defn- expand-fork
  "Expand a fork node (if/cond) into viable [branch-env body-expr] pairs.
   env is the scope node (navigated). Returns a seq of pairs."
  [env fork]
  (case (:kind fork)
    :if
    (let [{:keys [test then else]} fork]
      (cond
        (true? test)  [[env then]]
        (false? test) [[env else]]
        :else
        (keep identity
              [(try [(bind/bind env test) then]
                    (catch clojure.lang.ExceptionInfo _ nil))
               (try [(bind/bind env (list 'not test)) else]
                    (catch clojure.lang.ExceptionInfo _ nil))])))

    :cond
    (keep (fn [[test expr]]
            (try
              (cond
                (= test :else) [env expr]
                (true? test)   [env expr]
                (false? test)  nil
                :else          [(bind/bind env test) expr])
              (catch clojure.lang.ExceptionInfo _ nil)))
          (:branches fork))

    ;; :trees kind is handled directly in split-fork, not here
    nil))

(defn- apply-branch
  "Apply one fork branch: bind the body, then merge the result back into the
   scope node at scope-path. Returns a new root tree, or nil on contradiction."
  [scope-path fork [branch-env body-expr]]
  (try
    (let [branch-env' (bind/bind branch-env body-expr)
          ;; If the body produced a NEW (nested) fork, preserve it
          new-fork    (let [bf (:fork branch-env')]
                        (when (and bf (not= bf fork)) bf))
          result-root (tree/root
                       (tree/upd (tree/root branch-env')
                                 scope-path
                                 (fn [n]
                                   (-> n
                                       (dissoc :fork)
                                       (merge (select-keys branch-env'
                                                           [:domain :link :vector :map]))
                                       (cond-> new-fork (assoc :fork new-fork))))))]
      result-root)
    (catch clojure.lang.ExceptionInfo _ nil)))

;; ════════════════════════════════════════════════════════════════
;; split
;; ════════════════════════════════════════════════════════════════

(declare split)

(defn- split-fork
  "Split a fork node into [left-tree right-tree] or nil."
  [env scope-path fork]
  (if (= :trees (:kind fork))
    ;; Pre-expanded tree list — peel off one tree at a time
    (let [trees (:trees fork)]
      (case (count trees)
        0 nil
        1 (split (first trees) scope-path)
        (let [[t1 & rest-trees] trees]
          (if (= 1 (count rest-trees))
            [t1 (first rest-trees)]
            [t1 (tree/put env scope-path
                          :fork {:kind :trees :trees (vec rest-trees)})]))))

    ;; Syntactic fork (:if, :cond) — expand all viable branches
    (let [scope-node (tree/cd env scope-path)
          branches   (expand-fork scope-node fork)
          trees      (->> branches
                          (map #(apply-branch scope-path fork %))
                          (filterv some?))]
      (case (count trees)
        0 nil
        1 (split (first trees) scope-path)  ;; only one branch viable — recurse
        2 trees
        ;; n>2: first tree vs rest wrapped in a :trees fork
        (let [[t1 & rest-trees] trees]
          [t1 (tree/put env scope-path
                        :fork {:kind :trees :trees (vec rest-trees)})])))))

(defn split
  "Bisect tree at scope-path into [left-tree right-tree].
   Picks the non-ground variable with the smallest domain (fail-first),
   calls dom/split on its domain, and returns two propagated trees.
   For fork nodes, expands branches instead.
   Returns nil if already ground or no split is possible."
  [env scope-path]
  (let [env        (tree/root env)
        scope-node (tree/cd env scope-path)]
    (when (and scope-node (not (grounded? env scope-path)))
      (if-let [fork (:fork scope-node)]
        ;; Fork node: split along branches
        (split-fork env scope-path fork)

        ;; Standard: find a variable to split on
        (let [var-path      (pick-split-variable env scope-path)
              ;; Also check if scope itself has a splittable domain
              scope-dom     (:domain scope-node)
              scope-split?  (and scope-dom
                                 (not (dom/singleton? scope-dom))
                                 (not (dom/void? scope-dom))
                                 (dom/steppable? scope-dom)
                                 (not (:link scope-node))
                                 (not (:vector scope-node))
                                 (not (:map scope-node)))]
          (cond
            ;; Found a variable — bisect its domain
            var-path
            (let [domain (bind/domain-of env var-path)]
              (when-let [[left-dom right-dom] (dom/split domain)]
                [(try-narrow env var-path left-dom)
                 (try-narrow env var-path right-dom)]))

            ;; Scope node itself has a splittable domain
            scope-split?
            (when-let [[left-dom right-dom] (dom/split scope-dom)]
              [(try-narrow env scope-path left-dom)
               (try-narrow env scope-path right-dom)])))))))

;; ════════════════════════════════════════════════════════════════
;; validate-ground — final consistency check after all vars are ground
;; ════════════════════════════════════════════════════════════════

(defn- collect-all-constraints
  "Recursively walk the entire tree rooted at `node` and return all
   constraint-node paths. Constraints are nodes with a :constraint key."
  [node acc]
  (let [acc' (if (:constraint node)
               (conj acc (tree/position node))
               acc)]
    (reduce (fn [a child] (collect-all-constraints child a))
            acc'
            (tree/children node))))

(defn validate-ground
  "After grounded? returns true, re-run ALL constraints in the tree to catch
   stale propagation (e.g. backward-only narrowing that set a var without
   re-checking its forward direction).
   Returns the validated env (rooted), or nil if any constraint is violated."
  [env scope-path]
  (let [root    (tree/root env)
        scope   (tree/cd root scope-path)
        c-paths (when scope (collect-all-constraints scope []))]
    (if (seq c-paths)
      (bind/propagate root c-paths)
      root)))

;; ════════════════════════════════════════════════════════════════
;; Value extraction
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
          (let [children (sort-by ::tree/name
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

(defn- user-binding?
  "True if a workspace child represents a user-defined value binding
   (not an internal/derived/function/constraint/domain definition node)."
  [child]
  (let [nm (::tree/name child)]
    (and (symbol? nm)
         (not (:derived child))
         (not (:primitive child))
         (not (:mufl-fn child))
         (not (:defn-constructor child))
         (not (:bind child))
         (not (:def-binding child))
         ;; Must hold a value: domain, link, vector, or map
         (or (:domain child)
             (:link child)
             (:vector child)
             (:map child)))))

(defn extract-bindings
  "Extract all user-defined bindings from the workspace scope as a map.
   Returns {symbol value} for each user variable in the scope."
  [env scope-path]
  (let [scope (tree/cd env scope-path)]
    (when scope
      (let [children (tree/children scope)]
        (into {}
              (keep (fn [child]
                      (when (user-binding? child)
                        [(::tree/name child)
                         (extract-value env (tree/position child))])))
              children)))))
