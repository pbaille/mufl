(ns mufl.core
  "Entry point for mufl.

   (query expr) — bind + solve + extract results.
   (query1 expr) — lazy: produce one solution at a time.
   (query-lazy expr) — lazy-seq of all solutions.
   (simplify expr) — bind + propagate, return {:tree env :scope path}."
  (:require [mufl.tree :as tree]
            [mufl.domain :as dom]
            [mufl.bind :as bind]
            [mufl.env :as env]
            [mufl.search :as search]))

;; ════════════════════════════════════════════════════════════════
;; DFS search engine (built on search/grounded? + search/split)
;; ════════════════════════════════════════════════════════════════

(defn- search-dfs
  "DFS over the tree: yield all ground leaves, splitting at every branch.
   extract-fn is called on each validated ground tree: (extract-fn env scope-path)."
  [env scope-path extract-fn]
  (cond
    (nil? env)                             []
    (search/grounded? env scope-path)
    ;; Re-run all constraints to catch stale backward-only propagation.
    ;; validate-ground returns nil if any constraint is violated.
    (if-let [valid (search/validate-ground env scope-path)]
      [(extract-fn valid scope-path)]
      [])
    :else
    (if-let [[left right] (search/split env scope-path)]
      (concat (search-dfs left  scope-path extract-fn)
              (search-dfs right scope-path extract-fn))
      [])))   ;; split returned nil → dead end (void or unsplittable)

(defn query*
  "Functional query: bind the expression, solve, extract results.
   Returns a seq of result values."
  [expr]
  (let [base    (env/base-env)
        ws-name (gensym "ws")
        ws-path [ws-name]
        env     (tree/ensure-path base ws-path)
        env     (tree/upd env ws-path #(bind/bind % expr))
        ws-node (tree/cd env ws-path)
        return-domain (:domain ws-node)]
    (cond
      ;; Already ground
      (and return-domain (dom/singleton? return-domain))
      [(dom/singleton-val return-domain)]

      ;; Void — no solutions
      (and return-domain (dom/void? return-domain))
      []

      (let [resolved-dom (:domain (bind/resolve ws-node))]
        (and resolved-dom (dom/void? resolved-dom)))
      []

      ;; DFS search
      :else
      (vec (search-dfs env ws-path search/extract-value)))))

(defmacro query
  "Query macro: bind the expression, solve, extract results.
   Accepts multiple body forms (implicit do):
     (query (defn positive [x] (> x 0))
            (let [n (one-of 1 2 3)] (positive n) n))
   is equivalent to:
     (query (do (defn positive [x] (> x 0))
                (let [n (one-of 1 2 3)] (positive n) n)))"
  [& body]
  (let [expr (if (= 1 (count body))
               (first body)
               (cons 'do body))]
    `(query* '~expr)))

(defn query+*
  "Functional query+: bind the expression, solve, extract full environments.
   Returns a vector of maps, where each map contains all user-defined
   bindings from the workspace scope: [{x 1, y 5} {x 2, y 4}]."
  [expr]
  (let [base    (env/base-env)
        ws-name (gensym "ws")
        ws-path [ws-name]
        env     (tree/ensure-path base ws-path)
        env     (tree/upd env ws-path #(bind/bind % expr))
        ws-node (tree/cd env ws-path)
        return-domain (:domain ws-node)]
    (cond
      ;; Void — no solutions
      (and return-domain (dom/void? return-domain))
      []

      (let [resolved-dom (:domain (bind/resolve ws-node))]
        (and resolved-dom (dom/void? resolved-dom)))
      []

      :else
      (vec (search-dfs env ws-path search/extract-bindings)))))

(defmacro query+
  "Query+ macro: bind the expression, solve, return full environments.
   Each solution is a map of all user-defined bindings.
   Accepts multiple body forms (implicit do), like query.
     (query+ (let [x (one-of 1 2 3)
                   y (one-of 1 2 3)]
               (and (< x y) [x y])))
     ;=> [{x 1, y 2} {x 1, y 3} {x 2, y 3}]"
  [& body]
  (let [expr (if (= 1 (count body))
               (first body)
               (cons 'do body))]
    `(query+* '~expr)))

;; ════════════════════════════════════════════════════════════════
;; Lazy query: query1 (one solution at a time) + query-lazy (lazy seq)
;;
;; Search state is a plain map {:remaining [{:tree t :scope s} ...]}
;; — a stack of unexplored trees. No SearchState record needed.
;; ════════════════════════════════════════════════════════════════

(defn- setup-and-bind
  "Create workspace, bind expression, return [env ws-path] or nil on void/contradiction."
  [expr]
  (try
    (let [base    (env/base-env)
          ws-name (gensym "ws")
          ws-path [ws-name]
          env     (tree/ensure-path base ws-path)
          env     (tree/upd env ws-path #(bind/bind % expr))
          ws-node (tree/cd env ws-path)
          return-domain (:domain ws-node)]
      (cond
        (and return-domain (dom/void? return-domain)) nil
        (let [resolved-dom (:domain (bind/resolve ws-node))]
          (and resolved-dom (dom/void? resolved-dom)))          nil
        :else [env ws-path]))
    (catch clojure.lang.ExceptionInfo _ nil)))

(defn- dfs-one
  "Find one ground solution via DFS, pushing unexplored right-branches onto
   `remaining` (a stack of {:tree env :scope path} maps).
   Returns [value {:remaining [...]}] or nil when exhausted."
  [env scope-path remaining]
  (cond
    ;; Dead tree (contradiction from narrowing) — try next on stack
    (nil? env)
    (when (seq remaining)
      (let [{:keys [tree scope]} (first remaining)]
        (dfs-one tree scope (subvec remaining 1))))

    ;; Ground — validate (re-run constraints) then return, or skip if contradictory
    (search/grounded? env scope-path)
    (if-let [valid (search/validate-ground env scope-path)]
      [(search/extract-value valid scope-path) {:remaining remaining}]
      ;; Stale propagation detected contradiction — skip to next on stack
      (when (seq remaining)
        (let [{:keys [tree scope]} (first remaining)]
          (dfs-one tree scope (subvec remaining 1)))))

    ;; Split and explore left; push right for later
    :else
    (when-let [[left right] (search/split env scope-path)]
      (dfs-one left scope-path
               (into [{:tree right :scope scope-path}] remaining)))))

(defn query1*
  "Functional query1: produce one solution at a time.
   - If `expr-or-state` is a map {:remaining [...]}: continue from saved state.
   - If it's an expression: fresh search.
   Returns [value state] or nil when exhausted."
  [expr-or-state]
  (if (map? expr-or-state)
    ;; Continue: pop next unexplored tree from the stack
    (let [{:keys [remaining]} expr-or-state]
      (when (seq remaining)
        (let [{:keys [tree scope]} (first remaining)]
          (dfs-one tree scope (subvec remaining 1)))))
    ;; Fresh query
    (when-let [[env ws-path] (setup-and-bind expr-or-state)]
      (dfs-one env ws-path []))))

(defmacro query1
  "Query1 macro: produce one solution.
   Returns [value state] or nil.
   Pass the state map to query1* to get the next solution.
     (let [[v state] (query1 (let [x (one-of 1 2 3)] x))]
       ;; v is 1, (query1* state) gets the next solution
       )"
  [& body]
  (let [expr (if (= 1 (count body))
               (first body)
               (cons 'do body))]
    `(query1* '~expr)))

(defn query-lazy*
  "Functional query-lazy: returns a lazy seq of all solutions.
   For finite domains, equivalent to (query* expr) but lazy.
   For infinite domains, produces solutions on demand."
  [expr]
  (letfn [(from [result]
            (when result
              (let [[v state] result]
                (lazy-seq (cons v (from (query1* state)))))))]
    (from (query1* expr))))

(defmacro query-lazy
  "Query-lazy macro: returns a lazy seq of all solutions.
   Accepts multiple body forms (implicit do), like query.
     (take 5 (query-lazy (let [x (between 1 100)] x)))
     ;=> (1 2 3 4 5)"
  [& body]
  (let [expr (if (= 1 (count body))
               (first body)
               (cons 'do body))]
    `(query-lazy* '~expr)))

;; ════════════════════════════════════════════════════════════════
;; simplify — bind + propagate without solving
;; ════════════════════════════════════════════════════════════════

(defn simplify*
  "Bind expression and propagate constraints to fixpoint — no search.
   Returns {:tree env :scope ws-path} where env may be ground, non-ground,
   or void. Returns nil if binding itself causes a contradiction."
  [expr]
  (try
    (let [base    (env/base-env)
          ws-name (gensym "ws")
          ws-path [ws-name]
          env     (tree/ensure-path base ws-path)
          env     (tree/upd env ws-path #(bind/bind % expr))]
      {:tree env :scope ws-path})
    (catch clojure.lang.ExceptionInfo _
      nil)))

(defmacro simplify
  "Bind expression and propagate to fixpoint. No search.
   Returns {:tree env :scope path} or nil on contradiction.
   Accepts multiple body forms (implicit do), like query."
  [& body]
  (let [expr (if (= 1 (count body))
               (first body)
               (cons 'do body))]
    `(simplify* '~expr)))
