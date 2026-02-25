(ns mufl.core
  "Entry point for mufl.

   (query expr) — bind + solve + extract results.
   (query1 expr) — lazy: produce one solution at a time.
   (query-lazy expr) — lazy-seq of all solutions."
  (:require [mufl.tree :as tree]
            [mufl.domain :as dom]
            [mufl.bind :as bind]
            [mufl.env :as env]
            [mufl.solve :as solve]))

;; Record for search state (enables pause/resume)
(defrecord SearchState [scope-path choice-points])

(defn query*
  "Functional query: bind the expression, solve, extract results.
   Returns a seq of result values."
  [expr]
  (let [base (env/base-env)
        ;; Create workspace scope
        ws-name (gensym "ws")
        ws-path [ws-name]
        env (tree/ensure-path base ws-path)
        ;; Bind the expression inside the workspace
        env (tree/upd env ws-path #(bind/bind % expr))
        ;; Get the return expression info
        ;; The bound node at ws-path should have a :domain
        ws-node (tree/cd env ws-path)
        return-domain (:domain ws-node)]
    (cond
      ;; If the result is already ground (singleton), just return it
      (and return-domain (dom/singleton? return-domain))
      [(dom/singleton-val return-domain)]

      ;; If void, no solutions (check both own domain and resolved domain for links)
      (and return-domain (dom/void? return-domain))
      []

      (let [resolved-dom (:domain (bind/resolve ws-node))]
        (and resolved-dom (dom/void? resolved-dom)))
      []

      ;; If result is a non-ground domain on the ws node itself (from if/or/cond)
      ;; and no non-ground variables underneath, just enumerate the domain
      (and return-domain (dom/finite? return-domain) (not (dom/singleton? return-domain))
           (empty? (solve/collect-non-ground env ws-path)))
      (vec (sort (dom/members return-domain)))

      ;; Otherwise, solve for all solutions
      :else
      (let [solutions (solve/solve env ws-path)]
        (mapv (fn [solved-env]
                (solve/extract-value solved-env ws-path))
              solutions)))))

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
  (let [base (env/base-env)
        ws-name (gensym "ws")
        ws-path [ws-name]
        env (tree/ensure-path base ws-path)
        env (tree/upd env ws-path #(bind/bind % expr))
        ws-node (tree/cd env ws-path)
        return-domain (:domain ws-node)]
    (cond
      ;; Void — no solutions
      (and return-domain (dom/void? return-domain))
      []

      (let [resolved-dom (:domain (bind/resolve ws-node))]
        (and resolved-dom (dom/void? resolved-dom)))
      []

      ;; Solve and extract full bindings from each solution
      :else
      (let [solutions (solve/solve env ws-path)]
        (mapv (fn [solved-env]
                (solve/extract-bindings solved-env ws-path))
              solutions)))))

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
;; ════════════════════════════════════════════════════════════════

(defn- setup-and-bind
  "Create workspace, bind expression, return [env ws-path] or nil on void/contradiction."
  [expr]
  (try
    (let [base (env/base-env)
          ws-name (gensym "ws")
          ws-path [ws-name]
          env (tree/ensure-path base ws-path)
          env (tree/upd env ws-path #(bind/bind % expr))
          ws-node (tree/cd env ws-path)
          return-domain (:domain ws-node)]
      (cond
        ;; Void — no solutions
        (and return-domain (dom/void? return-domain))
        nil

        (let [resolved-dom (:domain (bind/resolve ws-node))]
          (and resolved-dom (dom/void? resolved-dom)))
        nil

        :else [env ws-path]))
    (catch clojure.lang.ExceptionInfo _
      ;; Bind-time contradiction — no solutions
      nil)))

(defn query1*
  "Functional query1: produce one solution at a time.
   - If `expr-or-state` is a SearchState: backtrack to get the next solution.
   - If it's an expression: create workspace, solve for first solution.
   Returns [value SearchState] or nil when exhausted."
  [expr-or-state]
  (if (instance? SearchState expr-or-state)
    ;; Continue from saved state: backtrack to get next solution
    (let [{:keys [scope-path choice-points]} expr-or-state]
      (when-let [[env cps] (solve/backtrack-step scope-path choice-points)]
        [(solve/extract-value env scope-path)
         (->SearchState scope-path cps)]))
    ;; Fresh query: setup env, solve for first solution
    (when-let [[env ws-path] (setup-and-bind expr-or-state)]
      (when-let [[solved-env cps] (solve/solve-step env ws-path [])]
        [(solve/extract-value solved-env ws-path)
         (->SearchState ws-path cps)]))))

(defmacro query1
  "Query1 macro: produce one solution.
   Returns [value search-state] or nil.
   Pass the search-state to query1* to get the next solution.
     (let [[v state] (query1 (let [x (one-of 1 2 3)] x))]
       ;; v is 1, (query1* state) gets the next solution
       )"
  [& body]
  (let [expr (if (= 1 (count body))
               (first body)
               (cons 'do body))]
    `(query1* '~expr)))

(defn- query-lazy-continue
  "Continue producing solutions from a SearchState."
  [state]
  (when-let [[v state'] (query1* state)]
    (lazy-seq (cons v (query-lazy-continue state')))))

(defn query-lazy*
  "Functional query-lazy: returns a lazy seq of all solutions.
   For finite domains, equivalent to (query* expr) but lazy.
   For infinite domains, produces solutions on demand."
  [expr]
  (when-let [[v state] (query1* expr)]
    (lazy-seq (cons v (query-lazy-continue state)))))

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
