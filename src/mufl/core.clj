(ns mufl.core
  "Entry point for mufl.

   (query expr) — bind + solve + extract results."
  (:require [mufl.tree :as tree]
            [mufl.domain :as dom]
            [mufl.bind :as bind]
            [mufl.env :as env]
            [mufl.solve :as solve]))

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
     (query (defc positive [x] (> x 0))
            (let [n (one-of 1 2 3)] (positive n) n))
   is equivalent to:
     (query (do (defc positive [x] (> x 0))
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
