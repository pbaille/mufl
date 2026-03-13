(ns mufl.show-test
  (:require [clojure.test :refer [deftest testing is]]
            [mufl.show :as show]
            [mufl.tree :as tree]
            [mufl.domain :as dom]
            [mufl.bind :as bind]
            [mufl.env :as env]))

(defn- setup
  "Bind an expression in a workspace, return [env ws-path]."
  [expr]
  (let [base (env/base-env)
        ws (gensym "ws")
        ws-path [ws]
        env (-> base
                (tree/ensure-path ws-path)
                (tree/upd ws-path #(bind/bind % expr)))]
    [env ws-path]))

;; ════════════════════════════════════════════════════════════════
;; 1. Ground values
;; ════════════════════════════════════════════════════════════════

(deftest ground-integer
  (let [[env path] (setup 42)]
    (is (= 42 (show/show env path)))))

(deftest ground-string
  (let [[env path] (setup "hello")]
    (is (= "hello" (show/show env path)))))

(deftest ground-keyword
  (let [[env path] (setup :foo)]
    (is (= :foo (show/show env path)))))

(deftest ground-boolean
  (let [[env path] (setup true)]
    (is (= true (show/show env path)))))

(deftest ground-nil
  (let [[env path] (setup nil)]
    (is (= nil (show/show env path)))))

(deftest ground-vector
  (let [[env path] (setup [1 2 3])]
    (is (= [1 2 3] (show/show env path)))))

(deftest ground-map
  (let [[env path] (setup {:a 1 :b 2})]
    (is (= {:a 1 :b 2} (show/show env path)))))

(deftest ground-nested
  (let [[env path] (setup [1 {:a 2} [3 4]])]
    (is (= [1 {:a 2} [3 4]] (show/show env path)))))

;; ════════════════════════════════════════════════════════════════
;; 2. Non-ground scalar domains → constructors
;; ════════════════════════════════════════════════════════════════

(deftest non-ground-integer
  (let [[env path] (setup '(integer))]
    (is (= '(integer) (show/show env path)))))

(deftest non-ground-string
  (let [[env path] (setup '(string))]
    (is (= '(string) (show/show env path)))))

(deftest non-ground-between
  (let [[env path] (setup '(between 1 10))]
    (is (= '(between 1 10) (show/show env path)))))

(deftest non-ground-one-of
  (let [[env path] (setup '(one-of 1 2 3))]
    (is (= '(one-of 1 2 3) (show/show env path)))))

(deftest non-ground-free
  (let [[env path] (setup '(free))]
    (is (= '(free) (show/show env path)))))

;; ════════════════════════════════════════════════════════════════
;; 3. Bare type-in-position (structures with non-ground children)
;; ════════════════════════════════════════════════════════════════

(deftest vector-with-types
  (let [[env path] (setup '[(integer) (string)])]
    (is (= '[(integer) (string)] (show/show env path)))))

(deftest map-with-types
  (let [[env path] (setup '{:name (string) :age (integer)})]
    (let [result (show/show env path)]
      (is (map? result))
      (is (= '(string) (:name result)))
      (is (= '(integer) (:age result))))))

(deftest mixed-ground-and-non-ground
  (let [[env path] (setup '[42 (string) :foo])]
    (is (= '[42 (string) :foo] (show/show env path)))))

(deftest vector-with-between
  (let [[env path] (setup '[(between 1 10) (between 20 30)])]
    (is (= '[(between 1 10) (between 20 30)] (show/show env path)))))

;; ════════════════════════════════════════════════════════════════
;; 4. Sharing (same variable at multiple output positions)
;; ════════════════════════════════════════════════════════════════

(deftest sharing-in-vector
  (let [[env path] (setup '(fresh [(integer x)] [x x]))]
    (let [result (show/show env path)]
      ;; Must be a let expression
      (is (seq? result) "sharing requires let wrapper")
      (is (= 'let (first result)) "wrapper is let")
      ;; Body is a vector with two identical symbols
      (let [body (last result)]
        (is (vector? body))
        (is (= 2 (count body)))
        (is (= (first body) (second body)) "both positions are the same variable")
        (is (symbol? (first body)))))))

(deftest sharing-preserves-user-name
  (let [[env path] (setup '(fresh [(integer x)] [x x]))]
    (let [result (show/show env path)
          bindings (second result)
          body (last result)]
      ;; The variable name should be 'x' (preserved from user)
      (is (= 'x (first bindings)))
      (is (= 'x (first body)))
      (is (= 'x (second body))))))

(deftest sharing-with-domain
  (let [[env path] (setup '(let [x (between 1 5)] [x x]))]
    (let [result (show/show env path)]
      (is (seq? result))
      (is (= 'let (first result)))
      (let [bindings (second result)
            body (last result)]
        ;; RHS should be (between 1 5)
        (is (= '(between 1 5) (second bindings)))
        ;; Body should use the variable twice
        (is (= (first body) (second body)))))))

;; ════════════════════════════════════════════════════════════════
;; 5. Residual constraints
;; ════════════════════════════════════════════════════════════════

(deftest constraint-lt-between-vars
  (let [[env path] (setup '(fresh [(integer x) (integer y)]
                             (< x y)
                             [x y]))]
    (let [result (show/show env path)]
      (is (seq? result))
      (is (= 'let (first result)))
      ;; Should contain a constraint expression in the body
      (let [forms (drop 2 result)   ;; skip 'let and bindings
            constraints (butlast forms)
            body (last forms)]
        (is (vector? body) "last form is the vector body")
        (is (pos? (count constraints)) "has at least one constraint")))))

(deftest constraint-preserves-var-names
  (let [[env path] (setup '(fresh [(integer x) (integer y)]
                             (< x y)
                             [x y]))]
    (let [result (show/show env path)
          bindings (second result)
          ;; Extract declared variable names
          declared-names (set (take-nth 2 bindings))]
      (is (contains? declared-names 'x))
      (is (contains? declared-names 'y)))))

;; ════════════════════════════════════════════════════════════════
;; 6. Half-bounded ranges (force variable introduction)
;; ════════════════════════════════════════════════════════════════

(deftest half-bounded-range-lower
  ;; (>= x 0) on an integer → range(0, nil) → needs var + constraint
  (let [[env path] (setup '(fresh [(integer x)] (>= x 0) [x]))]
    (let [result (show/show env path)]
      (is (seq? result) "half-bounded range needs let wrapper")
      (is (= 'let (first result))))))

;; ════════════════════════════════════════════════════════════════
;; 7. show-domain (unit tests for domain→constructor mapping)
;; ════════════════════════════════════════════════════════════════

(deftest show-domain-any
  (is (= '(free) (show/show-domain dom/any))))

(deftest show-domain-integer-type
  (is (= '(integer) (show/show-domain dom/integer-dom))))

(deftest show-domain-string-type
  (is (= '(string) (show/show-domain dom/string-dom))))

(deftest show-domain-range-unbounded
  (is (= '(integer) (show/show-domain (dom/range-dom nil nil)))))

(deftest show-domain-range-bounded
  (is (= '(between 0 100) (show/show-domain (dom/range-dom 0 100)))))

(deftest show-domain-range-half-bounded
  (is (nil? (show/show-domain (dom/range-dom 0 nil)))
      "half-bounded range returns nil (needs variable)")
  (is (nil? (show/show-domain (dom/range-dom nil 100)))))

(deftest show-domain-finite
  (is (= '(one-of 1 2 3) (show/show-domain (dom/finite #{1 2 3})))))

(deftest show-domain-singleton
  (is (= 42 (show/show-domain (dom/single 42)))))

(deftest show-domain-vector-of
  (is (= '(vector-of (integer)) (show/show-domain (dom/vector-of-dom dom/integer-dom)))))

(deftest show-domain-tuple
  (is (= '[(integer) (string)] (show/show-domain (dom/tuple-dom [dom/integer-dom dom/string-dom])))))

(deftest show-domain-map-of
  (is (= '(map-of (keyword) (integer)) (show/show-domain (dom/map-of-dom dom/keyword-dom dom/integer-dom)))))
