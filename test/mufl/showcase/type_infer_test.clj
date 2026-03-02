(ns mufl.showcase.type-infer-test
  "Tests for the Type Inference Engine showcase.

   Covers all 8 required cases:
   1. Integer literal
   2. Boolean literal
   3. Variable lookup
   4. Addition (with variable)
   5. Lambda (param type narrowed by body constraints)
   6. Application (lambda applied to int-lit)
   7. Conditional (branches match)
   8. Type error (applying non-arrow type)"
  (:require [clojure.test :refer [deftest testing is]]
            [mufl.showcase.type-infer :as ti]))

;; ── Helpers ──────────────────────────────────────────────────────

(def empty-env {})

;; ── 1. Literals ──────────────────────────────────────────────────

(deftest integer-literal
  (testing "integer literal infers as :int"
    (is (= {:kind :int}
           (ti/infer empty-env {:kind :int-lit :val 42})))))

(deftest boolean-literal
  (testing "boolean literal infers as :bool"
    (is (= {:kind :bool}
           (ti/infer empty-env {:kind :bool-lit :val true})))))

;; ── 2. Variable lookup ───────────────────────────────────────────

(deftest variable-lookup
  (testing "variable lookup returns type from env"
    (is (= {:kind :int}
           (ti/infer {:x {:kind :int}}
                     {:kind :var :name :x}))))

  (testing "variable lookup for bool type"
    (is (= {:kind :bool}
           (ti/infer {:y {:kind :bool}}
                     {:kind :var :name :y})))))

;; ── 3. Addition ──────────────────────────────────────────────────

(deftest addition-type
  (testing "add with int variable and int literal → :int"
    (is (= {:kind :int}
           (ti/infer {:x {:kind :int}}
                     {:kind :add
                      :left  {:kind :var :name :x}
                      :right {:kind :int-lit :val 1}}))))

  (testing "add of two int literals → :int"
    (is (= {:kind :int}
           (ti/infer empty-env
                     {:kind :add
                      :left  {:kind :int-lit :val 3}
                      :right {:kind :int-lit :val 4}})))))

;; ── 4. Lambda — constraint propagation ───────────────────────────

(deftest lambda-type
  (testing "λx. (x + 1) — body constrains param to :int"
    ;; t-param starts uncertain (int or bool).
    ;; The add rule constrains it to :int via propagation.
    (is (= {:kind :arrow :from {:kind :int} :to {:kind :int}}
           (ti/infer empty-env
                     {:kind :lam
                      :param :x
                      :body  {:kind :add
                              :left  {:kind :var :name :x}
                              :right {:kind :int-lit :val 1}}}))))

  (testing "λx. true — unused param has multiple valid typings"
    ;; Both int and bool are valid for an unused parameter.
    (let [types (ti/infer-all empty-env {:kind :lam
                                         :param :x
                                         :body  {:kind :bool-lit :val true}})]
      (is (= 2 (count types)))
      (is (every? #(= (:kind %) :arrow) types))
      (is (every? #(= {:kind :bool} (:to %)) types))))

  (testing "λx. λy. (x + y) — nested lambdas, both params :int"
    (is (= {:kind :arrow
            :from {:kind :int}
            :to   {:kind :arrow :from {:kind :int} :to {:kind :int}}}
           (ti/infer empty-env
                     {:kind :lam
                      :param :x
                      :body  {:kind :lam
                              :param :y
                              :body  {:kind :add
                                      :left  {:kind :var :name :x}
                                      :right {:kind :var :name :y}}}})))))

;; ── 5. Application ───────────────────────────────────────────────

(deftest application-type
  (testing "(λx. x + 1) 42 → :int"
    (is (= {:kind :int}
           (ti/infer empty-env
                     {:kind :app
                      :fn  {:kind :lam
                            :param :x
                            :body  {:kind :add
                                    :left  {:kind :var :name :x}
                                    :right {:kind :int-lit :val 1}}}
                      :arg {:kind :int-lit :val 42}}))))

  (testing "applying env-bound arrow type"
    ;; x : int → bool, so (x 0) : bool
    (is (= {:kind :bool}
           (ti/infer {:x {:kind :arrow :from {:kind :int} :to {:kind :bool}}}
                     {:kind :app
                      :fn  {:kind :var :name :x}
                      :arg {:kind :int-lit :val 0}})))))

;; ── 6. Conditional ───────────────────────────────────────────────

(deftest conditional-type
  (testing "if true then 1 else 2 → :int"
    (is (= {:kind :int}
           (ti/infer empty-env
                     {:kind :iff
                      :cond {:kind :bool-lit :val true}
                      :then {:kind :int-lit :val 1}
                      :else {:kind :int-lit :val 2}}))))

  (testing "if y then (x+1) else 0 where y:bool, x:int → :int"
    ;; Uses :y for bool condition, :x for int operand (both in showcase var set)
    (is (= {:kind :int}
           (ti/infer {:y {:kind :bool} :x {:kind :int}}
                     {:kind :iff
                      :cond {:kind :var :name :y}
                      :then {:kind :add
                             :left  {:kind :var :name :x}
                             :right {:kind :int-lit :val 1}}
                      :else {:kind :int-lit :val 0}})))))

;; ── 7. Type errors ───────────────────────────────────────────────

(deftest type-error-apply-non-function
  (testing "applying a non-arrow type throws ExceptionInfo"
    ;; (42) 1 — 42 has type :int, not :arrow
    (is (thrown? clojure.lang.ExceptionInfo
                 (ti/infer empty-env
                           {:kind :app
                            :fn  {:kind :int-lit :val 42}
                            :arg {:kind :int-lit :val 1}})))))

(deftest type-error-add-bool
  (testing "adding a bool operand throws ExceptionInfo"
    ;; true + 1 — left operand is :bool, not :int
    (is (thrown? clojure.lang.ExceptionInfo
                 (ti/infer empty-env
                           {:kind :add
                            :left  {:kind :bool-lit :val true}
                            :right {:kind :int-lit :val 1}})))))

(deftest type-error-mismatched-branches
  (testing "if-then-else with different branch types throws ExceptionInfo"
    ;; if true then 1 else false — :int ≠ :bool
    (is (thrown? clojure.lang.ExceptionInfo
                 (ti/infer empty-env
                           {:kind :iff
                            :cond {:kind :bool-lit :val true}
                            :then {:kind :int-lit :val 1}
                            :else {:kind :bool-lit :val false}})))))

(deftest type-error-wrong-arg-type
  (testing "applying λx.(x+1) to a bool throws ExceptionInfo"
    ;; (λx. x+1) true — :bool doesn't match arrow domain :int
    (is (thrown? clojure.lang.ExceptionInfo
                 (ti/infer empty-env
                           {:kind :app
                            :fn  {:kind :lam
                                  :param :x
                                  :body  {:kind :add
                                          :left  {:kind :var :name :x}
                                          :right {:kind :int-lit :val 1}}}
                            :arg {:kind :bool-lit :val true}})))))
