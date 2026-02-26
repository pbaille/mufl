(ns mufl.multi-branch-test
  "Tests for multi-branch fn with pattern matching."
  (:require [clojure.test :refer [deftest testing is]]
            [mufl.core :as m]))

;; ════════════════════════════════════════════════════════════════
;; 1. BASIC MULTI-BRANCH — literal matching
;; ════════════════════════════════════════════════════════════════

(deftest literal-pattern-matching
  (testing "two branches with literal first param"
    (is (= [1]
           (m/query (let [fact (fn [0] 1
                                  [n] (* n (fact (- n 1))))]
                      (fact 0))))))

  (testing "factorial via multi-branch"
    (is (= [120]
           (m/query (let [fact (fn [0] 1
                                  [n] (* n (fact (- n 1))))]
                      (fact 5))))))

  (testing "fibonacci via multi-branch"
    (is (= [55]
           (m/query (let [fib (fn [0] 0
                                  [1] 1
                                  [n] (+ (fib (- n 1)) (fib (- n 2))))]
                      (fib 10))))))

  (testing "keyword literal dispatch"
    (is (= [:yes]
           (m/query (let [f (fn [:ok] :yes
                               [:err] :no)]
                      (f :ok))))))

  (testing "keyword literal dispatch — second branch"
    (is (= [:no]
           (m/query (let [f (fn [:ok] :yes
                               [:err] :no)]
                      (f :err)))))))

;; ════════════════════════════════════════════════════════════════
;; 2. MIXED ARITY
;; ════════════════════════════════════════════════════════════════

(deftest mixed-arity-branches
  (testing "single and two-arg branches"
    (is (= [10]
           (m/query (let [f (fn [x] (* x 2)
                              [x y] (+ x y))]
                      (f 5))))))

  (testing "two-arg branch selected"
    (is (= [8]
           (m/query (let [f (fn [x] (* x 2)
                              [x y] (+ x y))]
                      (f 3 5)))))))

;; ════════════════════════════════════════════════════════════════
;; 3. DEFAULT CASE
;; ════════════════════════════════════════════════════════════════

(deftest default-branch
  (testing "default when no branch matches"
    (is (= [42]
           (m/query (let [f (fn [0] :zero
                              42)]
                      (f 5))))))

  (testing "branch matches before default"
    (is (= [:zero]
           (m/query (let [f (fn [0] :zero
                              42)]
                      (f 0)))))))

;; ════════════════════════════════════════════════════════════════
;; 4. TYPE WRAPPERS IN PARAMS
;; ════════════════════════════════════════════════════════════════

(deftest type-wrapper-params
  (testing "type wrapper narrows in multi-branch"
    (is (= [:int]
           (m/query (let [f (fn [(integer x)] :int
                              [(string x)] :str)]
                      (f 42))))))

  (testing "type wrapper — second branch"
    (is (= [:str]
           (m/query (let [f (fn [(integer x)] :int
                              [(string x)] :str)]
                      (f "hello")))))))

;; ════════════════════════════════════════════════════════════════
;; 5. DEFN MULTI-BRANCH
;; ════════════════════════════════════════════════════════════════

(deftest defn-multi-branch
  (testing "defn factorial"
    (is (= [120]
           (m/query (do (defn fact
                          [0] 1
                          [n] (* n (fact (- n 1))))
                        (fact 5))))))

  (testing "defn with constraint in branch body"
    (is (= [3 4 5]
           (m/query (do (defn positive [x]
                          (and (> x 0) x))
                        (let [n (one-of -1 0 1 2 3 4 5)]
                          (and (> (positive n) 2) n))))))))

;; ════════════════════════════════════════════════════════════════
;; 6. BIDIRECTIONAL — multi-branch destructuring
;; ════════════════════════════════════════════════════════════════

(deftest multi-branch-destructuring
  (testing "destruct uses correct branch inverse"
    (is (= [[1 2]]
           (m/query (do (defn point [x y] {:x x :y y})
                        (let [(point a b) {:x 1 :y 2}]
                          [a b]))))))

  (testing "literal branch inverts correctly"
    (is (= [0]
           (m/query (let [f (fn [0] :zero
                              [n] :nonzero)
                          (f x) :zero]
                      x)))))

  (testing "non-literal branch inverts via map body"
    (is (= [[1 2]]
           (m/query (do (defn point [x y] {:x x :y y})
                        (let [(point a b) {:x 1 :y 2}]
                          [a b])))))))

;; ════════════════════════════════════════════════════════════════
;; 7. FIRST-MATCH SEMANTICS
;; ════════════════════════════════════════════════════════════════

(deftest first-match-semantics
  (testing "first matching branch wins — both could match but first takes precedence"
    (is (= [:specific]
           (m/query (let [f (fn [0] :specific
                              [n] :general)]
                      (f 0)))))))

;; ════════════════════════════════════════════════════════════════
;; 8. RECURSION WITH MULTI-BRANCH (no if needed)
;; ════════════════════════════════════════════════════════════════

(deftest recursive-multi-branch
  (testing "sum-to via multi-branch recursion"
    (is (= [15]
           (m/query (let [sum-to (fn [0] 0
                                    [n] (+ n (sum-to (- n 1))))]
                      (sum-to 5))))))

  (testing "list length via multi-branch"
    (is (= [120]
           (m/query (do (defn fact
                          [0] 1
                          [n] (* n (fact (- n 1))))
                        (fact 5)))))))
