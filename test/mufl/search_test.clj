(ns mufl.search-test
  (:require [clojure.test :refer [deftest testing is]]
            [mufl.core :as m]))

;; ════════════════════════════════════════════════════════════════
;; Solve edge cases
;; ════════════════════════════════════════════════════════════════

(deftest no-solutions
  (testing "all branches contradict during solve"
    (is (thrown? clojure.lang.ExceptionInfo
                 (m/query (let [x (one-of 1 2 3)]
                            (and (> x 5) x)))))))

(deftest single-solution-already-ground
  (testing "all variables ground after propagation — one solution"
    (is (= [5] (m/query (let [x (one-of 5)] x))))))

(deftest solve-no-variables
  (testing "solve on scope with no variables — literal return"
    (is (= [42] (m/query 42)))
    (is (= [true] (m/query true)))
    (is (= ["hello"] (m/query "hello")))))

(deftest ground-literal-expressions
  (testing "literal arithmetic returns single result"
    (is (= [6] (m/query (+ 2 4))))
    (is (= [2] (m/query (- 5 3))))))

(deftest solve-cartesian-product
  (testing "unconstrained variables enumerate full cartesian product"
    (let [results (m/query (let [x (one-of 1 2)
                                 y (one-of :a :b)]
                             [x y]))]
      (is (= 4 (count results)))
      (is (= #{[1 :a] [1 :b] [2 :a] [2 :b]} (set results))))))

;; ════════════════════════════════════════════════════════════════
;; If / cond / or edge cases
;; ════════════════════════════════════════════════════════════════

(deftest if-different-return-shapes
  (testing "if with different return shapes per branch"
    (let [results (m/query (let [x (one-of 1 2 3 4 5)]
                             (if (< x 3) [x] x)))]
      (is (some #(vector? %) results))
      (is (some #(number? %) results)))))

(deftest nested-if
  (testing "nested if: (if (< x 3) (if (> x 1) x 0) 10)"
    (let [results (m/query (let [x (between 1 5)]
                             (if (< x 3) (if (> x 1) x 0) 10)))]
      (is (= #{0 2 10} (set results))))))

(deftest if-boolean-literal-true
  (testing "(if true x y) — only then branch"
    (is (= #{1 2 3}
           (set (m/query (let [x (one-of 1 2 3)]
                           (if true x 99))))))))

(deftest if-boolean-literal-false
  (testing "(if false x y) — only else branch"
    (is (= [99]
           (m/query (let [x (one-of 1 2 3)]
                      (if false x 99)))))))

(deftest or-single-branch
  (testing "(or (= x 1)) — single branch"
    (is (= [1] (m/query (let [x (one-of 1 2 3)]
                          (and (or (= x 1)) x)))))))

(deftest when-contradicting-body
  (testing "when with body that contradicts — should not crash outer"
    (let [results (m/query (let [x (between 1 5)]
                             (when (< x 3) (> x 5))))]
      (is (some? results)))))

(deftest if-with-nil-else
  (testing "if with nil else branch"
    (let [results (m/query (let [x (one-of 1 2 3)]
                             (if (= x 1) :found nil)))]
      (is (some #{:found} results))
      (is (some nil? results)))))

(deftest cond-boolean-literal-test
  (testing "cond with true as test"
    (is (= #{1 2 3}
           (set (m/query (let [x (one-of 1 2 3)]
                           (cond true x))))))))

(deftest cond-false-literal-test
  (testing "cond with false as test — branch is skipped"
    (is (= [99]
           (m/query (let [x (one-of 1 2 3)]
                      (cond false x
                            :else 99)))))))

;; ════════════════════════════════════════════════════════════════
;; Or edge cases
;; ════════════════════════════════════════════════════════════════

(deftest or-empty-branches
  (testing "(or) with no branches should error"
    (is (thrown? clojure.lang.ExceptionInfo
                 (m/query (let [x (one-of 1 2 3)]
                            (and (or) x)))))))

(deftest or-all-contradict
  (testing "or where all branches contradict"
    (is (thrown? clojure.lang.ExceptionInfo
                 (m/query (let [x (one-of 1 2 3)]
                            (and (or (= x 10) (= x 20)) x)))))))

(deftest or-loses-correlation
  (testing "or unions domains independently — loses branch correlation (known limitation)"
    ;; (or (and (= x 1) (= y 2)) (and (= x 3) (= y 4)))
    ;; should ideally give {[1,2], [3,4]} but gives {[1,2], [1,4], [3,2], [3,4]}
    ;; because or only unions x→{1,3}, y→{2,4} independently
    (let [results (m/query (let [x (one-of 1 2 3 4 5)
                                 y (one-of 1 2 3 4 5)]
                             (and (or (and (= x 1) (= y 2))
                                      (and (= x 3) (= y 4)))
                                  [x y])))]
      ;; We get overapproximation due to domain union
      (is (>= (count results) 2))
      ;; But the correct pairs ARE in the results
      (is (some #{[1 2]} results))
      (is (some #{[3 4]} results)))))

;; ════════════════════════════════════════════════════════════════
;; Cond edge cases
;; ════════════════════════════════════════════════════════════════

(deftest cond-with-else-only
  (testing "cond with only :else branch"
    (is (= #{1 2 3}
           (set (m/query (let [x (one-of 1 2 3)]
                           (cond :else x))))))))

(deftest cond-empty-no-branches
  (testing "(cond) with no branches returns no fork (empty partition)"
    ;; cond with no args → empty :branches, solve finds no viable branches
    (let [results (m/query (let [x (one-of 1 2 3)]
                             (cond)))]
      (is (= [] results)))))

;; ════════════════════════════════════════════════════════════════
;; Deeply nested if
;; ════════════════════════════════════════════════════════════════

(deftest triple-nested-if
  (testing "three levels of nested if"
    (let [results (m/query (let [x (between 1 10)]
                             (if (< x 4)
                               (if (< x 2) :tiny :small)
                               (if (> x 7) :big :medium))))]
      (is (= #{:tiny :small :medium :big} (set results))))))

;; ════════════════════════════════════════════════════════════════
;; Distinct edge cases
;; ════════════════════════════════════════════════════════════════

(deftest distinct-two-vars
  (testing "distinct with two variables"
    (let [results (m/query (let [x (one-of 1 2)
                                 y (one-of 1 2)]
                             (and (distinct [x y]) [x y])))]
      (is (= #{[1 2] [2 1]} (set results))))))

(deftest distinct-singleton-cascade
  (testing "distinct with singleton cascades to narrow others"
    (let [results (m/query (let [a (one-of 1)
                                 b (one-of 1 2)
                                 c (one-of 1 2 3)]
                             (and (distinct [a b c]) [a b c])))]
      ;; a=1, b must be 2, c must be 3
      (is (= [[1 2 3]] results)))))

;; ════════════════════════════════════════════════════════════════
;; Or disjunction (from phase2)
;; ════════════════════════════════════════════════════════════════

(deftest or-disjunction
  (testing "or narrows to union of branches"
    (is (= #{3 7}
           (set (m/query (let [x (between 1 10)]
                           (and (or (= x 3) (= x 7)) x)))))))
  (testing "or with between constraints"
    (is (= #{1 2 8 9 10}
           (set (m/query (let [x (between 1 10)]
                           (and (or (< x 3) (> x 7)) x))))))))

;; ════════════════════════════════════════════════════════════════
;; If branching (from phase2)
;; ════════════════════════════════════════════════════════════════

(deftest if-branching
  (testing "if returns union of branch values"
    (is (= #{1 2 3 4 10}
           (set (m/query (let [x (between 1 10)] (if (< x 5) x 10)))))))
  (testing "if with vector return"
    (let [results (m/query (let [x (one-of 1 2 3)
                                 y (one-of 4 5 6)]
                             (if (< x 2) [x y] [y x])))]
      ;; then: x=1, [1,y] for y∈{4,5,6}
      ;; else: x∈{2,3}, [y,x] for all combos
      (is (some #{[1 4]} results))
      (is (some #{[4 2]} results)))))

;; ════════════════════════════════════════════════════════════════
;; Cond branching (from phase2)
;; ════════════════════════════════════════════════════════════════

(deftest cond-branching
  (testing "cond returns union of all branches"
    (is (= #{0 1 2 3 8 9 10}
           (set (m/query (let [x (between 1 10)]
                           (cond
                             (< x 4) x
                             (> x 7) x
                             :else 0))))))))
