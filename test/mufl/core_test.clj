(ns mufl.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [mufl.core :as m]))

;; ════════════════════════════════════════════════════════════════
;; Phase 1: Basic domain + single variable
;; ════════════════════════════════════════════════════════════════

(deftest basic-one-of
  (testing "single variable enumeration"
    (is (= #{1 2 3}
           (set (m/query (let [x (one-of 1 2 3)] x))))))

  (testing "single value domain"
    (is (= [42]
           (m/query (let [x (one-of 42)] x))))))

;; ════════════════════════════════════════════════════════════════
;; Phase 2: Constraints narrow domains
;; ════════════════════════════════════════════════════════════════

(deftest basic-constraints
  (testing "> narrows"
    (is (= #{2 3}
           (set (m/query (let [x (one-of 1 2 3)]
                           (and (> x 1) x)))))))

  (testing "< narrows"
    (is (= #{1 2}
           (set (m/query (let [x (one-of 1 2 3 4 5)]
                           (and (< x 3) x)))))))

  (testing "!= narrows"
    (is (= #{1 3}
           (set (m/query (let [x (one-of 1 2 3)]
                           (and (!= x 2) x)))))))

  (testing "combined constraints"
    (is (= #{2 4}
           (set (m/query (let [x (one-of 1 2 3 4 5)]
                           (and (> x 1)
                                (< x 5)
                                (!= x 3)
                                x))))))))

;; ════════════════════════════════════════════════════════════════
;; Phase 3: Multi-variable + vector return
;; ════════════════════════════════════════════════════════════════

(deftest multi-variable
  (testing "two variables with constraints, vector return"
    (is (= #{[1 5] [2 4]}
           (set (m/query (let [x (one-of 1 2 3 4 5)
                               y (one-of 1 2 3 4 5)]
                           (and (< x y)
                                (= (+ x y) 6)
                                [x y]))))))))

;; ════════════════════════════════════════════════════════════════
;; Phase 4: Distinct (all-different)
;; ════════════════════════════════════════════════════════════════

(deftest distinct-constraint
  (testing "all different values — 3! permutations"
    (let [results (m/query (let [x (one-of 1 2 3)
                                 y (one-of 1 2 3)
                                 z (one-of 1 2 3)]
                             (and (distinct [x y z])
                                  [x y z])))]
      (is (= 6 (count results)))
      (is (every? #(= 3 (count (set %))) results)))))

;; ════════════════════════════════════════════════════════════════
;; Phase 5: Map coloring (classic CSP)
;; ════════════════════════════════════════════════════════════════

(deftest map-coloring
  (testing "3-node graph coloring with != constraints"
    (let [results (m/query (let [a (one-of 1 2 3)
                                 b (one-of 1 2 3)
                                 c (one-of 1 2 3)]
                             (and (!= a b)
                                  (!= b c)
                                  (!= a c)
                                  [a b c])))]
      (is (= 6 (count results)))
      (is (every? (fn [[a b c]]
                    (and (not= a b) (not= b c) (not= a c)))
                  results)))))

;; ════════════════════════════════════════════════════════════════
;; Phase 6: Contradiction
;; ════════════════════════════════════════════════════════════════

(deftest contradiction
  (testing "impossible constraint throws"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Contradiction"
                          (m/query (let [x (one-of 1 2 3)]
                                     (and (> x 5) x))))))

  (testing "contradictory equality"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Contradiction"
                          (m/query (let [x (one-of 1 2)
                                         y (one-of 3 4)]
                                     (and (= x y) [x y])))))))

;; ════════════════════════════════════════════════════════════════
;; Phase 7: Arithmetic
;; ════════════════════════════════════════════════════════════════

(deftest arithmetic
  (testing "derived sum variable"
    (is (= #{[1 5 6] [2 4 6]}
           (set (m/query (let [x (one-of 1 2 3 4 5)
                               y (one-of 1 2 3 4 5)
                               s (+ x y)]
                           (and (< x y)
                                (= s 6)
                                [x y s]))))))))
