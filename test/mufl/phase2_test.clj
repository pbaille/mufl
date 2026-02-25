(ns mufl.phase2-test
  (:require [clojure.test :refer [deftest testing is]]
            [mufl.core :as m]))

;; ════════════════════════════════════════════════════════════════
;; range
;; ════════════════════════════════════════════════════════════════

(deftest range-domain
  (testing "integer range"
    (is (= [1 2 3 4 5] (m/query (let [x (range 1 5)] x)))))
  (testing "range with constraint"
    (is (= [8 9 10] (m/query (let [x (range 1 10)] (and (> x 7) x)))))))

;; ════════════════════════════════════════════════════════════════
;; mod / quot
;; ════════════════════════════════════════════════════════════════

(deftest mod-quot
  (testing "mod constraint — multiples of 3"
    (is (= [3 6 9 12 15 18]
           (m/query (let [x (range 1 20)] (and (= (mod x 3) 0) x))))))
  (testing "quot constraint"
    (is (= [6 7 8]
           (m/query (let [x (range 0 10)] (and (= (quot x 3) 2) x)))))))

;; ════════════════════════════════════════════════════════════════
;; even? / odd?
;; ════════════════════════════════════════════════════════════════

(deftest even-odd
  (testing "even?"
    (is (= [2 4 6 8 10]
           (m/query (let [x (range 1 10)] (and (even? x) x))))))
  (testing "odd?"
    (is (= [1 3 5 7 9]
           (m/query (let [x (range 1 10)] (and (odd? x) x)))))))

;; ════════════════════════════════════════════════════════════════
;; or
;; ════════════════════════════════════════════════════════════════

(deftest or-disjunction
  (testing "or narrows to union of branches"
    (is (= #{3 7}
           (set (m/query (let [x (range 1 10)]
                           (and (or (= x 3) (= x 7)) x)))))))
  (testing "or with range constraints"
    (is (= #{1 2 8 9 10}
           (set (m/query (let [x (range 1 10)]
                           (and (or (< x 3) (> x 7)) x))))))))

;; ════════════════════════════════════════════════════════════════
;; not
;; ════════════════════════════════════════════════════════════════

(deftest not-negation
  (testing "not ="
    (is (= #{1 3} (set (m/query (let [x (one-of 1 2 3)] (and (not (= x 2)) x)))))))
  (testing "not <"
    (is (= #{3 4 5} (set (m/query (let [x (one-of 1 2 3 4 5)] (and (not (< x 3)) x))))))))

;; ════════════════════════════════════════════════════════════════
;; if / cond
;; ════════════════════════════════════════════════════════════════

(deftest if-branching
  (testing "if returns union of branch values"
    (is (= #{1 2 3 4 10}
           (set (m/query (let [x (range 1 10)] (if (< x 5) x 10)))))))
  (testing "if with vector return"
    (let [results (m/query (let [x (one-of 1 2 3)
                                 y (one-of 4 5 6)]
                             (if (< x 2) [x y] [y x])))]
      ;; then: x=1, [1,y] for y∈{4,5,6}
      ;; else: x∈{2,3}, [y,x] for all combos
      (is (some #{[1 4]} results))
      (is (some #{[4 2]} results)))))

(deftest cond-branching
  (testing "cond returns union of all branches"
    (is (= #{0 1 2 3 8 9 10}
           (set (m/query (let [x (range 1 10)]
                           (cond
                             (< x 4) x
                             (> x 7) x
                             :else 0))))))))

;; ════════════════════════════════════════════════════════════════
;; fn (user-defined functions)
;; ════════════════════════════════════════════════════════════════

(deftest fn-application
  (testing "fn with constraint propagation — double"
    (is (= [3]
           (m/query (let [double (fn [x] (+ x x))
                          n (range 1 5)]
                      (and (= (double n) 6) n))))))
  (testing "fn with constraint propagation — square"
    (is (= [5]
           (m/query (let [sq (fn [x] (* x x))
                          n (range 1 10)]
                      (and (= (sq n) 25) n)))))))

;; ════════════════════════════════════════════════════════════════
;; abs
;; ════════════════════════════════════════════════════════════════

(deftest abs-constraint
  (testing "|x| = 2 finds both signs"
    (is (= #{-2 2}
           (set (m/query (let [x (one-of -3 -2 -1 0 1 2 3)]
                           (and (= (abs x) 2) x))))))))

;; ════════════════════════════════════════════════════════════════
;; min / max
;; ════════════════════════════════════════════════════════════════

(deftest min-max
  (testing "min of two domains"
    ;; min(3,2)=2, min(3,4)=3, min(5,2)=2, min(5,4)=4
    (is (= #{2 3 4}
           (set (m/query (let [x (one-of 3 5)
                               y (one-of 2 4)]
                           (min x y))))))))

;; ════════════════════════════════════════════════════════════════
;; Combined: FizzBuzz
;; ════════════════════════════════════════════════════════════════

(deftest fizzbuzz
  (testing "multiples of 3 or 5 under 20"
    (is (= #{3 5 6 9 10 12 15 18 20}
           (set (m/query (let [x (range 1 20)]
                           (and (or (= (mod x 3) 0)
                                    (= (mod x 5) 0))
                                x))))))))

;; ════════════════════════════════════════════════════════════════
;; Combined: Pythagorean triples
;; ════════════════════════════════════════════════════════════════

(deftest pythagorean-triples
  (testing "a² + b² = c² for a,b,c ≤ 15"
    (is (= #{[3 4 5] [6 8 10] [5 12 13] [9 12 15]}
           (set (m/query (let [a (range 1 15)
                               b (range 1 15)
                               c (range 1 15)]
                           (and (< a b)
                                (<= b c)
                                (= (+ (* a a) (* b b)) (* c c))
                                [a b c]))))))))
