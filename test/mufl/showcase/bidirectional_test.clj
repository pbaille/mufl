(ns mufl.showcase.bidirectional-test
  "Tests for the Bidirectional Functions showcase.

   One `deftest` per showcase section. Verifies that the bidirectional
   fn/defn patterns produce the expected results in both expression and
   pattern position."
  (:require [clojure.test :refer [deftest testing is]]
            [mufl.core :as m]))

;;════════════════════════════════════════════════════════════════
;; 1. THE CORE IDEA
;;════════════════════════════════════════════════════════════════

(deftest test-core-idea
  (testing "expression position — construct a point"
    (is (= [{:x 1 :y 2}]
           (m/query
            (do
              (defn point [x y] {:x (integer x) :y (integer y)})
              (point 1 2))))))

  (testing "pattern position — destruct a point"
    (is (= [[1 2]]
           (m/query
            (do
              (defn point [x y] {:x (integer x) :y (integer y)})
              (let [(point a b) {:x 1 :y 2}]
                [a b]))))))

  (testing "both positions in same query"
    (is (= [[{:x 3 :y 4} 10 20]]
           (m/query
            (do
              (defn point [x y] {:x (integer x) :y (integer y)})
              (let [constructed  (point 3 4)
                    (point cx cy) {:x 10 :y 20}]
                [constructed cx cy])))))))

;;════════════════════════════════════════════════════════════════
;; 2. TYPE GUARDS PROPAGATE BOTH WAYS
;;════════════════════════════════════════════════════════════════

(deftest test-type-guards
  (testing "expression position — let-bound integer domain, all values pass"
    (is (= [{:x 1 :y 0} {:x 2 :y 0} {:x 3 :y 0}]
           (m/query
            (do
              (defn point [x y] {:x (integer x) :y (integer y)})
              (let [x (one-of 1 2 3)]
                (point x 0)))))))

  (testing "pattern position — non-integers filtered from extracted domain"
    (is (= [[1 10] [2 10]]
           (m/query
            (do
              (defn point [x y] {:x (integer x) :y (integer y)})
              (let [(point a b) {:x (one-of 1 "hello" 2) :y 10}]
                [a b])))))))

;;════════════════════════════════════════════════════════════════
;; 3. BODY CONSTRAINTS ARE INVERTIBLE
;;════════════════════════════════════════════════════════════════

(deftest test-body-constraints-bidirectional
  (testing "expression position — out-of-range arg raises contradiction"
    (is (thrown? clojure.lang.ExceptionInfo
                 (m/query
                  (do
                    (defn bounded-point [x y]
                      (and (>= x 0) (<= x 5)
                           (>= y 0) (<= y 5)
                           {:x (integer x) :y (integer y)}))
                    (bounded-point -1 2))))))

  (testing "pattern position — out-of-range values filtered from extraction"
    (is (= [[0 2] [1 2] [2 2] [3 2] [4 2]]
           (m/query
            (do
              (defn bounded-point [x y]
                (and (>= x 0) (<= x 5)
                     (>= y 0) (<= y 5)
                     {:x (integer x) :y (integer y)}))
              (let [(bounded-point a b) {:x (one-of 0 1 2 3 4) :y 2}]
                [a b]))))))

  (testing "pattern position — both axes filtered simultaneously"
    (is (= [[3 5]]
           (m/query
            (do
              (defn bounded-point [x y]
                (and (>= x 0) (<= x 5)
                     (>= y 0) (<= y 5)
                     {:x (integer x) :y (integer y)}))
              (let [(bounded-point a b) {:x (one-of -1 3 7) :y (one-of -1 5 6)}]
                [a b])))))))

;;════════════════════════════════════════════════════════════════
;; 4. THE `as` PATTERN
;;════════════════════════════════════════════════════════════════

(deftest test-as-pattern
  (testing "bind whole + extract components"
    (is (= [[{:x 5 :y 10} 5 10]]
           (m/query
            (do
              (defn point [x y] {:x (integer x) :y (integer y)})
              (let [(as p (point x y)) {:x 5 :y 10}]
                [p x y]))))))

  (testing "additional constraints on extracted components"
    (is (= [[{:x 2 :y 10} 2 10] [{:x 3 :y 10} 3 10]]
           (m/query
            (do
              (defn point [x y] {:x (integer x) :y (integer y)})
              (let [(as p (point x y)) {:x (one-of 1 2 3) :y 10}]
                (> x 1)
                [p x y])))))))

;;════════════════════════════════════════════════════════════════
;; 5. ROUND-TRIP IDENTITY
;;════════════════════════════════════════════════════════════════

(deftest test-round-trip
  (testing "construct then destruct recovers original args"
    (is (= [[2 3]]
           (m/query
            (do
              (defn point [x y] {:x (integer x) :y (integer y)})
              (let [p           (point 2 3)
                    (point a b) p]
                [a b]))))))

  (testing "round-trip across an entire domain — let-bound variables"
    (is (= #{[1 4] [2 4] [3 4]
             [1 5] [2 5] [3 5]
             [1 6] [2 6] [3 6]}
           (set (m/query
                 (do
                   (defn point [x y] {:x (integer x) :y (integer y)})
                   (let [x           (one-of 1 2 3)
                         y           (one-of 4 5 6)
                         p           (point x y)
                         (point a b) p]
                     [a b])))))))

  (testing "destruct then reconstruct produces same value"
    (is (= [{:x 7 :y 9}]
           (m/query
            (do
              (defn point [x y] {:x (integer x) :y (integer y)})
              (let [(point a b)    {:x 7 :y 9}
                    reconstructed (point a b)]
                reconstructed)))))))

;;════════════════════════════════════════════════════════════════
;; 6. BACKWARD ARITHMETIC
;;════════════════════════════════════════════════════════════════

(deftest test-backward-arithmetic
  (testing "equality constraint propagates backward through double"
    (is (= [3]
           (m/query
            (let [double (fn [x] (+ x x))
                  n      (between 1 5)]
              (and (= (double n) 6) n))))))

  (testing "equality constraint propagates backward through translate"
    (is (= [3]
           (m/query
            (let [translate (fn [x] (+ x 10))
                  n         (between 0 20)]
              (and (= (translate n) 13) n)))))))

;;════════════════════════════════════════════════════════════════
;; 7. NAMED CONSTRAINT FUNCTIONS
;;════════════════════════════════════════════════════════════════

(deftest test-named-constraint-functions
  (testing "on-diagonal: equates two independent map fields"
    (is (= #{[0 0] [1 1] [2 2] [3 3]}
           (set (m/query
                 (do
                   (defn on-diagonal [p] (= (:x p) (:y p)))
                   (let [p {:x (between 0 3) :y (between 0 3)}]
                     (on-diagonal p)
                     [(:x p) (:y p)])))))))

  (testing "above-diagonal: only pairs where x < y survive"
    (is (= #{[1 2] [1 3] [2 3]}
           (set (m/query
                 (do
                   (defn above-diagonal [p] (< (:x p) (:y p)))
                   (let [p {:x (between 1 3) :y (between 1 3)}]
                     (above-diagonal p)
                     [(:x p) (:y p)])))))))

  (testing "composing named constraints — intersection"
    (is (= [[2 3]]
           (m/query
            (do
              (defn above-diagonal [p] (< (:x p) (:y p)))
              (defn far-from-origin [p] (and (>= (:x p) 2) (>= (:y p) 2)))
              (let [p {:x (between 1 3) :y (between 1 3)}]
                (above-diagonal p)
                (far-from-origin p)
                [(:x p) (:y p)])))))))

;;════════════════════════════════════════════════════════════════
;; 8. MULTI-BRANCH DISPATCH
;;════════════════════════════════════════════════════════════════

(deftest test-multi-branch-dispatch
  (testing "integer branch selected for integer input"
    (is (= [:number]
           (m/query
            (let [label (fn [(integer x)] :number
                          [(string x)]  :text)]
              (label 42))))))

  (testing "string branch selected for string input"
    (is (= [:text]
           (m/query
            (let [label (fn [(integer x)] :number
                          [(string x)]  :text)]
              (label "hello"))))))

  (testing "both branches in one vector"
    (is (= [[:number :text]]
           (m/query
            (let [label (fn [(integer x)] :number
                          [(string x)]  :text)]
              [(label 42) (label "hello")])))))

  (testing "dispatch combines with point constructor — let-bound domain"
    (is (= [[{:x 1 :y 1} :number] [{:x 2 :y 2} :number] [{:x 3 :y 3} :number]]
           (m/query
            (do
              (defn point [x y] {:x (integer x) :y (integer y)})
              (let [label (fn [(integer x)] :number
                            [(string x)]  :text)
                    xs    (one-of 1 2 3)]
                [(point xs xs) (label xs)])))))))
