(ns mufl.defc-test
  (:require [clojure.test :refer [deftest testing is]]
            [mufl.core :as m]))

;; ════════════════════════════════════════════════════════════════
;; 1. BASIC defn — single parameter constraint functions
;; ════════════════════════════════════════════════════════════════

(deftest defn-basic-positive
  (testing "basic constraint function filters values"
    (is (= [1 2 3]
           (m/query (do (defn positive [x] (> x 0))
                        (let [n (one-of -1 0 1 2 3)]
                          (positive n)
                          n)))))))

(deftest defn-basic-negative
  (testing "constraint function for negative values"
    (is (= [-3 -2 -1]
           (m/query (do (defn negative [x] (< x 0))
                        (let [n (one-of -3 -2 -1 0 1 2)]
                          (negative n)
                          n)))))))

(deftest defn-basic-nonzero
  (testing "constraint function for nonzero values"
    (is (= [-1 1 2]
           (m/query (do (defn nonzero [x] (!= x 0))
                        (let [n (one-of -1 0 1 2)]
                          (nonzero n)
                          n)))))))

;; ════════════════════════════════════════════════════════════════
;; 2. MULTI-PARAMETER defn
;; ════════════════════════════════════════════════════════════════

(deftest defn-multi-param
  (testing "two-parameter constraint function"
    (is (= #{[2 1] [3 1] [3 2] [4 1] [4 2] [4 3] [5 1] [5 2] [5 3] [5 4]}
           (set (m/query (do (defn greater-than [a b] (> a b))
                             (let [x (one-of 1 2 3 4 5)
                                   y (one-of 1 2 3 4 5)]
                               (greater-than x y)
                               [x y]))))))))

(deftest defn-in-range
  (testing "three-parameter in-range constraint"
    (is (= [3 4 5]
           (m/query (do (defn in-range [x lo hi]
                          (and (>= x lo) (<= x hi)))
                        (let [n (one-of 1 2 3 4 5 6 7)]
                          (in-range n 3 5)
                          n)))))))

;; ════════════════════════════════════════════════════════════════
;; 3. defn WITH ARITHMETIC
;; ════════════════════════════════════════════════════════════════

(deftest defn-with-arithmetic
  (testing "constraint function using arithmetic"
    (is (= [2 4 6]
           (m/query (do (defn even-val [x] (= (mod x 2) 0))
                        (let [n (one-of 1 2 3 4 5 6)]
                          (even-val n)
                          n)))))))

(deftest defn-sum-constraint
  (testing "constraint that two values sum to a target"
    (is (= #{[1 9] [2 8] [3 7] [4 6] [5 5] [6 4] [7 3] [8 2] [9 1]}
           (set (m/query (do (defn sums-to [a b target] (= (+ a b) target))
                             (let [x (between 1 10)
                                   y (between 1 10)]
                               (sums-to x y 10)
                               [x y]))))))))

;; ════════════════════════════════════════════════════════════════
;; 4. defn COMPOSED WITH DOMAIN CONSTRAINTS
;; ════════════════════════════════════════════════════════════════

(deftest defn-with-domain
  (testing "defn combined with def domain"
    (is (= [25 30]
           (m/query (do (def person {:name string :age (between 0 150)})
                        (defn adult [p] (>= (:age p) 18))
                        (let [p {:name "Alice" :age (one-of 10 25 30)}]
                          (narrow p person)
                          (adult p)
                          (:age p))))))))

;; ════════════════════════════════════════════════════════════════
;; 5. defn MULTIPLE APPLICATIONS
;; ════════════════════════════════════════════════════════════════

(deftest defn-multiple-applications
  (testing "applying same constraint function multiple times"
    (is (= [[1 2] [1 3] [2 3]]
           (m/query (do (defn positive [x] (> x 0))
                        (defn ordered [a b] (< a b))
                        (let [x (one-of -1 0 1 2 3)
                              y (one-of -1 0 1 2 3)]
                          (positive x)
                          (positive y)
                          (ordered x y)
                          [x y])))))))

;; ════════════════════════════════════════════════════════════════
;; 6. defn ERROR CASES
;; ════════════════════════════════════════════════════════════════

(deftest defn-arity-mismatch
  (testing "wrong number of arguments throws"
    (is (thrown? Exception
                (m/query (do (defn positive [x] (> x 0))
                             (let [n 5]
                               (positive n 3))))))))

;; ════════════════════════════════════════════════════════════════
;; 7. defn WITH MULTIPLE BODY FORMS
;; ════════════════════════════════════════════════════════════════

(deftest defn-multi-body
  (testing "constraint function with multiple body expressions"
    (is (= [3 4 5 6 7]
           (m/query (do (defn bounded [x]
                          (> x 2)
                          (< x 8))
                        (let [n (between 1 10)]
                          (bounded n)
                          n)))))))
