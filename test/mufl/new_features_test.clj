(ns mufl.new-features-test
  (:require [clojure.test :refer [deftest testing is]]
            [mufl.core :as m]))

;; ════════════════════════════════════════════════════════════════
;; implicit do in query (multi-body forms)
;; ════════════════════════════════════════════════════════════════

(deftest implicit-do-in-query
  (testing "single body form still works"
    (is (= [42] (m/query 42))))

  (testing "multi-body query with defc"
    (is (= [1 2 3]
           (m/query (defc positive [x] (> x 0))
                    (let [n (one-of -1 0 1 2 3)]
                      (positive n)
                      n)))))

  (testing "multi-body query with defdomain"
    (is (= [10 25]
           (m/query (defdomain Person {:name string :age (between 0 150)})
                    (let [p {:name "Alice" :age (one-of 10 25 200)}]
                      (Person p)
                      (:age p))))))

  (testing "multi-body query with multiple defs"
    (is (= [25 30]
           (m/query (defdomain Person {:name string :age (between 0 150)})
                    (defc adult [p] (>= (:age p) 18))
                    (let [p {:name "Alice" :age (one-of 10 25 30)}]
                      (Person p)
                      (adult p)
                      (:age p)))))))

;; ════════════════════════════════════════════════════════════════
;; Numeric predicates: pos, neg, zero
;; ════════════════════════════════════════════════════════════════

(deftest pos-predicate
  (testing "pos filters to positive values"
    (is (= [1 2 3]
           (m/query (let [x (one-of -2 -1 0 1 2 3)]
                      (and (pos x) x))))))

  (testing "pos contradiction"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Contradiction"
          (m/query (let [x (one-of -2 -1 0)]
                     (and (pos x) x)))))))

(deftest neg-predicate
  (testing "neg filters to negative values"
    (is (= [-2 -1]
           (m/query (let [x (one-of -2 -1 0 1 2)]
                      (and (neg x) x))))))

  (testing "neg contradiction"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Contradiction"
          (m/query (let [x (one-of 0 1 2)]
                     (and (neg x) x)))))))

(deftest zero-predicate
  (testing "zero filters to zero"
    (is (= [0]
           (m/query (let [x (one-of -1 0 1)]
                      (and (zero x) x))))))

  (testing "zero contradiction"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Contradiction"
          (m/query (let [x (one-of 1 2 3)]
                     (and (zero x) x)))))))

;; ════════════════════════════════════════════════════════════════
;; Type predicates via type domains: (number x), (string x), (keyword x)
;; ════════════════════════════════════════════════════════════════

(deftest number-type-predicate
  (testing "number keeps numbers from mixed domain"
    (is (= #{1 2 3}
           (set (m/query (let [x (one-of 1 2 3 "a" "b" :k)]
                           (and (number x) x))))))))

(deftest string-type-predicate
  (testing "string keeps strings from mixed domain"
    (is (= #{"a" "b"}
           (set (m/query (let [x (one-of 1 2 "a" "b" :k)]
                           (and (string x) x))))))))

(deftest keyword-type-predicate
  (testing "keyword keeps keywords from mixed domain"
    (is (= #{:k :j}
           (set (m/query (let [x (one-of 1 "a" :k :j)]
                           (and (keyword x) x))))))))

;; ════════════════════════════════════════════════════════════════
;; Predicate composition with and/or
;; ════════════════════════════════════════════════════════════════

(deftest predicate-composition
  (testing "even and pos combined"
    (is (= [2 4]
           (m/query (let [x (one-of -4 -2 0 1 2 3 4)]
                      (and (even x) (pos x) x))))))

  (testing "or with predicates: even or negative"
    (is (= #{-3 -1 2 4}
           (set (m/query (let [x (one-of -3 -1 1 2 3 4)]
                           (and (or (even x) (neg x)) x))))))))
