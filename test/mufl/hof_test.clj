(ns mufl.hof-test
  (:require [clojure.test :refer [deftest testing is]]
            [mufl.core :as m]))

;; ════════════════════════════════════════════════════════════════
;; 1. MAP — basic usage
;; ════════════════════════════════════════════════════════════════

(deftest map-basic
  (testing "map with doubling function"
    (is (= [[2 4 6]]
           (m/query (let [v [1 2 3]
                          doubled (map (fn [x] (* x 2)) v)]
                      doubled)))))

  (testing "map with addition"
    (is (= [[11 12 13]]
           (m/query (let [v [1 2 3]
                          result (map (fn [x] (+ x 10)) v)]
                      result)))))

  (testing "map over empty vector"
    (is (= [[]]
           (m/query (let [v []
                          result (map (fn [x] (* x 2)) v)]
                      result))))))

(deftest map-with-constraints
  (testing "map with constrained input"
    (is (= [[2 4 6] [2 4 8] [2 6 8] [4 6 8]]
           (m/query (let [a (one-of 1 2 3 4)
                          b (one-of 2 3 4)
                          c (one-of 3 4)
                          v [a b c]
                          doubled (map (fn [x] (* x 2)) v)]
                      (< a b)
                      (< b c)
                      doubled))))))

(deftest map-preserves-constraint-propagation
  (testing "constraints on mapped result propagate back"
    (is (= [[2 4 6]]
           (m/query (let [v [1 2 3]
                          doubled (map (fn [x] (* x 2)) v)]
                      doubled))))))

;; ════════════════════════════════════════════════════════════════
;; 2. FILTER — predicate-based selection
;; ════════════════════════════════════════════════════════════════

(deftest filter-basic
  (testing "filter with even"
    (is (= [[2 4]]
           (m/query (let [v [1 2 3 4 5]
                          evens (filter even v)]
                      evens)))))

  (testing "filter with odd"
    (is (= [[1 3 5]]
           (m/query (let [v [1 2 3 4 5]
                          odds (filter odd v)]
                      odds)))))

  (testing "filter keeps all when all pass"
    (is (= [[2 4 6]]
           (m/query (let [v [2 4 6]
                          evens (filter even v)]
                      evens)))))

  (testing "filter removes all when none pass"
    (is (= [[]]
           (m/query (let [v [1 3 5]
                          evens (filter even v)]
                      evens))))))

(deftest filter-with-constraint-fn
  (testing "filter with defc predicate"
    (is (= [[3 4 5]]
           (m/query (do (defc big [x] (> x 2))
                        (let [v [1 2 3 4 5]
                              bigs (filter big v)]
                          bigs)))))))

(deftest filter-empty-vector
  (testing "filter over empty vector"
    (is (= [[]]
           (m/query (let [v []
                          result (filter even v)]
                      result))))))

;; ════════════════════════════════════════════════════════════════
;; 3. REDUCE — fold over collection
;; ════════════════════════════════════════════════════════════════

(deftest reduce-basic
  (testing "reduce with + for sum"
    (is (= [6]
           (m/query (let [v [1 2 3]
                          total (reduce + 0 v)]
                      total)))))

  (testing "reduce with * for product"
    (is (= [24]
           (m/query (let [v [1 2 3 4]
                          product (reduce * 1 v)]
                      product)))))

  (testing "reduce over single element"
    (is (= [42]
           (m/query (let [v [42]
                          result (reduce + 0 v)]
                      result)))))

  (testing "reduce over empty vector returns init"
    (is (= [0]
           (m/query (let [v []
                          result (reduce + 0 v)]
                      result))))))

(deftest reduce-with-fn
  (testing "reduce with custom function"
    (is (= [15]
           (m/query (let [v [1 2 3 4 5]
                          total (reduce (fn [acc x] (+ acc x)) 0 v)]
                      total))))))

;; ════════════════════════════════════════════════════════════════
;; 4. COMPOSED HOFs
;; ════════════════════════════════════════════════════════════════

(deftest hof-composed
  (testing "map then reduce (sum of doubles)"
    (is (= [12]
           (m/query (let [v [1 2 3]
                          doubled (map (fn [x] (* x 2)) v)
                          total (reduce + 0 doubled)]
                      total)))))

  (testing "filter then reduce (sum of evens)"
    (is (= [6]
           (m/query (let [v [1 2 3 4 5]
                          evens (filter even v)
                          total (reduce + 0 evens)]
                      total))))))

;; ════════════════════════════════════════════════════════════════
;; 5. HOFs WITH DOMAIN CONSTRAINTS
;; ════════════════════════════════════════════════════════════════

(deftest hof-with-domain
  (testing "map result constrained by domain"
    (is (= [[2 4 6]]
           (m/query (let [v [1 2 3]
                          result (map (fn [x] (* x 2)) v)]
                      result))))))

;; ════════════════════════════════════════════════════════════════
;; 6. REDUCE ARITHMETIC
;; ════════════════════════════════════════════════════════════════

(deftest reduce-arithmetic
  (testing "sum of first 5 integers"
    (is (= [15]
           (m/query (let [v [1 2 3 4 5]
                          s (reduce + 0 v)]
                      s)))))

  (testing "factorial via reduce"
    (is (= [120]
           (m/query (let [v [1 2 3 4 5]
                          f (reduce * 1 v)]
                      f))))))
