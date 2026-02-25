(ns mufl.dot-syntax-test
  "Tests for dot syntax in destructuring patterns.
   Ported from immucode's `.` rest pattern convention."
  (:require [clojure.test :refer [deftest testing is]]
            [mufl.core :as m]))

;; ════════════════════════════════════════════════════════════════
;; Vector dot syntax: [a b . xs]
;; ════════════════════════════════════════════════════════════════

(deftest vector-dot-basic
  (testing "dot rest captures remaining elements"
    (is (= [[3 4 5]]
           (m/query (let [[a b . xs] [1 2 3 4 5]] xs)))))

  (testing "dot rest with first element"
    (is (= [[10 [20 30]]]
           (m/query (let [[a . xs] [10 20 30]] [a xs])))))

  (testing "dot rest returns two-element rest"
    (is (= [[2 3]]
           (m/query (let [[a . xs] [1 2 3]] xs)))))

  (testing "dot rest returns single-element rest"
    (is (= [[20]]
           (m/query (let [[a . xs] [10 20]] xs)))))

  (testing "dot rest returns empty vector when nothing left"
    (is (= [[]]
           (m/query (let [[a . xs] [42]] xs))))))

(deftest vector-dot-bare
  (testing "bare dot ignores rest (no binding after dot)"
    (is (= [[1 2]]
           (m/query (let [[a b .] [1 2 3 4]] [a b]))))))

(deftest vector-dot-with-as
  (testing "dot syntax + as operator binds both rest and whole"
    (is (= [[[3 4 5] [1 2 3 4 5]]]
           (m/query (let [(as v [a b . xs]) [1 2 3 4 5]] [xs v])))))

  (testing "as operator works with vector pattern"
    (is (= [[10 20]]
           (m/query (let [(as v [a b]) [10 20]] v))))))

(deftest vector-dot-nested-rest
  (testing "dot rest with nested vector destructuring"
    (is (= [[3 4]]
           (m/query (let [[a b . [c d]] [1 2 3 4]] [c d])))))

  (testing "dot rest with nested [h . t] on rest"
    (is (= [3]
           (m/query (let [[a b . [h . t]] [1 2 3 4 5]] h)))))

  (testing "dot rest element count"
    (is (= [3]
           (m/query (let [[a b . xs] [1 2 3 4 5]] (count xs)))))))

(deftest vector-dot-with-constraints
  (testing "constraint on positional element with dot rest"
    (is (= [2 3]
           (m/query (let [[a . xs] [(one-of 1 2 3) 10 20]]
                      (and (> a 1) a))))))

  (testing "positional elements + rest coexist"
    (is (= [1]
           (m/query (let [[a b . xs] [1 2 3 4 5]] a)))))

  (testing "rest has correct elements"
    (is (= [3]
           (m/query (let [[a b . xs] [1 2 3 4 5]
                          [c d e] xs]
                      c))))))

;; ════════════════════════════════════════════════════════════════
;; Vector head/middle/tail: [a . mid c] decomposition
;; ════════════════════════════════════════════════════════════════

(deftest vector-dot-head-middle-tail
  (testing "[a . mid c] — basic head/middle/last"
    (is (= [[1 [2 3 4] 5]]
           (m/query (let [[a . mid c] [1 2 3 4 5]] [a mid c])))))

  (testing "[a b . mid c d] — two head, two tail"
    (is (= [[1 2 [3 4 5] 6 7]]
           (m/query (let [[a b . mid c d] [1 2 3 4 5 6 7]] [a b mid c d])))))

  (testing "[. mid c] — no head, middle + tail"
    (is (= [[[1 2 3 4] 5]]
           (m/query (let [[. mid c] [1 2 3 4 5]] [mid c])))))

  (testing "[a . mid c] on exact-fit (empty middle)"
    (is (= [[1 [] 2]]
           (m/query (let [[a . mid c] [1 2]] [a mid c])))))

  (testing "middle count correct"
    (is (= [3]
           (m/query (let [[a . mid c] [1 2 3 4 5]] (count mid)))))))

(deftest vector-dot-head-middle-tail-with-constraints
  (testing "constraint on head with tail positionals"
    (is (= [2 3]
           (m/query (let [[a . mid c] [(one-of 1 2 3) 10 20 99]]
                      (and (> a 1) a))))))

  (testing "constraint on tail positional"
    (is (= [[1 [2 3] 4]]
           (m/query (let [[a . mid c] [1 2 3 (one-of 4 5 6)]]
                      (and (< c 5) [a mid c])))))))

(deftest vector-dot-head-middle-tail-with-as
  (testing "(as v [a . mid c]) — as wrapping head/middle/tail"
    (is (= [[[1 2 3 4 5] 1 [2 3 4] 5]]
           (m/query (let [(as v [a . mid c]) [1 2 3 4 5]]
                      [v a mid c]))))))

;; ════════════════════════════════════════════════════════════════
;; General map destructuring: {:key pattern}
;; ════════════════════════════════════════════════════════════════

(deftest map-general-basic
  (testing "general map pattern with symbol values"
    (is (= [[1 2]]
           (m/query (let [{:x a :y b} {:x 1 :y 2}] [a b])))))

  (testing "single key general pattern"
    (is (= [42]
           (m/query (let [{:x a} {:x 42 :y 99}] a)))))

  (testing "general map pattern with nested vector destructuring"
    (is (= [[1 2 3]]
           (m/query (let [{:x a :y [b c]} {:x 1 :y [2 3]}] [a b c])))))

  (testing "general map pattern with nested ks"
    (is (= [10]
           (m/query (let [{:x (ks y)} {:x {:y 10}}] y))))))

(deftest map-general-with-as
  (testing "general map pattern with as operator"
    (is (= [{:x 1 :y 2}]
           (m/query (let [(as m {:x a}) {:x 1 :y 2}] m))))))

;; ════════════════════════════════════════════════════════════════
;; Map dot syntax: {:key val . rest}
;; ════════════════════════════════════════════════════════════════

(deftest map-dot-basic
  (testing "dot rest captures remaining keys"
    (is (= [{:y 2 :z 3}]
           (m/query (let [{:x a . rest} {:x 1 :y 2 :z 3}] rest)))))

  (testing "dot rest with multiple named keys"
    (is (= [{:z 3}]
           (m/query (let [{:x a :y b . rest} {:x 1 :y 2 :z 3}] rest)))))

  (testing "dot rest with no remaining keys gives empty map"
    (is (= [{}]
           (m/query (let [{:x a :y b . rest} {:x 1 :y 2}] rest))))))

(deftest map-dot-nested-rest
  (testing "dot rest with nested ks destructuring"
    (is (= [[1 2 3]]
           (m/query (let [{:x a . (ks y z)} {:x 1 :y 2 :z 3}] [a y z]))))))

(deftest map-dot-with-as
  (testing "dot rest + as operator"
    (is (= [[{:y 2 :z 3} {:x 1 :y 2 :z 3}]]
           (m/query (let [(as m {:x a . rest}) {:x 1 :y 2 :z 3}] [rest m]))))))

(deftest map-dot-with-constraints
  (testing "constraint on destructured value with dot rest"
    (is (= [2 3]
           (m/query (let [{:x a . rest} {:x (one-of 1 2 3) :y 10}]
                      (and (> a 1) a)))))))

;; ════════════════════════════════════════════════════════════════
;; drop and dissoc primitives (direct usage)
;; ════════════════════════════════════════════════════════════════

(deftest drop-primitive
  (testing "drop 0 returns all elements"
    (is (= [[1 2 3]]
           (m/query (let [v [1 2 3]] (drop 0 v))))))

  (testing "drop 1 same as rest"
    (is (= [[2 3]]
           (m/query (let [v [1 2 3]] (drop 1 v))))))

  (testing "drop 2 returns from index 2"
    (is (= [[3 4 5]]
           (m/query (let [v [1 2 3 4 5]] (drop 2 v))))))

  (testing "drop all returns empty vector"
    (is (= [[]]
           (m/query (let [v [1 2]] (drop 2 v)))))))

(deftest dissoc-primitive
  (testing "dissoc single key"
    (is (= [{:y 2}]
           (m/query (let [m {:x 1 :y 2}] (dissoc m :x))))))

  (testing "dissoc multiple keys"
    (is (= [{:z 3}]
           (m/query (let [m {:x 1 :y 2 :z 3}] (dissoc m :x :y))))))

  (testing "dissoc all keys"
    (is (= [{}]
           (m/query (let [m {:x 1 :y 2}] (dissoc m :x :y)))))))
