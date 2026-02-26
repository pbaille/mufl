(ns mufl.defn-constructor-test
  "Tests for defn as constructor/destructor.
   defn defines a constructor that builds values in expression position
   and destructs them in pattern position."
  (:require [clojure.test :refer [deftest testing is]]
            [mufl.core :as m]))

;; ════════════════════════════════════════════════════════════════
;; 1. CONSTRUCTOR — expression position
;; ════════════════════════════════════════════════════════════════

(deftest constructor-basic-map
  (testing "defn creates a map from args"
    (is (= [{:x 1 :y 2}]
           (m/query (do (defn point [x y] {:x x :y y})
                        (point 1 2))))))

  (testing "defn with integer constraints in body"
    (is (= [{:x 1 :y 2}]
           (m/query (do (defn point [x y] {:x (integer x) :y (integer y)})
                        (point 1 2))))))

  (testing "field access on constructed value"
    (is (= [1]
           (m/query (do (defn point [x y] {:x (integer x) :y (integer y)})
                        (let [p (point 1 2)]
                          (:x p)))))))

  (testing "both fields accessible"
    (is (= [[1 2]]
           (m/query (do (defn point [x y] {:x (integer x) :y (integer y)})
                        (let [p (point 1 2)]
                          [(:x p) (:y p)])))))))

(deftest constructor-basic-vector
  (testing "defn creates a vector from args"
    (is (= [[3 4]]
           (m/query (do (defn pair [x y] [x y])
                        (pair 3 4))))))

  (testing "defn with integer constraints in vector body"
    (is (= [[3 4]]
           (m/query (do (defn pair [x y] [(integer x) (integer y)])
                        (pair 3 4))))))

  (testing "element access on constructed vector"
    (is (= [3]
           (m/query (do (defn pair [x y] [(integer x) (integer y)])
                        (let [p (pair 3 4)]
                          (nth p 0))))))))

(deftest constructor-param-constraints
  (testing "constraints in params: (integer x) wrapper"
    (is (= [{:x 1 :y 2}]
           (m/query (do (defn point [(integer x) (integer y)] {:x x :y y})
                        (point 1 2))))))

  (testing "param constraints reject wrong types"
    (is (thrown? Exception
                (m/query (do (defn point [(integer x) (integer y)] {:x x :y y})
                             (point "hello" 2)))))))

(deftest constructor-nested
  (testing "constructor result used as arg to another constructor"
    (is (= [[1 4]]
           (m/query (do (defn point [x y] {:x x :y y})
                        (defn line [p1 p2] {:start p1 :end p2})
                        (let [l (line (point 1 2) (point 3 4))]
                          [(:x (:start l)) (:y (:end l))]))))))

  (testing "multiple constructor calls"
    (is (= [[4 6]]
           (m/query (do (defn point [x y] {:x x :y y})
                        (let [p (point 1 2)
                              q (point 3 4)]
                          [(+ (:x p) (:x q))
                           (+ (:y p) (:y q))])))))))

(deftest constructor-arity
  (testing "single-arg constructor"
    (is (= [{:val 42}]
           (m/query (do (defn wrapper [x] {:val (integer x)})
                        (wrapper 42))))))

  (testing "three-arg constructor"
    (is (= [{:x 1 :y 2 :z 3}]
           (m/query (do (defn point3d [x y z] {:x x :y y :z z})
                        (point3d 1 2 3))))))

  (testing "arity mismatch throws"
    (is (thrown? Exception
                (m/query (do (defn point [x y] {:x x :y y})
                             (point 1)))))))

;; ════════════════════════════════════════════════════════════════
;; 2. DESTRUCTOR — pattern position
;; ════════════════════════════════════════════════════════════════

(deftest destructor-basic-map
  (testing "basic map destructor"
    (is (= [[1 2]]
           (m/query (do (defn point [x y] {:x x :y y})
                        (let [(point a b) {:x 1 :y 2}]
                          [a b]))))))

  (testing "destructor with body constraints"
    (is (= [[1 2]]
           (m/query (do (defn point [x y] {:x (integer x) :y (integer y)})
                        (let [(point a b) {:x 1 :y 2}]
                          [a b]))))))

  (testing "destructor with param constraints"
    (is (= [[1 2]]
           (m/query (do (defn point [(integer x) (integer y)] {:x x :y y})
                        (let [(point a b) {:x 1 :y 2}]
                          [a b])))))))

(deftest destructor-basic-vector
  (testing "vector body destructor"
    (is (= [[3 4]]
           (m/query (do (defn pair [x y] [x y])
                        (let [(pair a b) [3 4]]
                          [a b]))))))

  (testing "vector body with constraints"
    (is (= [[3 4]]
           (m/query (do (defn pair [x y] [(integer x) (integer y)])
                        (let [(pair a b) [3 4]]
                          [a b])))))))

(deftest destructor-narrows-values
  (testing "body constraints narrow values during destructuring"
    (is (= [[1 10] [2 10]]
           (m/query (do (defn point [x y] {:x (integer x) :y (integer y)})
                        (let [(point a b) {:x (one-of 1 "hello" 2) :y 10}]
                          [a b]))))))

  (testing "param constraints narrow values during destructuring"
    (is (= [[1 10] [2 10]]
           (m/query (do (defn point [(integer x) (integer y)] {:x x :y y})
                        (let [(point a b) {:x (one-of 1 "hello" 2) :y 10}]
                          [a b])))))))

(deftest destructor-with-additional-constraints
  (testing "destructor + further constraint on extracted value"
    (is (= [[4 10] [5 10]]
           (m/query (do (defn point [x y] {:x (integer x) :y (integer y)})
                        (let [(point a b) {:x (one-of 1 2 3 4 5) :y 10}]
                          (> a 3)
                          [a b])))))))

;; ════════════════════════════════════════════════════════════════
;; 3. ROUNDTRIP — construct then destruct
;; ════════════════════════════════════════════════════════════════

(deftest roundtrip
  (testing "construct then destruct (body constraints)"
    (is (= [[1 2]]
           (m/query (do (defn point [x y] {:x (integer x) :y (integer y)})
                        (let [p (point 1 2)
                              (point a b) p]
                          [a b]))))))

  (testing "construct then destruct (param constraints)"
    (is (= [[1 2]]
           (m/query (do (defn point [(integer x) (integer y)] {:x x :y y})
                        (let [p (point 1 2)
                              (point a b) p]
                          [a b]))))))

  (testing "construct then destruct vector"
    (is (= [[3 4]]
           (m/query (do (defn pair [x y] [x y])
                        (let [p (pair 3 4)
                              (pair a b) p]
                          [a b])))))))

;; ════════════════════════════════════════════════════════════════
;; 4. CONSTRAINT FUNCTION — backward compatibility
;; ════════════════════════════════════════════════════════════════

(deftest constraint-function-single-param
  (testing "single-param constraint function"
    (is (= [1 2 3]
           (m/query (do (defn positive [x] (> x 0))
                        (let [n (one-of -1 0 1 2 3)]
                          (positive n)
                          n)))))))

(deftest constraint-function-multi-param
  (testing "two-param constraint function"
    (is (= #{[2 1] [3 1] [3 2] [4 1] [4 2] [4 3] [5 1] [5 2] [5 3] [5 4]}
           (set (m/query (do (defn greater-than [a b] (> a b))
                             (let [x (one-of 1 2 3 4 5)
                                   y (one-of 1 2 3 4 5)]
                               (greater-than x y)
                               [x y]))))))))

(deftest constraint-function-multi-body
  (testing "constraint function with multiple body forms"
    (is (= [3 4 5 6 7]
           (m/query (do (defn bounded [x]
                          (> x 2)
                          (< x 8))
                        (let [n (between 1 10)]
                          (bounded n)
                          n)))))))

(deftest constraint-with-arithmetic
  (testing "constraint function using arithmetic"
    (is (= [2 4 6]
           (m/query (do (defn even-val [x] (= (mod x 2) 0))
                        (let [n (one-of 1 2 3 4 5 6)]
                          (even-val n)
                          n)))))))

(deftest constraint-sum
  (testing "sum-to constraint"
    (is (= #{[1 9] [2 8] [3 7] [4 6] [5 5] [6 4] [7 3] [8 2] [9 1]}
           (set (m/query (do (defn sums-to [a b target] (= (+ a b) target))
                             (let [x (between 1 10)
                                   y (between 1 10)]
                               (sums-to x y 10)
                               [x y]))))))))

(deftest constraint-with-domain
  (testing "defn combined with def domain"
    (is (= [25 30]
           (m/query (do (def person {:name string :age (between 0 150)})
                        (defn adult [p] (>= (:age p) 18))
                        (let [p {:name "Alice" :age (one-of 10 25 30)}]
                          (narrow p person)
                          (adult p)
                          (:age p))))))))

;; ════════════════════════════════════════════════════════════════
;; 5. MIXED USE — constructor + constraint in same program
;; ════════════════════════════════════════════════════════════════

(deftest mixed-constructor-constraint
  (testing "constructor and constraint function together"
    (is (= [[2 10] [3 10]]
           (m/query (do (defn point [x y] {:x (integer x) :y (integer y)})
                        (defn positive [x] (> x 0))
                        (let [(point a b) {:x (one-of -1 0 1 2 3) :y 10}]
                          (positive a)
                          (> a 1)
                          [a b]))))))

  (testing "constraint applied to constructed value"
    (is (= [3]
           (m/query (do (defn point [x y] {:x x :y y})
                        (defn x-positive [p] (> (:x p) 0))
                        (let [p (point (one-of -1 0 3) 2)]
                          (x-positive p)
                          (:x p))))))))

;; ════════════════════════════════════════════════════════════════
;; 6. def DOMAIN — expression-position constraint only (no destructuring)
;; ════════════════════════════════════════════════════════════════

(deftest def-domain-constraint
  (testing "def domain still works as expression-position constraint"
    (is (= [1 2 3]
           (m/query (do (def small-int (between 1 3))
                        (let [x (one-of 1 2 3 4 5)]
                          (narrow x small-int)
                          x))))))

  (testing "def domain constrains map"
    (is (= ["Alice"]
           (m/query (do (def person {:name string :age (between 0 150)})
                        (let [p {:name "Alice" :age 30}]
                          (narrow p person)
                          (:name p)))))))

  (testing "def domain narrows"
    (is (= [10]
           (m/query (do (def person {:name string :age (between 0 150)})
                        (let [p {:name "Alice" :age (one-of 10 200)}]
                          (narrow p person)
                          (:age p))))))))
