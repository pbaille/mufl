(ns mufl.doc-examples-test
  "Tests verifying all examples from doc/introduction.md"
  (:require [clojure.test :refer [deftest testing is]]
            [mufl.core :as m]))

;; ════════════════════════════════════════════════════════════════
;; Values
;; ════════════════════════════════════════════════════════════════

(deftest doc-values
  (is (= [42] (m/query 42)))
  (is (= ["hello"] (m/query "hello")))
  (is (= [:ok] (m/query :ok)))
  (is (= [true] (m/query true))))

;; ════════════════════════════════════════════════════════════════
;; Uncertain values
;; ════════════════════════════════════════════════════════════════

(deftest doc-uncertain
  (is (= [1 2 3] (m/query (let [x (one-of 1 2 3)] x))))
  (is (= [1 2 3 4 5] (m/query (let [x (between 1 5)] x)))))

;; ════════════════════════════════════════════════════════════════
;; Constraints
;; ════════════════════════════════════════════════════════════════

(deftest doc-constraints
  (is (= [3 4 5]
         (m/query (let [x (one-of 1 2 3 4 5)]
                    (and (> x 2) x)))))
  (is (= [2 4]
         (m/query (let [x (one-of 1 2 3 4 5)]
                    (and (> x 1) (< x 5) (!= x 3) x))))))

;; ════════════════════════════════════════════════════════════════
;; Multiple variables
;; ════════════════════════════════════════════════════════════════

(deftest doc-multi-var
  (is (= #{[1 5] [2 4]}
         (set (m/query (let [x (one-of 1 2 3 4 5)
                              y (one-of 1 2 3 4 5)]
                          (and (< x y) (= (+ x y) 6) [x y])))))))

;; ════════════════════════════════════════════════════════════════
;; Arithmetic
;; ════════════════════════════════════════════════════════════════

(deftest doc-arithmetic
  (is (= [11 12 13]
         (m/query (let [x (one-of 1 2 3) s (+ x 10)] s))))
  (is (= #{[1 5 6] [2 4 6]}
         (set (m/query (let [x (one-of 1 2 3 4 5)
                              y (one-of 1 2 3 4 5)
                              s (+ x y)]
                          (and (< x y) (= s 6) [x y s]))))))
  (is (= [3 6 9 12 15 18]
         (m/query (let [x (between 1 20)]
                    (and (= (mod x 3) 0) x))))))

;; ════════════════════════════════════════════════════════════════
;; Distinct
;; ════════════════════════════════════════════════════════════════

(deftest doc-distinct
  (let [results (m/query (let [a (one-of 1 2 3)
                               b (one-of 1 2 3)
                               c (one-of 1 2 3)]
                           (and (distinct [a b c]) [a b c])))]
    (is (= 6 (count results)))))

;; ════════════════════════════════════════════════════════════════
;; Branching
;; ════════════════════════════════════════════════════════════════

(deftest doc-branching
  (is (= #{1 2 3 4 10}
         (set (m/query (let [x (between 1 10)]
                         (if (< x 5) x 10))))))
  (is (= #{0 1 2 3 8 9 10}
         (set (m/query (let [x (between 1 10)]
                         (cond
                           (< x 4) x
                           (> x 7) x
                           :else 0)))))))

;; ════════════════════════════════════════════════════════════════
;; Disjunction
;; ════════════════════════════════════════════════════════════════

(deftest doc-disjunction
  (is (= #{3 7}
         (set (m/query (let [x (between 1 10)]
                         (and (or (= x 3) (= x 7)) x))))))
  (is (= #{1 2 8 9 10}
         (set (m/query (let [x (between 1 10)]
                         (and (or (< x 3) (> x 7)) x))))))
  (is (= #{3 4 5}
         (set (m/query (let [x (one-of 1 2 3 4 5)]
                         (and (not (< x 3)) x)))))))

;; ════════════════════════════════════════════════════════════════
;; Predicates (enriched)
;; ════════════════════════════════════════════════════════════════

(deftest doc-predicates
  (is (= [2 4 6 8 10]
         (m/query (let [x (between 1 10)]
                    (and (even x) x)))))
  (is (= [1 2 3]
         (m/query (let [x (one-of -3 -2 -1 0 1 2 3)]
                    (and (pos x) x)))))
  (is (= [1 2]
         (m/query (let [x (one-of 1 2 "hello" "world" :ok)]
                    (and (number x) x)))))
  (is (= [2 4]
         (m/query (let [x (one-of -4 -2 0 1 2 3 4)]
                    (and (even x) (pos x) x)))))
  (is (= #{-3 -1 2 4}
         (set (m/query (let [x (one-of -3 -1 1 2 3 4)]
                         (and (or (even x) (neg x)) x)))))))

;; ════════════════════════════════════════════════════════════════
;; Functions
;; ════════════════════════════════════════════════════════════════

(deftest doc-functions
  (is (= [3]
         (m/query (let [double (fn [x] (+ x x))
                        n (between 1 5)]
                    (and (= (double n) 6) n))))))

;; ════════════════════════════════════════════════════════════════
;; Recursion
;; ════════════════════════════════════════════════════════════════

(deftest doc-recursion
  (is (= [120]
         (m/query (let [fact (fn [n]
                               (if (= n 0) 1 (* n (fact (- n 1)))))]
                    (fact 5)))))
  (is (= [55]
         (m/query (let [fib (fn [n]
                              (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))]
                    (fib 10)))))
  (is (= [32]
         (m/query (let [base 2
                        pow (fn [n] (if (= n 0) 1 (* base (pow (- n 1)))))]
                    (pow 5))))))

;; ════════════════════════════════════════════════════════════════
;; Collections
;; ════════════════════════════════════════════════════════════════

(deftest doc-collections
  (is (= [20] (m/query (let [v [10 20 30]] (nth v 1)))))
  (is (= [1] (m/query (let [m {:x 1 :y 2}] (get m :x)))))
  (is (= [1] (m/query (let [m {:x 1 :y 2}] (:x m)))))
  (is (= [{:x 2 :y 10} {:x 3 :y 10}]
         (m/query (let [m {:x (one-of 1 2 3) :y 10}]
                    (and (> (:x m) 1) m)))))
  (is (= [42] (m/query (let [m {:a {:b 42}}] (:b (:a m)))))))

;; ════════════════════════════════════════════════════════════════
;; Destructuring
;; ════════════════════════════════════════════════════════════════

(deftest doc-destructuring
  (is (= [[1 2] [1 3] [2 3]]
         (m/query (let [{:x x :y y} {:x (one-of 1 2 3) :y (one-of 1 2 3)}]
                    (and (< x y) [x y])))))
  (is (= [30]
         (m/query (let [[a b c] [10 20 30]] (+ a b)))))
  (is (= [[2 10] [3 10]]
         (m/query (let [(as v [a b]) [(one-of 1 2 3) 10]]
                    (and (> a 1) v))))))

;; ════════════════════════════════════════════════════════════════
;; Dot syntax (rest patterns) — from introduction.md
;; ════════════════════════════════════════════════════════════════

(deftest doc-dot-syntax-vector-rest
  (is (= [[3 4 5]]
         (m/query (let [[a b . xs] [1 2 3 4 5]] xs))))
  (is (= [[10 [20 30]]]
         (m/query (let [[a . xs] [10 20 30]] [a xs]))))
  (is (= [[1 2]]
         (m/query (let [[a b .] [1 2 3 4]] [a b]))))
  (is (= [[[3 4 5] [1 2 3 4 5]]]
         (m/query (let [(as v [a b . xs]) [1 2 3 4 5]] [xs v]))))
  (is (= [[3 4]]
         (m/query (let [[a b . [c d]] [1 2 3 4]] [c d])))))

(deftest doc-dot-syntax-head-middle-tail
  (is (= [[1 [2 3 4] 5]]
         (m/query (let [[a . mid c] [1 2 3 4 5]] [a mid c]))))
  (is (= [[1 2 [3 4 5] 6 7]]
         (m/query (let [[a b . mid x y] [1 2 3 4 5 6 7]] [a b mid x y]))))
  (is (= [[1 [] 2]]
         (m/query (let [[a . mid c] [1 2]] [a mid c]))))
  (is (= [[1 [2 3] 4]]
         (m/query (let [[a . mid {:x x}] [1 2 3 {:x 4}]] [a mid x])))))

(deftest doc-dot-syntax-map
  (is (= [[1 2]]
         (m/query (let [{:x a :y b} {:x 1 :y 2}] [a b]))))
  (is (= [[1 2 3]]
         (m/query (let [{:x a :y [b c]} {:x 1 :y [2 3]}] [a b c]))))
  (is (= [{:y 2 :z 3}]
         (m/query (let [{:x a . rest} {:x 1 :y 2 :z 3}] rest))))
  (is (= [[1 2 3]]
         (m/query (let [{:x a . (ks y z)} {:x 1 :y 2 :z 3}] [a y z]))))
  (is (= [[{:y 2 :z 3} {:x 1 :y 2 :z 3}]]
         (m/query (let [(as m {:x a . rest}) {:x 1 :y 2 :z 3}] [rest m])))))

;; ════════════════════════════════════════════════════════════════
;; Constraint functions (defn) — multi-body query
;; ════════════════════════════════════════════════════════════════

(deftest doc-defn
  (is (= [1 2 3]
         (m/query (defn positive [x] (> x 0))
                  (let [n (one-of -1 0 1 2 3)]
                    (positive n)
                    n))))
  (is (= #{[3 5] [4 4] [5 3]}
         (set (m/query (defn sums-to [a b target]
                         (= (+ a b) target))
                       (let [x (between 1 5)
                             y (between 1 5)]
                         (sums-to x y 8)
                         [x y])))))
  (is (= [[1 2] [1 3] [2 3]]
         (m/query (defn positive [x] (> x 0))
                  (defn ordered [a b] (< a b))
                  (let [x (one-of -1 0 1 2 3)
                        y (one-of -1 0 1 2 3)]
                    (positive x)
                    (positive y)
                    (ordered x y)
                    [x y])))))

;; ════════════════════════════════════════════════════════════════
;; Domains (def) — multi-body query
;; ════════════════════════════════════════════════════════════════

(deftest doc-def
  (is (= [10 25]
         (m/query (def person {:name string :age (between 0 150)})
                  (let [p {:name "Alice" :age (one-of 10 25 200)}]
                    (narrow p person)
                    (:age p)))))
  (is (= [30]
         (m/query (def person {:name string :age (between 0 150)})
                  (def employee (and person {:company string}))
                  (let [e {:name "Alice" :age (one-of 30 200) :company "Acme"}]
                    (narrow e employee)
                    (:age e)))))
  (is (= [["Bob" 25]]
         (m/query (do (defn person [name age] {:name (string name) :age (integer age)})
                      (let [(person name age) {:name "Bob" :age 25}]
                        [name age])))))
  (is (= [25 30]
         (m/query (def person {:name string :age (between 0 150)})
                  (defn adult [p] (>= (:age p) 18))
                  (let [p {:name "Alice" :age (one-of 10 25 30)}]
                    (narrow p person)
                    (adult p)
                    (:age p))))))

;; ════════════════════════════════════════════════════════════════
;; Higher-order functions (enriched examples)
;; ════════════════════════════════════════════════════════════════

(deftest doc-hofs-basic
  (is (= [[2 4 6]]
         (m/query (let [v [1 2 3]
                        doubled (map (fn [x] (* x 2)) v)]
                    doubled))))
  (is (= [[2 4]]
         (m/query (let [v [1 2 3 4 5]
                        evens (filter even v)]
                    evens))))
  (is (= [15]
         (m/query (let [v [1 2 3 4 5]
                        total (reduce + 0 v)]
                    total)))))

(deftest doc-hofs-non-ground
  (testing "map over uncertain elements with constraint"
    (is (= [[4 8] [4 10] [4 12] [6 8] [6 10] [6 12]]
           (m/query (let [v [(one-of 1 2 3) (one-of 4 5 6)]
                          doubled (map (fn [x] (* x 2)) v)]
                      (and (> (nth doubled 0) 3)
                           doubled)))))))

(deftest doc-hofs-filter-with-defn
  (testing "filter with custom constraint function"
    (is (= [[4 5]]
           (m/query (do (defn big [x] (> x 3))
                        (let [v [1 2 3 4 5]
                              bigs (filter big v)]
                          bigs)))))))

(deftest doc-hofs-reduce-constraint
  (testing "reduce + constraint on total"
    (is (= #{[[1 6 7] 14] [[5 2 7] 14] [[5 6 3] 14] [[5 6 7] 18]}
           (set (m/query (let [v [(one-of 1 5) (one-of 2 6) (one-of 3 7)]
                               total (reduce + 0 v)]
                           (and (> total 10)
                                [v total]))))))))

(deftest doc-hofs-chain
  (is (= [12]
         (m/query (let [v [1 2 3]
                        doubled (map (fn [x] (* x 2)) v)
                        total (reduce + 0 doubled)]
                    total)))))

;; ════════════════════════════════════════════════════════════════
;; Pythagorean triples
;; ════════════════════════════════════════════════════════════════

(deftest doc-pythagorean
  (is (= #{[3 4 5] [5 12 13] [6 8 10] [9 12 15]}
         (set (m/query (let [a (between 1 15)
                              b (between 1 15)
                              c (between 1 15)]
                          (and (< a b)
                               (<= b c)
                               (= (+ (* a a) (* b b)) (* c c))
                               [a b c])))))))
