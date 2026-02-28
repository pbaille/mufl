(ns mufl.doc-verify
  "Verify every code example from introduction.md"
  (:require [clojure.test :refer [deftest testing is]]
            [mufl.core :as m]))

;; ═══════════════════════════════════════════════════════════
;; Values
;; ═══════════════════════════════════════════════════════════

(deftest values
  (is (= [42]      (m/query 42)))
  (is (= ["hello"] (m/query "hello")))
  (is (= [:ok]     (m/query :ok)))
  (is (= [true]    (m/query true))))

;; ═══════════════════════════════════════════════════════════
;; Uncertain values
;; ═══════════════════════════════════════════════════════════

(deftest uncertain-values
  (is (= [1 2 3]     (m/query (let [x (one-of 1 2 3)] x))))
  (is (= [1 2 3 4 5] (m/query (let [x (between 1 5)] x)))))

;; ═══════════════════════════════════════════════════════════
;; Constraints
;; ═══════════════════════════════════════════════════════════

(deftest constraints
  (is (= [3 4 5]
         (m/query (let [x (one-of 1 2 3 4 5)]
                    (and (> x 2) x)))))
  (is (= [2 4]
         (m/query (let [x (one-of 1 2 3 4 5)]
                    (and (> x 1)
                         (< x 5)
                         (!= x 3)
                         x))))))

;; ═══════════════════════════════════════════════════════════
;; Multiple variables
;; ═══════════════════════════════════════════════════════════

(deftest multiple-variables
  (is (= [[2 4] [1 5]]
         (m/query (let [x (one-of 1 2 3 4 5)
                        y (one-of 1 2 3 4 5)]
                    (and (< x y)
                         (= (+ x y) 6)
                         [x y]))))))

;; ═══════════════════════════════════════════════════════════
;; Arithmetic
;; ═══════════════════════════════════════════════════════════

(deftest arithmetic
  (is (= [11 12 13]
         (m/query (let [x (one-of 1 2 3)
                        s (+ x 10)]
                    s))))
  (is (= [[2 4 6] [1 5 6]]
         (m/query (let [x (one-of 1 2 3 4 5)
                        y (one-of 1 2 3 4 5)
                        s (+ x y)]
                    (and (< x y)
                         (= s 6)
                         [x y s])))))
  (testing "mod"
    (is (= [3 6 9 12 15 18]
           (m/query (let [x (between 1 20)]
                      (and (= (mod x 3) 0) x))))))
  (testing "abs"
    (is (= [5] (m/query (abs -5))))
    (is (= [0 1 2 3]
           (m/query (let [x (one-of -3 -2 -1 0 1 2 3)]
                      (abs x)))))
    (is (= [-2 2]
           (m/query (let [x (one-of -3 -2 -1 0 1 2 3)]
                      (and (= (abs x) 2) x))))))
  (testing "min/max"
    (is (= [2] (m/query (min 3 2))))
    (is (= [5] (m/query (max 3 5))))))

;; ═══════════════════════════════════════════════════════════
;; Distinct
;; ═══════════════════════════════════════════════════════════

(deftest distinct-test
  (is (= [[3 2 1] [2 3 1] [3 1 2] [1 3 2] [2 1 3] [1 2 3]]
         (m/query (let [a (one-of 1 2 3)
                        b (one-of 1 2 3)
                        c (one-of 1 2 3)]
                    (and (distinct [a b c])
                         [a b c]))))))

;; ═══════════════════════════════════════════════════════════
;; Branching
;; ═══════════════════════════════════════════════════════════

(deftest branching
  (is (= [1 2 3 4 10]
         (m/query (let [x (between 1 10)]
                    (if (< x 5) x 10)))))
  (is (= [1 2 3 8 9 10 0]
         (m/query (let [x (between 1 10)]
                    (cond
                      (< x 4) x
                      (> x 7) x
                      :else 0))))))

;; ═══════════════════════════════════════════════════════════
;; Disjunction
;; ═══════════════════════════════════════════════════════════

(deftest disjunction
  (is (= [3 7]
         (m/query (let [x (between 1 10)]
                    (and (or (= x 3) (= x 7)) x)))))
  (is (= [1 2 8 9 10]
         (m/query (let [x (between 1 10)]
                    (and (or (< x 3) (> x 7)) x)))))
  (testing "not"
    (is (= [3 4 5]
           (m/query (let [x (one-of 1 2 3 4 5)]
                      (and (not (< x 3)) x)))))))

;; ═══════════════════════════════════════════════════════════
;; Predicates
;; ═══════════════════════════════════════════════════════════

(deftest predicates
  (is (= [2 4 6 8 10]
         (m/query (let [x (between 1 10)]
                    (and (even x) x)))))
  (is (= [1 2 3]
         (m/query (let [x (one-of -3 -2 -1 0 1 2 3)]
                    (and (pos x) x)))))
  (testing "type domain as constraint"
    (is (= [1 2]
           (m/query (let [x (one-of 1 2 "hello" "world" :ok)]
                      (and (number x) x))))))
  (testing "even AND positive"
    (is (= [2 4]
           (m/query (let [x (one-of -4 -2 0 1 2 3 4)]
                      (and (even x) (pos x) x))))))
  (testing "even OR negative"
    (is (= [-3 -1 2 4]
           (m/query (let [x (one-of -3 -1 1 2 3 4)]
                      (and (or (even x) (neg x)) x)))))))

;; ═══════════════════════════════════════════════════════════
;; Functions
;; ═══════════════════════════════════════════════════════════

(deftest functions
  (is (= [3]
         (m/query (let [double (fn [x] (+ x x))
                        n (between 1 5)]
                    (and (= (double n) 6) n))))))

;; ═══════════════════════════════════════════════════════════
;; Recursion
;; ═══════════════════════════════════════════════════════════

(deftest recursion
  (is (= [120]
         (m/query (let [fact (fn [n]
                               (if (= n 0) 1 (* n (fact (- n 1)))))]
                    (fact 5)))))
  (is (= [55]
         (m/query (let [fib (fn [n]
                              (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))]
                    (fib 10)))))
  (testing "closure + recursion"
    (is (= [32]
           (m/query (let [base 2
                          pow (fn [n] (if (= n 0) 1 (* base (pow (- n 1)))))]
                      (pow 5)))))))

;; ═══════════════════════════════════════════════════════════
;; Collections
;; ═══════════════════════════════════════════════════════════

(deftest collections
  (is (= [20] (m/query (let [v [10 20 30]] (nth v 1)))))
  (is (= [1]  (m/query (let [m {:x 1 :y 2}] (get m :x)))))
  (is (= [1]  (m/query (let [m {:x 1 :y 2}] (:x m)))))
  (testing "constraint propagation"
    (is (= [{:x 2 :y 10} {:x 3 :y 10}]
           (m/query (let [m {:x (one-of 1 2 3) :y 10}]
                      (and (> (:x m) 1) m))))))
  (testing "nested"
    (is (= [42] (m/query (let [m {:a {:b 42}}]
                            (:b (:a m)))))))
  (testing "count"
    (is (= [3] (m/query (let [v [10 20 30]] (count v)))))
    (is (= [2] (m/query (let [m {:x 1 :y 2}] (count m)))))
    (is (= [3] (m/query (let [[a b . xs] [1 2 3 4 5]] (count xs))))))
  (testing "drop"
    (is (= [[3 4 5]] (m/query (let [v [1 2 3 4 5]] (drop 2 v))))))
  (testing "dissoc"
    (is (= [{:y 2 :z 3}] (m/query (let [m {:x 1 :y 2 :z 3}] (dissoc m :x)))))
    (is (= [{:z 3}]      (m/query (let [m {:x 1 :y 2 :z 3}] (dissoc m :x :y)))))))

;; ═══════════════════════════════════════════════════════════
;; Destructuring
;; ═══════════════════════════════════════════════════════════

(deftest destructuring
  (testing "map destructuring"
    (is (= [[1 2] [1 3] [2 3]]
           (m/query (let [{:x x :y y} {:x (one-of 1 2 3) :y (one-of 1 2 3)}]
                      (and (< x y) [x y]))))))
  (testing "vector destructuring"
    (is (= [30]
           (m/query (let [[a b c] [10 20 30]] (+ a b))))))
  (testing "as with vector"
    (is (= [[2 10] [3 10]]
           (m/query (let [(as v [a b]) [(one-of 1 2 3) 10]]
                      (and (> a 1) v))))))
  (testing "as with map"
    (is (= [[{:x 1 :y 2} 1]]
           (m/query (let [(as m {:x x}) {:x 1 :y 2}] [m x])))))
  (testing "ks"
    (is (= [3]
           (m/query (let [(ks x y) {:x 1 :y 2}] (+ x y))))))
  (testing "as + ks"
    (is (= [[{:x 1 :y 2} 1]]
           (m/query (let [(as m (ks x y)) {:x 1 :y 2}] [m x])))))
  (testing "or pattern"
    (is (= [42]
           (m/query (let [(or (ks x) [x]) {:x 42}] x)))))
  (testing "nested ks in vector"
    (is (= [1]
           (m/query (let [[(ks x) b] [{:x 1} 20]] x)))))
  (testing "as with ks"
    (is (= [{:x 1 :y 2}]
           (m/query (let [(as whole (ks x y)) {:x 1 :y 2}] whole))))))

;; ═══════════════════════════════════════════════════════════
;; Dot syntax (rest patterns)
;; ═══════════════════════════════════════════════════════════

(deftest dot-syntax
  (testing "vector rest"
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
  (testing "head/middle/tail"
    (is (= [[1 [2 3 4] 5]]
           (m/query (let [[a . mid c] [1 2 3 4 5]] [a mid c]))))
    (is (= [[1 2 [3 4 5] 6 7]]
           (m/query (let [[a b . mid x y] [1 2 3 4 5 6 7]] [a b mid x y]))))
    (is (= [[1 [] 2]]
           (m/query (let [[a . mid c] [1 2]] [a mid c]))))
    (is (= [[1 [2 3] 4]]
           (m/query (let [[a . mid {:x x}] [1 2 3 {:x 4}]] [a mid x]))))))

;; ═══════════════════════════════════════════════════════════
;; Map rest
;; ═══════════════════════════════════════════════════════════

(deftest map-rest
  (is (= [{:y 2 :z 3}]
         (m/query (let [{:x a . rest} {:x 1 :y 2 :z 3}] rest))))
  (is (= [[1 2 3]]
         (m/query (let [{:x a . (ks y z)} {:x 1 :y 2 :z 3}] [a y z]))))
  (is (= [[{:y 2 :z 3} {:x 1 :y 2 :z 3}]]
         (m/query (let [(as m {:x a . rest}) {:x 1 :y 2 :z 3}] [rest m])))))

;; ═══════════════════════════════════════════════════════════
;; Named constraints (defn)
;; ═══════════════════════════════════════════════════════════

(deftest named-constraints
  (is (= [1 2 3]
         (m/query (defn positive [x] (> x 0))
                  (let [n (one-of -1 0 1 2 3)]
                    (positive n)
                    n))))
  (testing "multi-param"
    (is (= [[5 3] [4 4] [3 5]]
           (m/query (defn sums-to [a b target]
                      (= (+ a b) target))
                    (let [x (between 1 5)
                          y (between 1 5)]
                      (sums-to x y 8)
                      [x y])))))
  (testing "composition"
    (is (= [[1 2] [1 3] [2 3]]
           (m/query (defn positive [x] (> x 0))
                    (defn ordered [a b] (< a b))
                    (let [x (one-of -1 0 1 2 3)
                          y (one-of -1 0 1 2 3)]
                      (positive x)
                      (positive y)
                      (ordered x y)
                      [x y]))))))

;; ═══════════════════════════════════════════════════════════
;; Domains (def and narrow)
;; ═══════════════════════════════════════════════════════════

(deftest domains
  (is (= [10 25]
         (m/query (def person {:name string :age (between 0 150)})
                  (let [p {:name "Alice" :age (one-of 10 25 200)}]
                    (narrow p person)
                    (:age p)))))
  (testing "scalar narrow"
    (is (= [1]
           (m/query (let [x (one-of 1 "hello" :foo)]
                      (narrow x integer)
                      x)))))
  (testing "and composition"
    (is (= [30]
           (m/query (def person {:name string :age (between 0 150)})
                    (def employee (and person {:company string}))
                    (let [e {:name "Alice" :age (one-of 30 200) :company "Acme"}]
                      (narrow e employee)
                      (:age e))))))
  (testing "vector tuple schema"
    (is (= [[3 4]]
           (m/query (def point [integer integer])
                    (let [p [(one-of 3 "x") (one-of 4 "y")]]
                      (narrow p point)
                      p)))))
  (testing "schema + constraint function"
    (is (= [25 30]
           (m/query (def person {:name string :age (between 0 150)})
                    (defn adult [p] (>= (:age p) 18))
                    (let [p {:name "Alice" :age (one-of 10 25 30)}]
                      (narrow p person)
                      (adult p)
                      (:age p)))))))

;; ═══════════════════════════════════════════════════════════
;; Type constructors
;; ═══════════════════════════════════════════════════════════

(deftest type-constructors
  (testing "vector-of"
    (is (= [[1 2 3]]
           (m/query (let [v [(one-of 1 "a") (one-of 2 "b") (one-of 3 "c")]]
                      (narrow v (vector-of integer))
                      v))))
    (is (= [[{:name "Alice"} {:name "Bob"}]]
           (m/query (def named {:name string})
                    (let [people [{:name (one-of "Alice" 42)} {:name (one-of "Bob" 99)}]]
                      (narrow people (vector-of named))
                      people))))
    (is (= [[1 2]]
           (m/query (def int-vec (vector-of integer))
                    (let [v [(one-of 1 "x") (one-of 2 "y")]]
                      (narrow v int-vec)
                      v)))))
  (testing "tuple"
    (is (= [[42 "hello" true]]
           (m/query (let [v [(one-of 42 "x") (one-of "hello" 99) (one-of true 0)]]
                      (narrow v (tuple [integer string boolean]))
                      v))))
    (is (= [[3 4]]
           (m/query (def point (tuple [integer integer]))
                    (let [p [(one-of 3 "x") (one-of 4 "y")]]
                      (narrow p point)
                      p)))))
  (testing "map-of"
    (is (= [{:a 1 :b 2}]
           (m/query (let [m {:a (one-of 1 "x") :b (one-of 2 "y")}]
                      (narrow m (map-of keyword integer))
                      m))))
    (is (= [{:x 10 :y 20}]
           (m/query (def scores (map-of keyword integer))
                    (let [s {:x (one-of 10 "a") :y (one-of 20 "b")}]
                      (narrow s scores)
                      s)))))
  (testing "nesting"
    (is (= [["Alice" [90 80]]]
           (m/query (def student (and {:name string}
                                      {:scores (vector-of integer)}))
                    (let [s {:name "Alice" :scores [(one-of 90 "x") (one-of 80 "y")]}]
                      (narrow s student)
                      [(get s :name) (get s :scores)]))))))

;; ═══════════════════════════════════════════════════════════
;; Higher-order functions
;; ═══════════════════════════════════════════════════════════

(deftest higher-order-functions
  (testing "map"
    (is (= [[2 4 6]]
           (m/query (let [v [1 2 3]
                          doubled (map (fn [x] (* x 2)) v)]
                      doubled)))))
  (testing "filter"
    (is (= [[2 4]]
           (m/query (let [v [1 2 3 4 5]
                          evens (filter even v)]
                      evens)))))
  (testing "reduce"
    (is (= [15]
           (m/query (let [v [1 2 3 4 5]
                          total (reduce + 0 v)]
                      total)))))
  (testing "map over uncertain"
    (is (= [[4 8] [4 10] [4 12] [6 8] [6 10] [6 12]]
           (m/query (let [v [(one-of 1 2 3) (one-of 4 5 6)]
                          doubled (map (fn [x] (* x 2)) v)]
                      (and (> (nth doubled 0) 3)
                           doubled))))))
  (testing "filter with defn"
    (is (= [[4 5]]
           (m/query (do (defn big [x] (> x 3))
                        (let [v [1 2 3 4 5]
                              bigs (filter big v)]
                          bigs))))))
  (testing "reduce with constraint"
    (is (= (set [[[5 2 7] 14] [[5 6 3] 14] [[1 6 7] 14] [[5 6 7] 18]])
           (set (m/query (let [v [(one-of 1 5) (one-of 2 6) (one-of 3 7)]
                               total (reduce + 0 v)]
                           (and (> total 10)
                                [v total])))))))
  (testing "chaining"
    (is (= [12]
           (m/query (let [v [1 2 3]
                          doubled (map (fn [x] (* x 2)) v)
                          total (reduce + 0 doubled)]
                      total))))))

;; ═══════════════════════════════════════════════════════════
;; Pythagorean triples
;; ═══════════════════════════════════════════════════════════

(deftest pythagorean-triples
  (is (= [[3 4 5] [6 8 10] [5 12 13] [9 12 15]]
         (m/query (let [a (between 1 15)
                        b (between 1 15)
                        c (between 1 15)]
                    (and (< a b)
                         (<= b c)
                         (= (+ (* a a) (* b b)) (* c c))
                         [a b c]))))))

;; ═══════════════════════════════════════════════════════════
;; query+
;; ═══════════════════════════════════════════════════════════

(deftest query-plus
  (is (= ['{x 1, y 2} '{x 1, y 3} '{x 2, y 3}]
         (m/query+ (let [x (one-of 1 2 3)
                         y (one-of 1 2 3)]
                     (and (< x y) [x y])))))
  (testing "with defn"
    (is (= ['{n 1} '{n 2} '{n 3}]
           (m/query+ (defn positive [x] (> x 0))
                     (let [n (one-of -2 -1 0 1 2 3)]
                       (positive n)
                       n))))))
