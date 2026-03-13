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
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) #"Contradiction"
                          (m/query (let [x (one-of 1 2 3)]
                                     (and (> x 5) x))))))

  (testing "contradictory equality"
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) #"Contradiction"
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

;; ════════════════════════════════════════════════════════════════
;; Fn edge cases (from edge_cases_test section 6)
;; ════════════════════════════════════════════════════════════════

(deftest fn-multiple-calls-different-args
  (testing "same function called with different args"
    (is (= #{4 6}
           (set (m/query (let [double (fn [x] (+ x x))
                               a (one-of 2 3)]
                           (double a))))))))

(deftest fn-returning-vector
  (testing "function returning a vector"
    (is (= [[1 2]] (m/query (let [pair (fn [a b] [a b])]
                              (pair 1 2)))))))

(deftest fn-wrong-arity
  (testing "function called with too few args"
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo)
                 (m/query (let [f (fn [x y] (+ x y))]
                            (f 1))))))
  (testing "function called with too many args"
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo)
                 (m/query (let [f (fn [x] (+ x 1))]
                            (f 1 2 3)))))))

(deftest fn-higher-order
  (testing "higher-order function application works"
    ;; (fn [f] (f 5)) — f resolves to double via link chain,
    ;; and :mufl-fn is found on the resolved target
    (is (= [10]
           (m/query (let [apply-to-5 (fn [f] (f 5))
                          double (fn [x] (+ x x))]
                      (apply-to-5 double)))))))

(deftest fn-with-closure
  (testing "fn can reference variables from enclosing scope"
    (is (= [11] (m/query (let [base 10
                               add-base (fn [x] (+ x base))]
                           (add-base 1)))))))

(deftest fn-two-independent-calls
  (testing "two calls to same fn with different args, both results used"
    (is (= [[4 6]] (m/query (let [double (fn [x] (+ x x))
                                  a (double 2)
                                  b (double 3)]
                              [a b]))))))

;; ════════════════════════════════════════════════════════════════
;; Combined / stress tests (from edge_cases_test section 9)
;; NOTE: abs-of-zero duplicate removed — kept in narrow_test as abs-of-zero-narrow
;; ════════════════════════════════════════════════════════════════

(deftest send-more-money-style
  (testing "small crypto-arithmetic: A + B = C where all different, all in 1..5"
    (let [results (m/query (let [a (between 1 5)
                                 b (between 1 5)
                                 c (between 1 5)]
                             (and (= (+ a b) c)
                                  (distinct [a b c])
                                  (< a b)
                                  [a b c])))]
      (is (= #{[1 2 3] [1 3 4] [1 4 5] [2 3 5]} (set results))))))

(deftest deep-search-terminates
  (testing "5 variables with constraints — search terminates"
    (let [results (m/query (let [a (one-of 1 2)
                                 b (one-of 1 2)
                                 c (one-of 1 2)
                                 d (one-of 1 2)
                                 e (one-of 1 2)]
                             (and (= (+ a b) (+ c d))
                                  (!= a e)
                                  [a b c d e])))]
      (is (pos? (count results))))))

(deftest map-return
  (testing "map literal return"
    (is (= [{:a 1 :b 2}]
           (m/query {:a 1 :b 2})))))

(deftest vector-return
  (testing "vector literal return"
    (is (= [[1 2 3]]
           (m/query [1 2 3])))))

(deftest even-odd-on-already-narrow
  (testing "even on a singleton"
    (is (= [2] (m/query (let [x (one-of 2)] (and (even x) x))))))
  (testing "even contradiction on odd singleton"
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo)
                 (m/query (let [x (one-of 1)] (and (even x) x)))))))

;; ════════════════════════════════════════════════════════════════
;; Mixed operations (from edge_cases_test section 24)
;; ════════════════════════════════════════════════════════════════

(deftest constraint-chain-plus-minus
  (testing "chaining + and - constraints"
    ;; a=1, s=a+b=4 → b=3, d=s-a=4-1=3
    (is (= #{[1 3 4 3]}
           (set (m/query (let [a (between 1 5)
                               b (between 1 5)
                               s (+ a b)
                               d (- s a)]
                           (and (= s 4) (= a 1)
                                [a b s d]))))))))

(deftest multiple-constraints-same-var
  (testing "multiple constraints all narrow the same variable"
    (is (= [5]
           (m/query (let [x (between 1 10)]
                      (and (> x 3) (< x 7) (odd x) x)))))))

(deftest nested-fn-call-with-constraint
  (testing "fn call result used in constraint"
    (is (= [3]
           (m/query (let [double (fn [x] (+ x x))
                          n (between 1 5)]
                      (and (= (double n) 6) n)))))))

;; ════════════════════════════════════════════════════════════════
;; Performance / scale tests (from edge_cases_test section 25)
;; ════════════════════════════════════════════════════════════════

(deftest large-domain-with-tight-constraint
  (testing "between 1..500 narrowed to 1 value is fast"
    (let [t0 #?(:clj (System/nanoTime) :cljs (system-time))
          results (m/query (let [x (between 1 500)]
                             (and (= x 42) x)))
          elapsed-ms #?(:clj (/ (- (System/nanoTime) t0) 1e6)
                        :cljs (- (system-time) t0))]
      (is (= [42] results))
      (is (< elapsed-ms 500) "Should complete in under 500ms"))))

(deftest four-queens-solvable
  (testing "4-queens is solvable with our constraint system"
    (let [results (m/query (let [q1 (between 1 4)
                                 q2 (between 1 4)
                                 q3 (between 1 4)
                                 q4 (between 1 4)]
                             (and (distinct [q1 q2 q3 q4])
                                  ;; No diagonal attacks
                                  (!= (abs (- q1 q2)) 1)
                                  (!= (abs (- q1 q3)) 2)
                                  (!= (abs (- q1 q4)) 3)
                                  (!= (abs (- q2 q3)) 1)
                                  (!= (abs (- q2 q4)) 2)
                                  (!= (abs (- q3 q4)) 1)
                                  [q1 q2 q3 q4])))]
      (is (= #{[2 4 1 3] [3 1 4 2]} (set results))))))

;; ════════════════════════════════════════════════════════════════
;; Single value domain operations (from edge_cases_test section 26)
;; ════════════════════════════════════════════════════════════════

(deftest single-value-arithmetic
  (testing "arithmetic on single-value domains"
    (is (= [7] (m/query (+ 3 4))))
    (is (= [1] (m/query (- 5 4))))
    (is (= [12] (m/query (* 3 4))))))

(deftest equality-of-same-var
  (testing "(= x x) is trivially true"
    (is (= #{1 2 3}
           (set (m/query (let [x (one-of 1 2 3)]
                           (and (= x x) x))))))))

(deftest neq-of-same-var
  (testing "(!= x x) is contradiction"
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo)
                 (m/query (let [x (one-of 1)]
                            (and (!= x x) x)))))))

;; ════════════════════════════════════════════════════════════════
;; Fn application (from phase2_test)
;; ════════════════════════════════════════════════════════════════

(deftest fn-application
  (testing "fn with constraint propagation — double"
    (is (= [3]
           (m/query (let [double (fn [x] (+ x x))
                          n (between 1 5)]
                      (and (= (double n) 6) n))))))
  (testing "fn with constraint propagation — square"
    (is (= [5]
           (m/query (let [sq (fn [x] (* x x))
                          n (between 1 10)]
                      (and (= (sq n) 25) n)))))))

;; ════════════════════════════════════════════════════════════════
;; FizzBuzz (from phase2_test)
;; ════════════════════════════════════════════════════════════════

(deftest fizzbuzz
  (testing "multiples of 3 or 5 under 20"
    (is (= #{3 5 6 9 10 12 15 18 20}
           (set (m/query (let [x (between 1 20)]
                           (and (or (= (mod x 3) 0)
                                    (= (mod x 5) 0))
                                x))))))))

;; ════════════════════════════════════════════════════════════════
;; Pythagorean triples (from phase2_test — renamed to avoid collision)
;; ════════════════════════════════════════════════════════════════

(deftest pythagorean-triples-phase2
  (testing "a² + b² = c² for a,b,c ≤ 15"
    (is (= #{[3 4 5] [6 8 10] [5 12 13] [9 12 15]}
           (set (m/query (let [a (between 1 15)
                               b (between 1 15)
                               c (between 1 15)]
                           (and (< a b)
                                (<= b c)
                                (= (+ (* a a) (* b b)) (* c c))
                                [a b c]))))))))

;; ════════════════════════════════════════════════════════════════
;; Implicit do in query (from new_features_test)
;; ════════════════════════════════════════════════════════════════

(deftest implicit-do-in-query
  (testing "single body form still works"
    (is (= [42] (m/query 42))))

  (testing "multi-body query with defn"
    (is (= [1 2 3]
           (m/query (defn positive [x] (> x 0))
                    (let [n (one-of -1 0 1 2 3)]
                      (positive n)
                      n)))))

  (testing "multi-body query with def"
    (is (= [10 25]
           (m/query (def person {:name string :age (between 0 150)})
                    (let [p {:name "Alice" :age (one-of 10 25 200)}]
                      (narrow p person)
                      (:age p))))))

  (testing "multi-body query with multiple defs"
    (is (= [25 30]
           (m/query (def person {:name string :age (between 0 150)})
                    (defn adult [p] (>= (:age p) 18))
                    (let [p {:name "Alice" :age (one-of 10 25 30)}]
                      (narrow p person)
                      (adult p)
                      (:age p)))))))
