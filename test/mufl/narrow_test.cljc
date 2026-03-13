(ns mufl.narrow-test
  (:require [clojure.test :refer [deftest testing is]]
            [mufl.core :as m]))

;; ════════════════════════════════════════════════════════════════
;; narrow primitive tests
;; ════════════════════════════════════════════════════════════════

(deftest narrow-scalar-type
  (testing "narrow to integer type"
    (is (= [42]
           (m/query (do (let [x 42]
                          (narrow x integer)
                          x))))))

  (testing "narrow to string type"
    (is (= ["hello"]
           (m/query (do (let [x "hello"]
                          (narrow x string)
                          x))))))

  (testing "narrow eliminates incompatible type values"
    (is (= [42]
           (m/query (do (let [x (one-of 42 "hello")]
                          (narrow x integer)
                          x)))))))

(deftest narrow-scalar-range
  (testing "narrow to between range"
    (is (= [5]
           (m/query (do (let [x 5]
                          (narrow x (between 1 10))
                          x))))))

  (testing "narrow range eliminates out-of-range values"
    (is (= [3]
           (m/query (do (let [x (one-of 3 15)]
                          (narrow x (between 1 10))
                          x)))))))

(deftest narrow-map-schema
  (testing "narrow map against structural schema"
    (is (= ["Alice"]
           (m/query (do (let [p {:name "Alice" :age 30}]
                          (narrow p {:name string :age integer})
                          (get p :name)))))))

  (testing "narrow map eliminates out-of-domain values"
    (is (= [10]
           (m/query (do (let [p {:name "Alice" :age (one-of 10 200)}]
                          (narrow p {:name string :age (between 0 150)})
                          (get p :age)))))))

  (testing "narrow map eliminates incompatible type in field"
    (is (= ["Bob"]
           (m/query (do (let [p {:name (one-of "Bob" 42)}]
                          (narrow p {:name string})
                          (get p :name))))))))

(deftest narrow-vector-tuple
  (testing "narrow vector as tuple"
    (is (= [[1 2]]
           (m/query (do (let [v [1 2]]
                          (narrow v [integer integer])
                          v))))))

  (testing "narrow tuple eliminates incompatible values"
    (is (= [[1 "hello"]]
           (m/query (do (let [v [(one-of 1 "x") (one-of "hello" 42)]]
                          (narrow v [integer string])
                          v)))))))

(deftest narrow-composite-domain
  (testing "narrow with vector-of"
    (is (= [[1 2 3]]
           (m/query (do (let [v [1 2 3]]
                          (narrow v (vector-of integer))
                          v))))))

  (testing "narrow with vector-of eliminates non-integer elements"
    (is (= [[1]]
           (m/query (do (let [v [(one-of 1 "x")]]
                          (narrow v (vector-of integer))
                          v)))))))

(deftest narrow-named-template
  (testing "narrow against def-bound domain schema"
    (is (= ["Alice"]
           (m/query (do (def person {:name string :age (between 0 150)})
                        (let [p {:name "Alice" :age 30}]
                          (narrow p person)
                          (get p :name)))))))

  (testing "narrow against def-bound scalar domain"
    (is (= [5]
           (m/query (do (def small-int (between 1 10))
                        (let [x 5]
                          (narrow x small-int)
                          x))))))

  (testing "narrow against def-bound domain eliminates values"
    (is (= [10]
           (m/query (do (def person {:name string :age (between 0 150)})
                        (let [p {:name "Alice" :age (one-of 10 200)}]
                          (narrow p person)
                          (get p :age))))))))

(deftest narrow-and-composition
  (testing "narrow with and-composed schemas"
    (is (= ["Alice"]
           (m/query (do (def person {:name string :age integer})
                        (def employee (and person {:company string}))
                        (let [e {:name "Alice" :age 30 :company "ACME"}]
                          (narrow e employee)
                          (get e :name)))))))

  (testing "narrow with and applies all sub-constraints"
    (is (= ["ACME"]
           (m/query (do (def has-name {:name string})
                        (def has-age {:age integer})
                        (def person (and has-name has-age))
                        (let [p {:name "Alice" :age 30 :company "ACME"}]
                          (narrow p person)
                          (get p :company))))))))

(deftest narrow-contradiction
  (testing "narrow with contradictory constraint throws"
    (is (thrown? #?(:clj Exception :cljs js/Error)
                (m/query (do (let [p {:name 42 :age 30}]
                              (narrow p {:name string :age integer})
                              (get p :name)))))))

  (testing "narrow named domain with contradictory value throws"
    (is (thrown? #?(:clj Exception :cljs js/Error)
                (m/query (do (def person {:name string :age (between 0 150)})
                             (let [p {:name 42 :age 30}]
                               (narrow p person)
                               (get p :name))))))))

(deftest narrow-one-of
  (testing "narrow with one-of domain"
    (is (= ["admin"]
           (m/query (do (let [x "admin"]
                          (narrow x (one-of "admin" "user" "guest"))
                          x))))))

  (testing "narrow one-of field in map"
    (is (= ["admin" "user"]
           (m/query (do (def account {:role (one-of "admin" "user" "guest")})
                        (let [a {:role (one-of "admin" "user")}]
                          (narrow a account)
                          (get a :role)))))))

  (testing "narrow with one-of eliminates invalid values"
    (is (= ["admin"]
           (m/query (do (let [x (one-of "admin" "banned")]
                          (narrow x (one-of "admin" "user" "guest"))
                          x)))))))

(deftest narrow-finite-enumeration
  (testing "narrow def-bound small domain enumerates solutions"
    (is (= #{1 2 3}
           (set (m/query (do (def small-int (between 1 3))
                             (let [x (one-of 1 2 3 4 5)]
                               (narrow x small-int)
                               x))))))))

;; ════════════════════════════════════════════════════════════════
;; Fix #4: structural = on map nodes
;; ════════════════════════════════════════════════════════════════

(deftest narrow-structural-eq
  (testing "same-key equality propagates constrained value"
    (is (= [2]
           (m/query (do (let [a {:x (one-of 1 2 3)}
                              b {:x 2}]
                          (= a b)
                          (get a :x)))))))

  (testing "nested map equality narrows recursively"
    (is (= [10]
           (m/query (do (let [a {:p {:q (one-of 10 20)}}
                              b {:p {:q 10}}]
                          (= a b)
                          (get (get a :p) :q)))))))

  (testing "mismatched key sets produce contradiction"
    (is (thrown? #?(:clj Exception :cljs js/Error)
           (m/query (do (let [a {:x 1 :y 2}
                              b {:x 1 :z 3}]
                          (= a b)
                          a))))))

  (testing "empty maps are trivially equal — no contradiction"
    (is (= [{}]
           (m/query (do (let [a {}
                              b {}]
                          (= a b)
                          a)))))))

;; ════════════════════════════════════════════════════════════════
;; Propagation edge cases (from edge_cases_test section 3)
;; ════════════════════════════════════════════════════════════════

(deftest circular-eq-propagation
  (testing "(= x y) + (= y x) — circular doesn't loop forever"
    (is (= #{[1 1] [2 2] [3 3]}
           (set (m/query (let [x (one-of 1 2 3)
                               y (one-of 1 2 3)]
                           (and (= x y) (= y x)
                                [x y]))))))))

(deftest long-propagation-chain
  (testing "narrowing cascades through chain: a=b, b=c, c=d, d>2"
    (let [results (m/query (let [a (one-of 1 2 3 4 5)
                                 b (one-of 1 2 3 4 5)
                                 c (one-of 1 2 3 4 5)
                                 d (one-of 1 2 3 4 5)]
                             (and (= a b) (= b c) (= c d) (> d 2)
                                  [a b c d])))]
      (is (every? (fn [[a b c d]] (and (= a b c d) (> d 2))) results))
      (is (= #{[3 3 3 3] [4 4 4 4] [5 5 5 5]} (set results))))))

(deftest cascading-neq-narrowing
  (testing "neq cascades when singleton is produced"
    (let [results (m/query (let [x (one-of 1)
                                 y (one-of 1 2 3)
                                 z (one-of 1 2 3)]
                             (and (!= x y) (!= y z) (!= x z)
                                  [x y z])))]
      (is (= #{[1 2 3] [1 3 2]} (set results))))))

(deftest eq-transitivity
  (testing "equality propagates transitively: a=b, b=c implies a=c domains"
    (let [results (m/query (let [a (one-of 1 2 3 4)
                                 b (one-of 2 3 4 5)
                                 c (one-of 3 4 5 6)]
                             (and (= a b) (= b c)
                                  [a b c])))]
      ;; Intersection: a∩b = {2,3,4}, b∩c = {3,4,5}
      ;; Then a=b=c must all share domain → {3,4}
      (is (= #{[3 3 3] [4 4 4]} (set results))))))

(deftest neq-singleton-cascade
  (testing "neq cascade: x=1 forces y!=1, if y was {1,2} → y=2, then z!=2 forces z∈{1,3}"
    (let [results (m/query (let [x (one-of 1)
                                 y (one-of 1 2)
                                 z (one-of 1 2 3)]
                             (and (!= x y) (!= y z)
                                  [x y z])))]
      ;; x=1, y must be 2 (only option after !=x), z must be != 2 → z∈{1,3}
      (is (= #{[1 2 1] [1 2 3]} (set results))))))

;; ════════════════════════════════════════════════════════════════
;; Min/max edge cases (from edge_cases_test section 7)
;; ════════════════════════════════════════════════════════════════

(deftest min-max-basic
  (testing "max of two domains"
    (is (= #{3 4 5}
           (set (m/query (let [x (one-of 3 5)
                               y (one-of 2 4)]
                           (max x y)))))))
  (testing "min/max with singletons"
    (is (= [2] (m/query (min 3 2))))
    (is (= [5] (m/query (max 3 5))))))

(deftest min-max-same-values
  (testing "min of equal singletons"
    (is (= [5] (m/query (min 5 5)))))
  (testing "max of equal singletons"
    (is (= [5] (m/query (max 5 5))))))

(deftest min-max-no-inverse-propagation
  (testing "min/max don't propagate back (known limitation)"
    ;; (min x y) = 1 doesn't narrow x or y
    ;; This is documented — min/max are compute-only, not constraint-backed
    (let [results (m/query (let [x (one-of 1 2 3)
                                 y (one-of 1 2 3)
                                 m (min x y)]
                             (and (= m 1) [x y])))]
      ;; All pairs where min(x,y) = 1 should appear
      ;; But since min doesn't create a constraint, m is just a computed domain
      ;; and (= m 1) constrains m but doesn't propagate to x,y
      (is (pos? (count results))))))

;; ════════════════════════════════════════════════════════════════
;; Not edge cases (from edge_cases_test section 8)
;; ════════════════════════════════════════════════════════════════

(deftest not-with-non-relational
  (testing "(not true) — non-relational inner form should error gracefully"
    (is (thrown? #?(:clj Exception :cljs js/Error)
                 (m/query (not true))))))

(deftest not-double-negation
  (testing "(not (not ...)) — double negation is not supported (known limitation)"
    ;; not only handles relational ops, not itself. Would need special handling.
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo)
                 (m/query (let [x (one-of 1 2 3)]
                            (and (not (not (= x 1))) x)))))))

(deftest not-negating-neq
  (testing "(not (!= x 1)) is equivalent to (= x 1)"
    (is (= [1] (m/query (let [x (one-of 1 2 3)]
                          (and (not (!= x 1)) x)))))))

;; ════════════════════════════════════════════════════════════════
;; Minus propagation (from edge_cases_test section 15)
;; ════════════════════════════════════════════════════════════════

(deftest minus-constraint
  (testing "subtraction with constraint"
    ;; d = x-y, d > 1: all pairs where x-y > 1
    ;; (3,1)→2✓ (4,1)→3✓ (4,2)→2✓ (5,1)→4✓ (5,2)→3✓ (5,3)→2✓
    (is (= #{[3 1] [4 1] [4 2] [5 1] [5 2] [5 3]}
           (set (m/query (let [x (one-of 3 4 5)
                               y (one-of 1 2 3)
                               d (- x y)]
                           (and (> d 1)
                                [x y]))))))))

;; ════════════════════════════════════════════════════════════════
;; Multiplication propagation (from edge_cases_test section 16)
;; ════════════════════════════════════════════════════════════════

(deftest times-constraint
  (testing "multiplication with constraint"
    (is (= #{[2 3] [3 2]}
           (set (m/query (let [x (one-of 1 2 3 4)
                               y (one-of 1 2 3 4)]
                           (and (= (* x y) 6)
                                [x y]))))))))

;; ════════════════════════════════════════════════════════════════
;; Even/odd interaction (from edge_cases_test section 19)
;; ════════════════════════════════════════════════════════════════

(deftest even-then-gt-constraint
  (testing "even followed by relational constraint preserves evenness"
    (is (= [8 10] (m/query (let [x (between 1 10)]
                             (and (even x) (> x 6) x)))))))

(deftest even-with-equality-constraint
  (testing "even combined with arithmetic equality"
    ;; x even, x+y=7: x=2→y=5, x=4→y=3, x=6→y=1
    (is (= #{[2 5] [4 3] [6 1]}
           (set (m/query (let [x (between 1 10)
                               y (between 1 10)]
                           (and (even x)
                                (= (+ x y) 7)
                                [x y]))))))))

(deftest odd-with-or
  (testing "odd combined with or"
    (is (= #{1 7 9}
           (set (m/query (let [x (between 1 10)]
                           (and (odd x)
                                (or (= x 1) (> x 6))
                                x))))))))

;; ════════════════════════════════════════════════════════════════
;; Abs edge cases (from edge_cases_test section 20)
;; NOTE: duplicate abs-of-zero from section 9 — keeping one as abs-of-zero-narrow
;; ════════════════════════════════════════════════════════════════

(deftest abs-of-zero-narrow
  (testing "|0| = 0"
    (is (= [0] (m/query (let [x (one-of -1 0 1)]
                          (and (= (abs x) 0) x)))))))

(deftest abs-negative-only
  (testing "abs on negative-only domain"
    (is (= #{1 2 3}
           (set (m/query (let [x (one-of -3 -2 -1)]
                           (abs x))))))))

(deftest abs-constraint-propagation
  (testing "constraining abs result propagates back to operand"
    (is (= #{-2 2}
           (set (m/query (let [x (one-of -3 -2 -1 0 1 2 3)]
                           (and (= (abs x) 2) x))))))))

;; ════════════════════════════════════════════════════════════════
;; Mod/quot edge cases (from edge_cases_test section 21)
;; ════════════════════════════════════════════════════════════════

(deftest mod-zero-divisor
  (testing "mod where divisor domain contains 0 — 0 is filtered out"
    (is (= #{0 1 2}
           (set (m/query (let [x (between 1 5)
                               d (one-of 0 3)]
                           (mod x d))))))))

(deftest quot-zero-divisor
  (testing "quot where divisor domain contains 0 — 0 is filtered out"
    (is (= #{0 1}
           (set (m/query (let [x (between 1 5)
                               d (one-of 0 3)]
                           (quot x d))))))))

(deftest mod-with-constraint-back-propagation
  (testing "constraining mod result narrows operand"
    (is (= #{3 6 9}
           (set (m/query (let [x (between 1 10)]
                           (and (= (mod x 3) 0) x))))))))

;; ════════════════════════════════════════════════════════════════
;; Mod/quot (from phase2_test)
;; ════════════════════════════════════════════════════════════════

(deftest mod-quot
  (testing "mod constraint — multiples of 3"
    (is (= [3 6 9 12 15 18]
           (m/query (let [x (between 1 20)] (and (= (mod x 3) 0) x))))))
  (testing "quot constraint"
    (is (= [6 7 8]
           (m/query (let [x (between 0 10)] (and (= (quot x 3) 2) x)))))))

;; ════════════════════════════════════════════════════════════════
;; Even/odd (from phase2_test)
;; ════════════════════════════════════════════════════════════════

(deftest even-odd
  (testing "even"
    (is (= [2 4 6 8 10]
           (m/query (let [x (between 1 10)] (and (even x) x))))))
  (testing "odd"
    (is (= [1 3 5 7 9]
           (m/query (let [x (between 1 10)] (and (odd x) x)))))))

;; ════════════════════════════════════════════════════════════════
;; Not negation (from phase2_test)
;; ════════════════════════════════════════════════════════════════

(deftest not-negation
  (testing "not ="
    (is (= #{1 3} (set (m/query (let [x (one-of 1 2 3)] (and (not (= x 2)) x)))))))
  (testing "not <"
    (is (= #{3 4 5} (set (m/query (let [x (one-of 1 2 3 4 5)] (and (not (< x 3)) x))))))))

;; ════════════════════════════════════════════════════════════════
;; Abs constraint (from phase2_test)
;; ════════════════════════════════════════════════════════════════

(deftest abs-constraint
  (testing "|x| = 2 finds both signs"
    (is (= #{-2 2}
           (set (m/query (let [x (one-of -3 -2 -1 0 1 2 3)]
                           (and (= (abs x) 2) x))))))))

;; ════════════════════════════════════════════════════════════════
;; Min/max (from phase2_test)
;; ════════════════════════════════════════════════════════════════

(deftest min-max
  (testing "min of two domains"
    ;; min(3,2)=2, min(3,4)=3, min(5,2)=2, min(5,4)=4
    (is (= #{2 3 4}
           (set (m/query (let [x (one-of 3 5)
                               y (one-of 2 4)]
                           (min x y))))))))

;; ════════════════════════════════════════════════════════════════
;; Numeric predicates (from new_features_test)
;; ════════════════════════════════════════════════════════════════

(deftest pos-predicate
  (testing "pos filters to positive values"
    (is (= [1 2 3]
           (m/query (let [x (one-of -2 -1 0 1 2 3)]
                      (and (pos x) x))))))

  (testing "pos contradiction"
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) #"Contradiction"
          (m/query (let [x (one-of -2 -1 0)]
                     (and (pos x) x)))))))

(deftest neg-predicate
  (testing "neg filters to negative values"
    (is (= [-2 -1]
           (m/query (let [x (one-of -2 -1 0 1 2)]
                      (and (neg x) x))))))

  (testing "neg contradiction"
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) #"Contradiction"
          (m/query (let [x (one-of 0 1 2)]
                     (and (neg x) x)))))))

(deftest zero-predicate
  (testing "zero filters to zero"
    (is (= [0]
           (m/query (let [x (one-of -1 0 1)]
                      (and (zero x) x))))))

  (testing "zero contradiction"
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) #"Contradiction"
          (m/query (let [x (one-of 1 2 3)]
                     (and (zero x) x)))))))

;; ════════════════════════════════════════════════════════════════
;; Type predicates (from new_features_test)
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
;; Predicate composition (from new_features_test)
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
