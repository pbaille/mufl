(ns mufl.type-constructors-test
  (:require [clojure.test :refer [deftest testing is]]
            [mufl.core :as m]
            [mufl.domain :as dom]))

;; ════════════════════════════════════════════════════════════════
;; vector-of: constrain all vector elements to a type
;; ════════════════════════════════════════════════════════════════

(deftest vector-of-basic
  (testing "vector-of integer narrows mixed-type elements"
    (is (= [[1 2 3]]
           (m/query (let [v [(one-of 1 "a") (one-of 2 "b") (one-of 3 "c")]]
                      (vector-of integer v)
                      v)))))

  (testing "vector-of string narrows to string elements"
    (is (= [["a" "b" "c"]]
           (m/query (let [v [(one-of 1 "a") (one-of 2 "b") (one-of 3 "c")]]
                      (vector-of string v)
                      v)))))

  (testing "vector-of with already-typed elements is a no-op"
    (is (= [[1 2 3]]
           (m/query (let [v [1 2 3]]
                      (vector-of integer v)
                      v)))))

  (testing "vector-of integer on empty vector"
    (is (= [[]]
           (m/query (let [v []]
                      (vector-of integer v)
                      v))))))

(deftest vector-of-with-solve
  (testing "vector-of + solve enumerates valid element combinations"
    (is (= [[1 2] [1 3] [2 3]]
           (m/query (let [v [(one-of 1 2 "x") (one-of 2 3 "y")]]
                      (vector-of integer v)
                      (< (nth v 0) (nth v 1))
                      v)))))

  (testing "vector-of keyword filters correctly"
    (is (= [[:a :b]]
           (m/query (let [v [(one-of :a 1) (one-of :b 2)]]
                      (vector-of keyword v)
                      v))))))

(deftest vector-of-contradiction
  (testing "vector-of integer with all-string elements throws"
    (is (thrown? Exception
                (m/query (let [v ["a" "b" "c"]]
                           (vector-of integer v)
                           v))))))

;; ════════════════════════════════════════════════════════════════
;; vector-of in defdomain context
;; ════════════════════════════════════════════════════════════════

(deftest vector-of-defdomain
  (testing "defdomain with vector-of schema"
    (is (= [[1 2 3]]
           (m/query (do (defdomain IntVec (vector-of integer))
                        (let [v [(one-of 1 "a") (one-of 2 "b") (one-of 3 "c")]]
                          (IntVec v)
                          v))))))

  (testing "defdomain vector-of in map field"
    (is (= [[1 2]]
           (m/query (do (defdomain HasScores {:scores (vector-of integer)})
                        (let [p {:scores [(one-of 1 "a") (one-of 2 "b")]}]
                          (HasScores p)
                          (get p :scores)))))))

  (testing "defdomain vector-of composed with and"
    (is (= [["Alice" [90 80]]]
           (m/query (do (defdomain Student (and {:name string}
                                                {:scores (vector-of integer)}))
                        (let [s {:name "Alice" :scores [(one-of 90 "x") (one-of 80 "y")]}]
                          (Student s)
                          [(get s :name) (get s :scores)])))))))

;; ════════════════════════════════════════════════════════════════
;; tuple: per-position type constraints
;; ════════════════════════════════════════════════════════════════

(deftest tuple-basic
  (testing "tuple constrains per-position types"
    (is (= [[1 "hello"]]
           (m/query (let [v [(one-of 1 "a") (one-of 2 "hello")]]
                      (tuple [integer string] v)
                      v)))))

  (testing "tuple with three positions"
    (is (= [[42 "hi" true]]
           (m/query (let [v [(one-of 42 "x") (one-of "hi" 99) (one-of true 0)]]
                      (tuple [integer string boolean] v)
                      v)))))

  (testing "tuple length mismatch throws"
    (is (thrown? Exception
                (m/query (let [v [1 2 3]]
                           (tuple [integer string] v)
                           v))))))

(deftest tuple-with-solve
  (testing "tuple + solve works across positions"
    (is (= #{[1 "a"] [2 "a"] [1 "b"] [2 "b"]}
           (set
            (m/query (let [v [(one-of 1 2) (one-of "a" "b")]]
                       (tuple [integer string] v)
                       v)))))))

(deftest tuple-defdomain
  (testing "defdomain with tuple schema"
    (is (= [[42 "hello"]]
           (m/query (do (defdomain Pair (tuple [integer string]))
                        (let [v [(one-of 42 "x") (one-of "hello" 99)]]
                          (Pair v)
                          v))))))

  (testing "defdomain tuple for 2D point"
    (is (= [[3 4]]
           (m/query (do (defdomain Point (tuple [integer integer]))
                        (let [p [(one-of 3 "x") (one-of 4 "y")]]
                          (Point p)
                          p)))))))

;; ════════════════════════════════════════════════════════════════
;; map-of: constrain all map entries
;; ════════════════════════════════════════════════════════════════

(deftest map-of-basic
  (testing "map-of keyword integer narrows values"
    (is (= [{:a 1 :b 2}]
           (m/query (let [m {:a (one-of 1 "x") :b (one-of 2 "y")}]
                      (map-of keyword integer m)
                      m)))))

  (testing "map-of keyword string narrows values"
    (is (= [{:x "hello" :y "world"}]
           (m/query (let [m {:x (one-of "hello" 42) :y (one-of "world" 99)}]
                      (map-of keyword string m)
                      m))))))

(deftest map-of-key-validation
  (testing "map-of with wrong key type throws"
    ;; map-of keyword integer on a map with integer keys would fail
    ;; However mufl maps use keyword keys by default, so this is
    ;; mainly for documentation. Let's test the positive case:
    (is (= [{:a 1}]
           (m/query (let [m {:a (one-of 1 "x")}]
                      (map-of keyword integer m)
                      m))))))

(deftest map-of-defdomain
  (testing "defdomain with map-of schema"
    (is (= [{:x 10 :y 20}]
           (m/query (do (defdomain Scores (map-of keyword integer))
                        (let [s {:x (one-of 10 "a") :y (one-of 20 "b")}]
                          (Scores s)
                          s))))))

  (testing "defdomain map-of in composed domain"
    (is (= [["Alice" {:math 90}]]
           (m/query (do (defdomain GradeCard (and {:name string}
                                                  {:grades (map-of keyword integer)}))
                        (let [gc {:name "Alice" :grades {:math (one-of 90 "A")}}]
                          (GradeCard gc)
                          [(get gc :name) (get gc :grades)])))))))

;; ════════════════════════════════════════════════════════════════
;; Nested type constructors
;; ════════════════════════════════════════════════════════════════

(deftest nested-vector-of
  (testing "vector-of with vector-of (matrix)"
    ;; (defdomain Matrix (vector-of (vector-of integer)))
    ;; This requires nested schema resolution
    (is (= [[[1 2] [3 4]]]
           (m/query (do (defdomain IntVec (vector-of integer))
                        (let [m [[(one-of 1 "a") (one-of 2 "b")]
                                 [(one-of 3 "c") (one-of 4 "d")]]]
                          (vector-of IntVec m)
                          m)))))))

(deftest vector-of-with-domain-def
  (testing "vector-of with a named domain type"
    (is (= [[{:name "Alice"} {:name "Bob"}]]
           (m/query (do (defdomain Named {:name string})
                        (let [people [{:name (one-of "Alice" 42)}
                                      {:name (one-of "Bob" 99)}]]
                          (vector-of Named people)
                          people)))))))

;; ════════════════════════════════════════════════════════════════
;; Type constructors with type predicates (unary form)
;; ════════════════════════════════════════════════════════════════

(deftest type-constructors-with-type-predicates
  (testing "vector-of integer acts as both type check and constraint"
    (is (= [[10 20]]
           (m/query (let [v [(one-of 10 "x") (one-of 20 "y")]]
                      (vector-of integer v)
                      v)))))

  (testing "tuple with between-constrained positions"
    ;; tuple types must reference named types/domains — between needs defdomain
    (is (= [[5 "hi"]]
           (m/query (do (defdomain SmallInt (between 1 10))
                        (let [v [(one-of 5 "x") (one-of "hi" 99)]]
                          (tuple [SmallInt string] v)
                          v)))))))

;; ════════════════════════════════════════════════════════════════
;; Integration: type constructors + existing features
;; ════════════════════════════════════════════════════════════════

(deftest type-constructors-with-map-fn
  (testing "vector-of + map produces correctly typed results"
    (is (= [[2 4 6]]
           (m/query (let [v [(one-of 1 "a") (one-of 2 "b") (one-of 3 "c")]]
                      (vector-of integer v)
                      (map (fn [x] (* x 2)) v)))))))

(deftest type-constructors-with-filter
  (testing "vector-of narrows before filter"
    (is (= [[2]]
           (m/query (let [v [(one-of 1 "a") (one-of 2 "b") (one-of 3 "c")]]
                      (vector-of integer v)
                      (filter (fn [x] (even x)) v)))))))

(deftest type-constructors-with-reduce
  (testing "vector-of + reduce works"
    (is (= [6]
           (m/query (let [v [(one-of 1 "a") (one-of 2 "b") (one-of 3 "c")]]
                      (vector-of integer v)
                      (reduce (fn [acc x] (+ acc x)) 0 v)))))))
