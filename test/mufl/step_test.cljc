(ns mufl.step-test
  (:require [clojure.test :refer [deftest testing is]]
            [mufl.domain :as dom]
            [mufl.core :as m]))

;; ════════════════════════════════════════════════════════════════
;; A. Unit tests for `step`
;; ════════════════════════════════════════════════════════════════

(deftest step-void
  (testing "void domain returns nil"
    (is (nil? (dom/step dom/void)))))

(deftest step-any
  (testing "any domain returns nil (not steppable)"
    (is (nil? (dom/step dom/any)))))

(deftest step-single
  (testing "single domain returns [value void]"
    (let [[v rest] (dom/step (dom/single 42))]
      (is (= 42 v))
      (is (dom/void? rest))))

  (testing "stepping void after single returns nil"
    (let [[_ rest] (dom/step (dom/single 42))]
      (is (nil? (dom/step rest))))))

(deftest step-finite
  (testing "finite domain picks smallest first"
    (let [[v rest] (dom/step (dom/finite #{3 1 2}))]
      (is (= 1 v))
      (is (= #{2 3} (:values rest)))))

  (testing "stepping through entire finite domain produces all values in order"
    (let [d (dom/finite #{5 2 8 1 3})
          results (loop [d d, acc []]
                    (if-let [[v d'] (dom/step d)]
                      (recur d' (conj acc v))
                      acc))]
      (is (= [1 2 3 5 8] results))))

  (testing "finite of two → single → void"
    (let [[v1 r1] (dom/step (dom/finite #{10 20}))
          [v2 r2] (dom/step r1)]
      (is (= 10 v1))
      (is (= 20 v2))
      (is (dom/void? r2)))))

(deftest step-range-bounded
  (testing "bounded range walks from lo to hi"
    (let [[v rest] (dom/step (dom/range-dom 1 5))]
      (is (= 1 v))
      (is (= {:kind :range, :lo 2, :hi 5} rest))))

  (testing "stepping through entire bounded range"
    (let [results (loop [d (dom/range-dom 3 7), acc []]
                    (if-let [[v d'] (dom/step d)]
                      (recur d' (conj acc v))
                      acc))]
      (is (= [3 4 5 6 7] results))))

  (testing "single-element range"
    (let [[v rest] (dom/step (dom/range-dom 5 5))]
      ;; range-dom normalizes 5,5 to (single 5)
      (is (= 5 v))
      (is (dom/void? rest)))))

(deftest step-range-half-infinite-up
  (testing "half-infinite upward: lo, lo+1, lo+2, ..."
    (let [d (dom/range-dom 10 nil)
          results (loop [d d, acc [], i 0]
                    (if (or (>= i 5) (nil? (dom/step d)))
                      acc
                      (let [[v d'] (dom/step d)]
                        (recur d' (conj acc v) (inc i)))))]
      (is (= [10 11 12 13 14] results)))))

(deftest step-range-half-infinite-down
  (testing "half-infinite downward: hi, hi-1, hi-2, ..."
    (let [d (dom/range-dom nil -5)
          results (loop [d d, acc [], i 0]
                    (if (or (>= i 5) (nil? (dom/step d)))
                      acc
                      (let [[v d'] (dom/step d)]
                        (recur d' (conj acc v) (inc i)))))]
      (is (= [-5 -6 -7 -8 -9] results)))))

(deftest step-range-unbounded-spiral
  (testing "unbounded range → spiral: 0, 1, -1, 2, -2, ..."
    (let [d (dom/range-dom nil nil)
          results (loop [d d, acc [], i 0]
                    (if (or (>= i 7) (nil? (dom/step d)))
                      acc
                      (let [[v d'] (dom/step d)]
                        (recur d' (conj acc v) (inc i)))))]
      (is (= [0 1 -1 2 -2 3 -3] results)))))

(deftest step-spiral-direct
  (testing "spiral domain produces correct sequence"
    (let [d (dom/spiral-dom)
          results (loop [d d, acc [], i 0]
                    (if (>= i 9)
                      acc
                      (let [[v d'] (dom/step d)]
                        (recur d' (conj acc v) (inc i)))))]
      (is (= [0 1 -1 2 -2 3 -3 4 -4] results)))))

(deftest step-boolean-type
  (testing "boolean type steps: true, false"
    (let [[v1 r1] (dom/step (dom/type-dom :boolean))
          [v2 r2] (dom/step r1)]
      (is (= true v1))
      (is (= false v2))
      (is (dom/void? r2))
      (is (nil? (dom/step r2))))))

(deftest step-non-boolean-type-not-steppable
  (testing "string type is not steppable"
    (is (nil? (dom/step dom/string-dom))))
  (testing "integer type is not steppable (use range-dom nil nil instead)"
    (is (nil? (dom/step dom/integer-dom))))
  (testing "keyword type is not steppable"
    (is (nil? (dom/step dom/keyword-dom)))))

;; ── steppable? predicate ───────────────────────────────────────

(deftest steppable-predicate
  (testing "steppable? returns correct values"
    (is (dom/steppable? (dom/single 1)))
    (is (dom/steppable? (dom/finite #{1 2 3})))
    (is (dom/steppable? (dom/range-dom 1 10)))
    (is (dom/steppable? (dom/range-dom 1 nil)))
    (is (dom/steppable? (dom/range-dom nil nil)))
    (is (dom/steppable? (dom/spiral-dom)))
    (is (dom/steppable? (dom/type-dom :boolean)))
    (is (not (dom/steppable? dom/void)))
    (is (not (dom/steppable? dom/any)))
    (is (not (dom/steppable? dom/string-dom)))
    (is (not (dom/steppable? dom/integer-dom)))
    (is (not (dom/steppable? (dom/vector-of-dom dom/integer-dom))))))

;; ── spiral relate ──────────────────────────────────────────────

(deftest spiral-relate
  (testing "spiral relates correctly"
    (is (= :equal (dom/relate (dom/spiral-dom) (dom/spiral-dom))))
    (is (= :equal (dom/relate (dom/spiral-dom) (dom/range-dom nil nil))))
    (is (= :equal (dom/relate (dom/spiral-dom) dom/integer-dom)))
    (is (= :subset (dom/relate (dom/spiral-dom) dom/number-dom)))
    (is (= :superset (dom/relate (dom/spiral-dom) (dom/range-dom 1 10))))
    (is (= :superset (dom/relate (dom/spiral-dom) (dom/single 5))))
    (is (= :superset (dom/relate (dom/spiral-dom) (dom/finite #{1 2 3}))))))

(deftest spiral-contains-val
  (testing "spiral contains all integers"
    (is (dom/contains-val? (dom/spiral-dom) 0))
    (is (dom/contains-val? (dom/spiral-dom) 42))
    (is (dom/contains-val? (dom/spiral-dom) -100))
    (is (not (dom/contains-val? (dom/spiral-dom) 1.5)))
    (is (not (dom/contains-val? (dom/spiral-dom) "hello")))))

;; ════════════════════════════════════════════════════════════════
;; B. query1 equivalence tests
;; ════════════════════════════════════════════════════════════════

(deftest query1-basic
  (testing "query1 returns first solution matching query"
    (let [result (m/query1 (let [x (one-of 1 2 3)] x))]
      (is (some? result))
      (is (= 1 (first result)))))

  (testing "query1 on single value"
    (let [[v _] (m/query1 (let [x (one-of 42)] x))]
      (is (= 42 v))))

  (testing "query1 on void returns nil"
    (is (nil? (m/query1 (let [x (one-of 1 2 3)]
                          (and (> x 10) x)))))))

(deftest query1-enumerate-all
  (testing "repeated query1 calls enumerate all solutions"
    (let [results (loop [state (m/query1* '(let [x (one-of 1 2 3)] x))
                         acc []]
                    (if state
                      (let [[v s] state]
                        (recur (m/query1* s) (conj acc v)))
                      acc))]
      (is (= [1 2 3] results)))))

(deftest query1-with-constraints
  (testing "query1 with constraints"
    (let [[v _] (m/query1 (let [x (one-of 1 2 3 4 5)]
                            (and (> x 3) x)))]
      (is (= 4 v))))

  (testing "enumerate constrained solutions"
    (let [results (loop [state (m/query1* '(let [x (one-of 1 2 3 4 5)]
                                             (and (> x 2) x)))
                         acc []]
                    (if state
                      (let [[v s] state]
                        (recur (m/query1* s) (conj acc v)))
                      acc))]
      (is (= [3 4 5] results)))))

(deftest query1-multi-variable
  (testing "query1 with two variables"
    (let [results (loop [state (m/query1* '(let [x (one-of 1 2)
                                                 y (one-of 10 20)]
                                             [x y]))
                         acc []]
                    (if state
                      (let [[v s] state]
                        (recur (m/query1* s) (conj acc v)))
                      acc))]
      (is (= (set results) (set [[1 10] [1 20] [2 10] [2 20]]))))))

;; ════════════════════════════════════════════════════════════════
;; C. query-lazy equivalence tests
;; ════════════════════════════════════════════════════════════════

(deftest query-lazy-basic
  (testing "query-lazy produces all solutions lazily"
    (is (= [1 2 3]
           (vec (m/query-lazy (let [x (one-of 1 2 3)] x))))))

  (testing "query-lazy with constraints"
    (is (= #{4 5}
           (set (m/query-lazy (let [x (one-of 1 2 3 4 5)]
                                (and (> x 3) x)))))))

  (testing "query-lazy on void returns empty seq"
    (is (empty? (m/query-lazy (let [x (one-of 1 2 3)]
                                (and (> x 10) x)))))))

(deftest query-lazy-equivalence
  (testing "query-lazy matches query for finite domains"
    ;; Simple enumeration
    (is (= (set (m/query (let [x (one-of 1 2 3)] x)))
           (set (m/query-lazy (let [x (one-of 1 2 3)] x)))))

    ;; With constraints
    (is (= (set (m/query (let [x (one-of 1 2 3 4 5)]
                           (and (> x 2) x))))
           (set (m/query-lazy (let [x (one-of 1 2 3 4 5)]
                                (and (> x 2) x))))))

    ;; Multi-variable
    (is (= (set (m/query (let [x (one-of 1 2)
                               y (one-of 10 20)]
                           [x y])))
           (set (m/query-lazy (let [x (one-of 1 2)
                                    y (one-of 10 20)]
                                [x y])))))))

(deftest query-lazy-range
  (testing "query-lazy with bounded range (between)"
    (is (= [1 2 3 4 5]
           (vec (m/query-lazy (let [x (between 1 5)] x))))))

  (testing "take from larger range"
    (is (= [1 2 3 4 5]
           (vec (take 5 (m/query-lazy (let [x (between 1 100)] x))))))))

;; ════════════════════════════════════════════════════════════════
;; D. Fork tests (if/cond with query1/query-lazy)
;; ════════════════════════════════════════════════════════════════

(deftest query-lazy-with-if
  (testing "query-lazy with if expression"
    (is (= (set (m/query (let [x (one-of 1 2 3 4 5)]
                           (if (> x 3) x (* x 10)))))
           (set (m/query-lazy (let [x (one-of 1 2 3 4 5)]
                                (if (> x 3) x (* x 10)))))))))

(deftest query1-with-if
  (testing "query1 with if produces solutions"
    (let [result (m/query1 (let [x (one-of 1 2 3)]
                             (if (> x 2) x (* x 10))))]
      (is (some? result)))))

;; ════════════════════════════════════════════════════════════════
;; E. Range domain step tests via query-lazy
;; ════════════════════════════════════════════════════════════════

(deftest query-lazy-range-with-constraints
  (testing "range with constraints via query-lazy"
    (is (= [6 7 8 9 10]
           (vec (m/query-lazy (let [x (between 1 10)]
                                (and (> x 5) x)))))))

  (testing "range with equality constraint"
    (is (= [5]
           (vec (m/query-lazy (let [x (between 1 10)]
                                (and (= x 5) x)))))))

  (testing "range with multiple constraints"
    (is (= [4 6 8 10]
           (vec (m/query-lazy (let [x (between 1 10)]
                                (and (> x 3)
                                     (= 0 (mod x 2))
                                     x))))))))
