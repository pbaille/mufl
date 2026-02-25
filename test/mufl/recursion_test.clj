(ns mufl.recursion-test
  (:require [clojure.test :refer [deftest testing is]]
            [mufl.core :as m]))

;; ════════════════════════════════════════════════════════════════
;; 1. BASIC RECURSION — Ground args
;; ════════════════════════════════════════════════════════════════

(deftest factorial
  (testing "factorial of 0"
    (is (= [1] (m/query (let [fact (fn [n] (if (= n 0) 1 (* n (fact (- n 1)))))]
                           (fact 0))))))
  (testing "factorial of 1"
    (is (= [1] (m/query (let [fact (fn [n] (if (= n 0) 1 (* n (fact (- n 1)))))]
                           (fact 1))))))
  (testing "factorial of 5"
    (is (= [120] (m/query (let [fact (fn [n] (if (= n 0) 1 (* n (fact (- n 1)))))]
                             (fact 5))))))
  (testing "factorial of 10"
    (is (= [3628800] (m/query (let [fact (fn [n] (if (= n 0) 1 (* n (fact (- n 1)))))]
                                (fact 10)))))))

(deftest fibonacci
  (testing "fibonacci of 0"
    (is (= [0] (m/query (let [fib (fn [n] (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))]
                           (fib 0))))))
  (testing "fibonacci of 1"
    (is (= [1] (m/query (let [fib (fn [n] (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))]
                           (fib 1))))))
  (testing "fibonacci of 6"
    (is (= [8] (m/query (let [fib (fn [n] (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))]
                           (fib 6))))))
  (testing "fibonacci of 10"
    (is (= [55] (m/query (let [fib (fn [n] (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))]
                            (fib 10)))))))

(deftest sum-to-n
  (testing "sum of 1..10"
    (is (= [55] (m/query (let [sum-to (fn [n] (if (= n 0) 0 (+ n (sum-to (- n 1)))))]
                            (sum-to 10)))))))

(deftest countdown
  (testing "countdown to zero"
    (is (= [0] (m/query (let [f (fn [n] (if (= n 0) 0 (f (- n 1))))]
                           (f 10))))))
  (testing "countdown to 1"
    (is (= [1] (m/query (let [f (fn [n] (if (= n 1) n (f (- n 1))))]
                           (f 10)))))))

;; ════════════════════════════════════════════════════════════════
;; 2. RECURSION WITH MULTIPLE PARAMS
;; ════════════════════════════════════════════════════════════════

(deftest multi-param-recursion
  (testing "tail-recursive sum via accumulator"
    (is (= [5] (m/query (let [f (fn [a b] (if (= a 0) b (f (- a 1) (+ b 1))))]
                           (f 5 0))))))
  (testing "power function (x^n)"
    (is (= [256] (m/query (let [pow (fn [x n] (if (= n 0) 1 (* x (pow x (- n 1)))))]
                             (pow 2 8))))))
  (testing "gcd"
    (is (= [6] (m/query (let [gcd (fn [a b] (if (= b 0) a (gcd b (mod a b))))]
                           (gcd 48 18)))))))

;; ════════════════════════════════════════════════════════════════
;; 3. RECURSION WITH <= / < BASE CASES
;; ════════════════════════════════════════════════════════════════

(deftest lte-base-case
  (testing "factorial with <= base case"
    (is (= [720] (m/query (let [fact (fn [n] (if (<= n 1) 1 (* n (fact (- n 1)))))]
                             (fact 6)))))))

;; ════════════════════════════════════════════════════════════════
;; 4. DEPTH LIMIT
;; ════════════════════════════════════════════════════════════════

(deftest recursion-depth-limit
  (testing "infinite recursion is caught"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Recursion depth limit exceeded"
                          (m/query (let [f (fn [x] (f x))] (f 1))))))
  (testing "mutual-style infinite recursion caught"
    ;; Even though true mutual recursion isn't supported,
    ;; a function that always recurs should be caught
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Recursion depth limit exceeded"
                          (m/query (let [f (fn [x] (f (+ x 1)))] (f 0)))))))

;; ════════════════════════════════════════════════════════════════
;; 5. RECURSION COMBINED WITH CLOSURE
;; ════════════════════════════════════════════════════════════════

(deftest recursion-with-closure
  (testing "recursive fn accessing closed-over value"
    (is (= [30] (m/query (let [step 3
                                f (fn [n] (if (= n 0) 0 (+ step (f (- n 1)))))]
                            (f 10))))))
  (testing "recursive fn with closed-over multiplier"
    (is (= [32] (m/query (let [base 2
                                pow (fn [n] (if (= n 0) 1 (* base (pow (- n 1)))))]
                            (pow 5)))))))

;; ════════════════════════════════════════════════════════════════
;; 6. NESTED FUNCTION CALLS (non-recursive + recursive)
;; ════════════════════════════════════════════════════════════════

(deftest nested-fn-with-recursion
  (testing "non-recursive fn wrapping recursive result"
    (is (= [240] (m/query (let [fact (fn [n] (if (= n 0) 1 (* n (fact (- n 1)))))
                                 double (fn [x] (+ x x))]
                             (double (fact 5)))))))
  (testing "recursive fn calling non-recursive fn"
    (is (= [1024] (m/query (let [double (fn [x] (+ x x))
                                  pow2 (fn [n] (if (= n 0) 1 (double (pow2 (- n 1)))))]
                              (pow2 10)))))))

;; ════════════════════════════════════════════════════════════════
;; 7. EAGERNESS OF IF — Verify non-ground still defers to fork
;; ════════════════════════════════════════════════════════════════

(deftest if-still-defers-for-non-ground
  (testing "if with non-ground variable still produces correct results"
    (is (= #{1 2 3 4 10}
           (set (m/query (let [x (between 1 10)]
                           (if (< x 5) x 10)))))))
  (testing "cond with non-ground variable still works"
    (is (= #{0 1 2 3 8 9 10}
           (set (m/query (let [x (between 1 10)]
                           (cond
                             (< x 4) x
                             (> x 7) x
                             :else 0))))))))

;; ════════════════════════════════════════════════════════════════
;; 8. EDGE CASES FOR EAGER IF
;; ════════════════════════════════════════════════════════════════

(deftest eager-if-both-branches-fail
  (testing "if where both branches of constraint are impossible"
    ;; x is in {5}, (= x 10) fails, (not (= x 10)) succeeds → else
    (is (= [:no] (m/query (let [x 5]
                             (if (= x 10) :yes :no)))))))

(deftest eager-if-then-only
  (testing "if where only then is satisfiable"
    (is (= [:yes] (m/query (let [x 5]
                              (if (= x 5) :yes :no)))))))

;; ════════════════════════════════════════════════════════════════
;; 9. COND EAGER RESOLUTION
;; ════════════════════════════════════════════════════════════════

(deftest cond-eager-with-ground
  (testing "cond with ground values resolves eagerly"
    (is (= [:medium] (m/query (let [x 5]
                                 (cond
                                   (< x 3)  :small
                                   (> x 7)  :big
                                   :else    :medium)))))))
