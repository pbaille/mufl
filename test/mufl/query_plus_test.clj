(ns mufl.query-plus-test
  (:require [clojure.test :refer [deftest testing is]]
            [mufl.core :as m]))

;; ════════════════════════════════════════════════════════════════
;; query+ — returns full solved environments as maps
;; ════════════════════════════════════════════════════════════════

(deftest query+-basic
  (testing "single variable"
    (is (= '[{x 1} {x 2} {x 3}]
           (m/query+ (let [x (one-of 1 2 3)] x)))))

  (testing "two constrained variables"
    (is (= '[{x 1, y 2} {x 1, y 3} {x 2, y 3}]
           (m/query+ (let [x (one-of 1 2 3)
                           y (one-of 1 2 3)]
                       (and (< x y) [x y]))))))

  (testing "ground values — one solution"
    (is (= '[{x 42, y "hello"}]
           (m/query+ (let [x 42 y "hello"] [x y]))))))

(deftest query+-with-between
  (testing "between domain with constraint"
    (is (= '[{x 4} {x 5}]
           (m/query+ (let [x (between 1 5)] (and (> x 3) x))))))

  (testing "pythagorean triples show all bindings"
    (let [results (m/query+ (let [a (between 1 10)
                                  b (between 1 10)
                                  c (between 1 10)]
                              (and (< a b)
                                   (<= b c)
                                   (= (+ (* a a) (* b b)) (* c c))
                                   [a b c])))]
      (is (= 2 (count results)))
      ;; Each result has all three bindings
      (is (every? #(= #{'a 'b 'c} (set (keys %))) results))
      ;; Values are correct
      (is (= '{a 3, b 4, c 5} (first results))))))

(deftest query+-with-collections
  (testing "map variable — extracted fully"
    (is (= '[{m {:x 2, :y 10}} {m {:x 3, :y 10}}]
           (m/query+ (let [m {:x (one-of 1 2 3) :y 10}]
                       (and (> (:x m) 1) m))))))

  (testing "vector variable"
    (is (= '[{v [1 20]} {v [2 20]} {v [3 20]}]
           (m/query+ (let [v [(one-of 1 2 3) 20]] v))))))

(deftest query+-with-defn
  (testing "defn definitions are excluded from bindings"
    (let [results (m/query+ (defn positive [x] (> x 0))
                            (let [n (one-of -1 0 1 2)]
                              (positive n)
                              n))]
      (is (= '[{n 1} {n 2}] results))
      ;; 'positive' should NOT appear as a binding
      (is (every? #(not (contains? % 'positive)) results)))))

(deftest query+-with-def
  (testing "def definitions are excluded from bindings"
    (let [results (m/query+ (def Person {:name string :age (between 0 150)})
                            (let [p {:name "Alice" :age (one-of 10 25 200)}]
                              (Person p)
                              (:age p)))]
      (is (= 2 (count results)))
      ;; Each result has 'p' as binding, not 'Person'
      (is (every? #(contains? % 'p) results))
      (is (every? #(not (contains? % 'Person)) results))
      ;; Ages are narrowed by the domain constraint
      (is (= #{10 25} (set (map #(get-in % ['p :age]) results)))))))

(deftest query+-with-fn
  (testing "fn definitions are excluded from bindings"
    (let [results (m/query+ (let [double (fn [x] (+ x x))
                                  n (between 1 5)]
                              (and (= (double n) 6) n)))]
      (is (= '[{n 3}] results))
      ;; 'double' should NOT appear as a binding
      (is (every? #(not (contains? % 'double)) results)))))

(deftest query+-contradiction
  (testing "contradiction throws (consistent with query)"
    (is (thrown? clojure.lang.ExceptionInfo
                (m/query+ (let [x (one-of 1 2 3)]
                            (and (> x 5) x)))))))

(deftest query+-void
  (testing "void domain returns empty vector"
    (is (= [] (m/query+ (let [x (one-of)] x))))))

(deftest query+-implicit-do
  (testing "multiple body forms (implicit do)"
    (let [results (m/query+ (defn big [x] (> x 3))
                            (let [n (one-of 1 2 3 4 5)]
                              (big n)
                              n))]
      (is (= '[{n 4} {n 5}] results)))))
