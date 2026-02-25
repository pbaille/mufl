(ns mufl.destructuring-test
  (:require [clojure.test :refer [deftest testing is]]
            [mufl.core :as m]))

;; ════════════════════════════════════════════════════════════════
;; Map destructuring: {:key pattern}
;; ════════════════════════════════════════════════════════════════

(deftest map-destructuring-basic
  (testing "basic map destructuring"
    (is (= [1] (m/query (let [{:x x} {:x 1 :y 2}] x))))
    (is (= [2] (m/query (let [{:y y} {:x 1 :y 2}] y)))))

  (testing "multiple keys"
    (is (= [3] (m/query (let [{:x x :y y} {:x 1 :y 2}] (+ x y))))))

  (testing "string-valued map"
    (is (= ["Alice"]
           (m/query (let [{:name name} {:name "Alice" :age 30}] name)))))

  (testing "return full domain through destructuring"
    (is (= [1 2 3]
           (m/query (let [{:x x} {:x (one-of 1 2 3)}] x))))))

(deftest map-destructuring-with-as
  (testing "as operator binds the whole value"
    (is (= [{:x 1 :y 2}]
           (m/query (let [(as m {:x x}) {:x 1 :y 2}] m)))))

  (testing "as and destructured keys coexist"
    (is (= [1]
           (m/query (let [(as m {:x x}) {:x 1 :y 2}] x)))))

  (testing "as with constraints on destructured bindings"
    (is (= [{:x 2 :y 10} {:x 3 :y 10}]
           (m/query (let [(as m {:x x}) {:x (one-of 1 2 3) :y 10}]
                      (and (> x 1) m)))))))

(deftest map-destructuring-with-constraints
  (testing "constraint propagation through destructured binding"
    (is (= [2 3]
           (m/query (let [{:x x :y y} {:x (one-of 1 2 3) :y 10}]
                      (and (> x 1) x))))))

  (testing "equality constraint"
    (is (= [5]
           (m/query (let [{:x x} {:x (one-of 1 5 10)}]
                      (and (= x 5) x))))))

  (testing "relational constraint between destructured vars"
    (is (= [[1 2] [1 3] [2 3]]
           (m/query (let [{:x x :y y} {:x (one-of 1 2 3) :y (one-of 1 2 3)}]
                      (and (< x y) [x y]))))))

  (testing "arithmetic on destructured values"
    (is (= [30]
           (m/query (let [{:x x :y y} {:x 10 :y 20}] (+ x y))))))

  (testing "constraint narrows the original map element"
    (is (= [{:x 2 :y 10} {:x 3 :y 10}]
           (m/query (let [(as m {:x x}) {:x (one-of 1 2 3) :y 10}]
                      (and (> x 1) m)))))))

;; ════════════════════════════════════════════════════════════════
;; Vector destructuring: [a b c]
;; ════════════════════════════════════════════════════════════════

(deftest vector-destructuring-basic
  (testing "basic vector destructuring"
    (is (= [10] (m/query (let [[a b c] [10 20 30]] a))))
    (is (= [20] (m/query (let [[a b c] [10 20 30]] b))))
    (is (= [30] (m/query (let [[a b c] [10 20 30]] c)))))

  (testing "two-element vector"
    (is (= [5] (m/query (let [[x y] [5 10]] x))))
    (is (= [10] (m/query (let [[x y] [5 10]] y)))))

  (testing "single element vector"
    (is (= [42] (m/query (let [[x] [42]] x)))))

  (testing "return full domain through destructured element"
    (is (= [1 2 3]
           (m/query (let [[a b] [(one-of 1 2 3) 10]] a))))))

(deftest vector-destructuring-with-as
  (testing "as operator in vector pattern"
    (is (= [[10 20 30]]
           (m/query (let [(as v [a b c]) [10 20 30]] v)))))

  (testing "as with access to both element and whole"
    (is (= [10]
           (m/query (let [(as v [a b]) [10 20]] a))))))

(deftest vector-destructuring-with-constraints
  (testing "constraint propagation through vector destructuring"
    (is (= [2 3]
           (m/query (let [[a b] [(one-of 1 2 3) 10]]
                      (and (> a 1) a))))))

  (testing "relational constraint between vector elements"
    (is (= [[1 2] [1 3] [2 3]]
           (m/query (let [[x y] [(one-of 1 2 3) (one-of 1 2 3)]]
                      (and (< x y) [x y]))))))

  (testing "arithmetic on destructured vector elements"
    (is (= [30]
           (m/query (let [[x y] [10 20]] (+ x y))))))

  (testing "constraint narrows the original vector"
    (is (= #{[2 10] [3 10]}
           (set (m/query (let [(as v [a b]) [(one-of 1 2 3) 10]]
                           (and (> a 1) v))))))))

;; ════════════════════════════════════════════════════════════════
;; Error cases
;; ════════════════════════════════════════════════════════════════

(deftest destructuring-errors
  (testing "map destructuring on a vector throws"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not a map"
          (m/query (let [{:x x} [1 2 3]] x)))))

  (testing "vector destructuring on a map throws"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not a vector"
          (m/query (let [[a b] {:x 1 :y 2}] a)))))

  (testing "map destructuring with missing key throws"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"key not found"
          (m/query (let [{:z z} {:x 1 :y 2}] z)))))

  (testing "vector destructuring with too many bindings throws"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"index out of bounds"
          (m/query (let [[a b c] [1 2]] c)))))

  (testing "{:keys [...]} gives helpful error"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not supported"
          (m/query (let [{:keys [x]} {:x 1}] x))))))

;; ════════════════════════════════════════════════════════════════
;; Combined / integration tests
;; ════════════════════════════════════════════════════════════════

(deftest destructuring-with-functions
  (testing "destructuring function-returned map"
    (is (= [1]
           (m/query (let [mk (fn [a b] {:x a :y b})
                          {:x x :y y} (mk 1 2)]
                      x)))))

  (testing "destructuring function-returned vector"
    (is (= [10]
           (m/query (let [mk (fn [a b] [a b])
                          [x y] (mk 10 20)]
                      x))))))

(deftest destructuring-nested-collections
  (testing "destructure map with vector value, then access vector"
    (is (= [20]
           (m/query (let [{:items items} {:items [10 20 30]}]
                      (nth items 1))))))

  (testing "destructure vector of maps"
    (is (= ["Alice"]
           (m/query (let [[first-person] [{:name "Alice"} {:name "Bob"}]]
                      (get first-person :name)))))))

(deftest multiple-destructurings
  (testing "multiple destructurings in same let block"
    (is (= [30]
           (m/query (let [{:x x} {:x 10}
                          {:y y} {:y 20}]
                      (+ x y))))))

  (testing "mixed symbol and destructuring bindings"
    (is (= [6]
           (m/query (let [z 3
                          {:x x} {:x (one-of 1 2 3)}]
                      (and (= x z) (+ x z)))))))

  (testing "vector and map destructuring in same let"
    (is (= [15]
           (m/query (let [[a b] [5 10]
                          {:c c} {:c 0}]
                      (+ a b c)))))))

(deftest destructuring-with-domain-constraints
  (testing "one-of elements with cross-element constraint"
    (is (= [[1 2] [1 3] [2 3]]
           (m/query (let [{:x x :y y} {:x (one-of 1 2) :y (one-of 2 3)}]
                      (and (< x y) [x y]))))))

  (testing "between domain through destructuring"
    (is (= [3 4 5]
           (m/query (let [{:x x} {:x (between 1 5)}]
                      (and (> x 2) x))))))

  (testing "constraint on destructured var propagates to return"
    (is (= [2 3]
           (m/query (let [[a] [(one-of 1 2 3)]]
                      (and (> a 1) a)))))))

(deftest destructuring-with-branching
  (testing "destructured var in if condition"
    (is (= [1 10]
           (sort (m/query (let [{:x x} {:x (one-of 1 2 3)}]
                            (if (< x 2) x 10)))))))

  (testing "destructured var in cond"
    (is (= [:small :big]
           (m/query (let [{:x x} {:x (one-of 1 2 3)}]
                      (cond (< x 2) :small
                            :else :big)))))))
