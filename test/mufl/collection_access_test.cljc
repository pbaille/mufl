(ns mufl.collection-access-test
  (:require [clojure.test :refer [deftest testing is]]
            [mufl.core :as m]))

;; ════════════════════════════════════════════════════════════════
;; get — Map key access
;; ════════════════════════════════════════════════════════════════

(deftest get-basic
  (testing "get on literal map"
    (is (= [1] (m/query (let [m {:x 1 :y 2}] (get m :x)))))
    (is (= [2] (m/query (let [m {:x 1 :y 2}] (get m :y))))))

  (testing "get on string-valued map"
    (is (= ["Alice"] (m/query (let [m {:name "Alice" :age 30}] (get m :name)))))
    (is (= [30] (m/query (let [m {:name "Alice" :age 30}] (get m :age))))))

  (testing "get returns full domain"
    (is (= [1 2 3] (m/query (let [m {:x (one-of 1 2 3)}] (get m :x)))))))

(deftest get-with-constraints
  (testing "constraint propagation through get"
    (is (= [2 3]
           (m/query (let [m {:x (one-of 1 2 3) :y 10}]
                      (and (> (get m :x) 1) (get m :x)))))))

  (testing "equality constraint through get"
    (is (= [5]
           (m/query (let [m {:x (one-of 1 5 10)}]
                      (and (= (get m :x) 5) (get m :x)))))))

  (testing "constraint narrows the source map element"
    ;; When we constrain get result, the map element should narrow too
    (is (= [{:x 2 :y 10} {:x 3 :y 10}]
           (m/query (let [m {:x (one-of 1 2 3) :y 10}]
                      (and (> (get m :x) 1) m)))))))

(deftest get-nested
  (testing "nested get — access into nested maps"
    (is (= [42]
           (m/query (let [m {:a {:b 42}}]
                      (get (get m :a) :b))))))

  (testing "deeply nested get"
    (is (= ["hello"]
           (m/query (let [m {:a {:b {:c "hello"}}}]
                      (get (get (get m :a) :b) :c)))))))

(deftest get-errors
  (testing "get on non-map throws"
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) #"not a map"
          (m/query (let [v [1 2 3]] (get v :x))))))

  (testing "get with missing key throws"
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) #"key not found"
          (m/query (let [m {:x 1}] (get m :y))))))

  (testing "get with non-keyword key throws"
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) #"key must be a literal keyword"
          (m/query (let [m {:x 1}] (get m 0)))))))

;; ════════════════════════════════════════════════════════════════
;; nth — Vector index access
;; ════════════════════════════════════════════════════════════════

(deftest nth-basic
  (testing "nth on literal vector"
    (is (= [10] (m/query (let [v [10 20 30]] (nth v 0)))))
    (is (= [20] (m/query (let [v [10 20 30]] (nth v 1)))))
    (is (= [30] (m/query (let [v [10 20 30]] (nth v 2))))))

  (testing "nth returns full domain"
    (is (= [1 2 3] (m/query (let [v [(one-of 1 2 3) 10]] (nth v 0)))))))

(deftest nth-with-constraints
  (testing "constraint propagation through nth"
    (is (= [2 3]
           (m/query (let [v [(one-of 1 2 3) (one-of 4 5)]]
                      (and (> (nth v 0) 1) (nth v 0)))))))

  (testing "nth with equality constraint"
    (is (= [4]
           (m/query (let [v [(one-of 1 2 3) (one-of 4 5)]]
                      (and (= (nth v 1) 4) (nth v 1)))))))

  (testing "constraint narrows source vector element"
    (is (= #{[2 4] [2 5] [3 4] [3 5]}
           (set (m/query (let [v [(one-of 1 2 3) (one-of 4 5)]]
                           (and (> (nth v 0) 1) v))))))))

(deftest nth-errors
  (testing "nth on non-vector throws"
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) #"not a vector"
          (m/query (let [m {:x 1}] (nth m 0))))))

  (testing "nth with out-of-bounds index throws"
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) #"index out of bounds"
          (m/query (let [v [1 2 3]] (nth v 5))))))

  (testing "nth with non-integer index throws"
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) #"index must be a literal integer"
          (m/query (let [v [1 2 3]] (nth v :x)))))))

;; ════════════════════════════════════════════════════════════════
;; Keyword-as-function — (:key m) sugar for (get m :key)
;; ════════════════════════════════════════════════════════════════

(deftest keyword-as-function
  (testing "keyword access on map"
    (is (= [1] (m/query (let [m {:x 1 :y 2}] (:x m)))))
    (is (= [2] (m/query (let [m {:x 1 :y 2}] (:y m))))))

  (testing "keyword access with constraints"
    (is (= [2 3]
           (m/query (let [m {:x (one-of 1 2 3)}]
                      (and (> (:x m) 1) (:x m)))))))

  (testing "nested keyword access"
    (is (= [42]
           (m/query (let [m {:a {:b 42}}]
                      (:b (:a m))))))))

;; ════════════════════════════════════════════════════════════════
;; count — Collection length
;; ════════════════════════════════════════════════════════════════

(deftest count-basic
  (testing "count of vector"
    (is (= [3] (m/query (let [v [10 20 30]] (count v))))))

  (testing "count of map"
    (is (= [2] (m/query (let [m {:x 1 :y 2}] (count m))))))

  (testing "count of empty vector"
    (is (= [0] (m/query (let [v []] (count v)))))))

;; ════════════════════════════════════════════════════════════════
;; Combined / integration tests
;; ════════════════════════════════════════════════════════════════

(deftest combined-get-nth
  (testing "get and nth together"
    ;; Map with a vector value, access both levels
    (is (= [20]
           (m/query (let [m {:items [10 20 30]}]
                      (nth (get m :items) 1))))))

  (testing "nth from vector of maps"
    (is (= ["Alice"]
           (m/query (let [v [{:name "Alice"} {:name "Bob"}]]
                      (get (nth v 0) :name))))))

  (testing "keyword access on vector-of-maps element"
    (is (= ["Bob"]
           (m/query (let [v [{:name "Alice"} {:name "Bob"}]]
                      (:name (nth v 1))))))))

(deftest access-with-arithmetic
  (testing "arithmetic on accessed values"
    (is (= [30]
           (m/query (let [m {:x 10 :y 20}]
                      (+ (get m :x) (get m :y)))))))

  (testing "arithmetic constraint through access"
    (is (= [3]
           (m/query (let [m {:x (one-of 1 2 3 4 5)}]
                      (and (= (+ (get m :x) 7) 10) (get m :x))))))))

(deftest access-with-branching
  (testing "get in if-then-else"
    (is (= [1 10]
           (sort (m/query (let [m {:x (one-of 1 2 3)}]
                            (if (< (get m :x) 2) (get m :x) 10)))))))

  (testing "multiple gets from same map with relational constraint"
    (is (= [{:x 1 :y 2} {:x 1 :y 3} {:x 2 :y 3}]
           (m/query (let [m {:x (one-of 1 2 3) :y (one-of 1 2 3)}]
                      (and (< (get m :x) (get m :y)) m)))))))

(deftest access-fn-returned-collection
  (testing "get on function-returned map"
    (is (= [1]
           (m/query (let [mk (fn [a b] {:x a :y b})
                          m (mk 1 2)]
                      (get m :x))))))

  (testing "nth on function-returned vector"
    (is (= [10]
           (m/query (let [mk (fn [a b] [a b])
                          v (mk 10 20)]
                      (nth v 0)))))))
