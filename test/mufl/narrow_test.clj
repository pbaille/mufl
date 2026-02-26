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
           (m/query (do (def Person {:name string :age (between 0 150)})
                        (let [p {:name "Alice" :age 30}]
                          (narrow p Person)
                          (get p :name)))))))

  (testing "narrow against def-bound scalar domain"
    (is (= [5]
           (m/query (do (def SmallInt (between 1 10))
                        (let [x 5]
                          (narrow x SmallInt)
                          x))))))

  (testing "narrow against def-bound domain eliminates values"
    (is (= [10]
           (m/query (do (def Person {:name string :age (between 0 150)})
                        (let [p {:name "Alice" :age (one-of 10 200)}]
                          (narrow p Person)
                          (get p :age))))))))

(deftest narrow-and-composition
  (testing "narrow with and-composed schemas"
    (is (= ["Alice"]
           (m/query (do (def Person {:name string :age integer})
                        (def Employee (and Person {:company string}))
                        (let [e {:name "Alice" :age 30 :company "ACME"}]
                          (narrow e Employee)
                          (get e :name)))))))

  (testing "narrow with and applies all sub-constraints"
    (is (= ["ACME"]
           (m/query (do (def HasName {:name string})
                        (def HasAge {:age integer})
                        (def Person (and HasName HasAge))
                        (let [p {:name "Alice" :age 30 :company "ACME"}]
                          (narrow p Person)
                          (get p :company))))))))

(deftest narrow-contradiction
  (testing "narrow with contradictory constraint throws"
    (is (thrown? Exception
                (m/query (do (let [p {:name 42 :age 30}]
                              (narrow p {:name string :age integer})
                              (get p :name)))))))

  (testing "narrow named domain with contradictory value throws"
    (is (thrown? Exception
                (m/query (do (def Person {:name string :age (between 0 150)})
                             (let [p {:name 42 :age 30}]
                               (narrow p Person)
                               (get p :name))))))))

(deftest narrow-one-of
  (testing "narrow with one-of domain"
    (is (= ["admin"]
           (m/query (do (let [x "admin"]
                          (narrow x (one-of "admin" "user" "guest"))
                          x))))))

  (testing "narrow one-of field in map"
    (is (= ["admin" "user"]
           (m/query (do (def Account {:role (one-of "admin" "user" "guest")})
                        (let [a {:role (one-of "admin" "user")}]
                          (narrow a Account)
                          (get a :role)))))))

  (testing "narrow with one-of eliminates invalid values"
    (is (= ["admin"]
           (m/query (do (let [x (one-of "admin" "banned")]
                          (narrow x (one-of "admin" "user" "guest"))
                          x)))))))

(deftest narrow-finite-enumeration
  (testing "narrow def-bound small domain enumerates solutions"
    (is (= #{1 2 3}
           (set (m/query (do (def SmallInt (between 1 3))
                             (let [x (one-of 1 2 3 4 5)]
                               (narrow x SmallInt)
                               x))))))))
