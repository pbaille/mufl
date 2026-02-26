(ns mufl.defdomain-test
  (:require [clojure.test :refer [deftest testing is]]
            [mufl.core :as m]
            [mufl.domain :as dom]))

;; ════════════════════════════════════════════════════════════════
;; Type domain algebra tests
;; ════════════════════════════════════════════════════════════════

(deftest type-domain-basics
  (testing "type domain construction"
    (is (= {:kind :type :type :string} dom/string-dom))
    (is (= {:kind :type :type :integer} dom/integer-dom))
    (is (dom/type? dom/string-dom))
    (is (not (dom/type? dom/any)))
    (is (not (dom/type? (dom/single 42)))))

  (testing "type domains are not finite/enumerable"
    (is (not (dom/finite? dom/string-dom)))
    (is (not (dom/finite? dom/integer-dom)))
    (is (not (dom/singleton? dom/string-dom)))
    (is (nil? (dom/members dom/string-dom)))
    (is (nil? (dom/size dom/string-dom))))

  (testing "type domain contains-val?"
    (is (dom/contains-val? dom/string-dom "hello"))
    (is (not (dom/contains-val? dom/string-dom 42)))
    (is (dom/contains-val? dom/integer-dom 42))
    (is (not (dom/contains-val? dom/integer-dom "hello")))
    (is (dom/contains-val? dom/number-dom 42))
    (is (dom/contains-val? dom/number-dom 3.14))
    (is (not (dom/contains-val? dom/number-dom "hello")))
    (is (dom/contains-val? dom/keyword-dom :foo))
    (is (not (dom/contains-val? dom/keyword-dom "foo")))
    (is (dom/contains-val? dom/boolean-dom true))
    (is (dom/contains-val? dom/boolean-dom false))
    (is (not (dom/contains-val? dom/boolean-dom 1)))))

(deftest type-domain-intersect
  (testing "type ∩ any = type"
    (is (= dom/string-dom (dom/intersect dom/string-dom dom/any)))
    (is (= dom/string-dom (dom/intersect dom/any dom/string-dom))))

  (testing "type ∩ void = void"
    (is (= dom/void (dom/intersect dom/string-dom dom/void)))
    (is (= dom/void (dom/intersect dom/void dom/string-dom))))

  (testing "same type ∩ same type = same type"
    (is (= dom/string-dom (dom/intersect dom/string-dom dom/string-dom))))

  (testing "type ∩ singleton (matching)"
    (is (= (dom/single "hello") (dom/intersect dom/string-dom (dom/single "hello"))))
    (is (= (dom/single "hello") (dom/intersect (dom/single "hello") dom/string-dom))))

  (testing "type ∩ singleton (non-matching)"
    (is (= dom/void (dom/intersect dom/string-dom (dom/single 42))))
    (is (= dom/void (dom/intersect (dom/single 42) dom/string-dom))))

  (testing "type ∩ finite (partial match)"
    (is (= (dom/finite #{1 2 3})
           (dom/intersect dom/integer-dom (dom/finite #{1 2 3 "x" "y"})))))

  (testing "type ∩ finite (no match)"
    (is (= dom/void (dom/intersect dom/string-dom (dom/finite #{1 2 3})))))

  (testing "type ∩ different type = void"
    (is (= dom/void (dom/intersect dom/string-dom dom/integer-dom)))
    (is (= dom/void (dom/intersect dom/string-dom dom/keyword-dom))))

  (testing "integer ∩ number = integer (subtype)"
    (is (= dom/integer-dom (dom/intersect dom/integer-dom dom/number-dom)))
    (is (= dom/integer-dom (dom/intersect dom/number-dom dom/integer-dom)))))

(deftest type-domain-unite
  (testing "type ∪ void = type"
    (is (= dom/string-dom (dom/unite dom/string-dom dom/void))))

  (testing "integer ∪ number = number"
    (is (= dom/number-dom (dom/unite dom/integer-dom dom/number-dom)))
    (is (= dom/number-dom (dom/unite dom/number-dom dom/integer-dom))))

  (testing "different types ∪ = any"
    (is (= dom/any (dom/unite dom/string-dom dom/integer-dom)))))

(deftest type-domain-subtract
  (testing "type - same type = void"
    (is (= dom/void (dom/subtract dom/string-dom dom/string-dom))))

  (testing "type - different type = type"
    (is (= dom/string-dom (dom/subtract dom/string-dom dom/integer-dom))))

  (testing "integer - number = void (subtype)"
    (is (= dom/void (dom/subtract dom/integer-dom dom/number-dom))))

  (testing "finite - type = filter out matching"
    (is (= (dom/finite #{1 2 3})
           (dom/subtract (dom/finite #{1 2 3 "x" "y"}) dom/string-dom)))))

;; ════════════════════════════════════════════════════════════════
;; Basic def domain tests (using `def` instead of `defdomain`)
;; ════════════════════════════════════════════════════════════════

(deftest def-basic-constraint
  (testing "basic domain constrains map values"
    (is (= ["Alice"]
           (m/query (do (def Person {:name string :age (between 0 150)})
                        (let [p {:name "Alice" :age 30}]
                          (narrow p Person)
                          (get p :name)))))))

  (testing "domain constraint narrows out-of-domain values"
    (is (= [10]
           (m/query (do (def Person {:name string :age (between 0 150)})
                        (let [p {:name "Alice" :age (one-of 10 200)}]
                          (narrow p Person)
                          (get p :age)))))))

  (testing "domain constraint eliminates incompatible type values"
    (is (= ["Bob"]
           (m/query (do (def Named {:name string})
                        (let [p {:name (one-of "Bob" 42)}]
                          (narrow p Named)
                          (get p :name))))))))

(deftest def-contradiction
  (testing "domain constraint with no valid values throws"
    (is (thrown? Exception
                (m/query (do (def Person {:name string :age (between 0 150)})
                             (let [p {:name 42 :age 30}]
                               (narrow p Person)
                               (get p :name))))))))

(deftest def-with-one-of-fields
  (testing "domain with one-of field narrows correctly"
    (is (= ["admin" "user"]
           (m/query (do (def Account {:role (one-of "admin" "user" "guest")})
                        (let [a {:role (one-of "admin" "user")}]
                          (narrow a Account)
                          (get a :role))))))))

;; ════════════════════════════════════════════════════════════════
;; Constructor destructuring tests (via defn)
;; ════════════════════════════════════════════════════════════════

(deftest defn-destructuring
  (testing "constructor destructuring binds fields and applies constraints"
    (is (= [["Bob" 25]]
           (m/query (do (defn person [name age] {:name (string name) :age (integer age)})
                        (let [(person name age) {:name "Bob" :age 25}]
                          [name age]))))))

  (testing "constructor destructuring narrows values"
    (is (= [["Alice" 30]]
           (m/query (do (defn person [name age]
                          (<= age 150) (>= age 0)
                          {:name (string name) :age (integer age)})
                        (let [(person name age) {:name "Alice" :age (one-of 30 200)}]
                          [name age])))))))

;; ════════════════════════════════════════════════════════════════
;; Domain composition tests
;; ════════════════════════════════════════════════════════════════

(deftest def-composition
  (testing "composed domain with 'and' applies both constraints"
    (is (= ["Acme"]
           (m/query (do (def Person {:name string :age (between 0 150)})
                        (def Employee (and Person {:company string}))
                        (let [e {:name "Alice" :age 30 :company "Acme"}]
                          (narrow e Employee)
                          (get e :company)))))))

  (testing "composed domain narrows from parent"
    (is (= [30]
           (m/query (do (def Person {:name string :age (between 0 150)})
                        (def Employee (and Person {:company string}))
                        (let [e {:name "Alice" :age (one-of 30 200) :company "Acme"}]
                          (narrow e Employee)
                          (get e :age))))))))

;; ════════════════════════════════════════════════════════════════
;; Domain + arithmetic constraints
;; ════════════════════════════════════════════════════════════════

(deftest def-with-arithmetic
  (testing "domain + additional arithmetic constraint"
    (is (= [20 30]
           (m/query (do (def Person {:name string :age (between 0 150)})
                        (let [p {:name "Alice" :age (one-of 10 20 30 200)}]
                          (narrow p Person)
                          (> (:age p) 15)
                          (get p :age)))))))

  (testing "domain with between field narrowed by equality"
    (is (= [25]
           (m/query (do (def Person {:name string :age (between 20 30)})
                        (let [p {:name "Bob" :age (one-of 25 35)}]
                          (narrow p Person)
                          (get p :age))))))))

;; ════════════════════════════════════════════════════════════════
;; Multiple domain constraints on same value
;; ════════════════════════════════════════════════════════════════

(deftest def-multiple-constraints
  (testing "two domain constraints narrow together"
    (is (= [30]
           (m/query (do (def HasName {:name string})
                        (def HasAge {:age (between 0 150)})
                        (let [p {:name "Alice" :age (one-of 30 200)}]
                          (narrow p HasName)
                          (narrow p HasAge)
                          (get p :age))))))))

;; ════════════════════════════════════════════════════════════════
;; Type domain as standalone constraint
;; ════════════════════════════════════════════════════════════════

(deftest type-domain-as-value-in-env
  (testing "string type domain filters one-of"
    (is (= ["hello" "world"]
           (m/query (do (def Stringy {:val string})
                        (let [x {:val (one-of "hello" "world" 42)}]
                          (narrow x Stringy)
                          (get x :val)))))))

  (testing "integer type domain filters one-of"
    (is (= [1 2 3]
           (m/query (do (def IntVal {:val integer})
                        (let [x {:val (one-of 1 2 3 "a" "b")}]
                          (narrow x IntVal)
                          (get x :val))))))))

;; ════════════════════════════════════════════════════════════════
;; New: vector literal as tuple schema
;; ════════════════════════════════════════════════════════════════

(deftest def-vector-tuple-schema
  (testing "vector literal defines tuple domain"
    (is (= [[1 2]]
           (m/query (do (def IntPair [integer integer])
                        (let [v [1 2]]
                          (narrow v IntPair)
                          v))))))

  (testing "vector tuple narrows element types"
    (is (= [[1 "a"] [2 "a"]]
           (m/query (do (def Mixed [integer string])
                        (let [v [(one-of 1 2 "x") (one-of "a" 3)]]
                          (narrow v Mixed)
                          v))))))

  (testing "def scalar domain"
    (is (= #{1 2 3}
           (set (m/query (do (def SmallInt (between 1 3))
                             (let [x (one-of 1 2 3 4 5)]
                               (narrow x SmallInt)
                               x))))))))
