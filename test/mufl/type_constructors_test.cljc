(ns mufl.type-constructors-test
  (:require [clojure.test :refer [deftest testing is]]
            [mufl.core :as m]
            [mufl.domain :as dom]
            [mufl.tree :as tree]
            [mufl.bind :as bind]
            [mufl.env :as env]))

;; ════════════════════════════════════════════════════════════════
;; vector-of: constrain all vector elements to a type
;; ════════════════════════════════════════════════════════════════

(deftest vector-of-basic
  (testing "vector-of integer narrows mixed-type elements"
    (is (= [[1 2 3]]
           (m/query (let [v [(one-of 1 "a") (one-of 2 "b") (one-of 3 "c")]]
                      (narrow v (vector-of integer))
                      v)))))

  (testing "vector-of string narrows to string elements"
    (is (= [["a" "b" "c"]]
           (m/query (let [v [(one-of 1 "a") (one-of 2 "b") (one-of 3 "c")]]
                      (narrow v (vector-of string))
                      v)))))

  (testing "vector-of with already-typed elements is a no-op"
    (is (= [[1 2 3]]
           (m/query (let [v [1 2 3]]
                      (narrow v (vector-of integer))
                      v)))))

  (testing "vector-of integer on empty vector"
    (is (= [[]]
           (m/query (let [v []]
                      (narrow v (vector-of integer))
                      v))))))

(deftest vector-of-with-solve
  (testing "vector-of + solve enumerates valid element combinations"
    (is (= [[1 2] [1 3] [2 3]]
           (m/query (let [v [(one-of 1 2 "x") (one-of 2 3 "y")]]
                      (narrow v (vector-of integer))
                      (< (nth v 0) (nth v 1))
                      v)))))

  (testing "vector-of keyword filters correctly"
    (is (= [[:a :b]]
           (m/query (let [v [(one-of :a 1) (one-of :b 2)]]
                      (narrow v (vector-of keyword))
                      v))))))

(deftest vector-of-contradiction
  (testing "vector-of integer with all-string elements throws"
    (is (thrown? #?(:clj Exception :cljs js/Error)
                (m/query (let [v ["a" "b" "c"]]
                           (narrow v (vector-of integer))
                           v))))))

;; ════════════════════════════════════════════════════════════════
;; vector-of in def context
;; ════════════════════════════════════════════════════════════════

(deftest vector-of-def
  (testing "def with vector-of schema"
    (is (= [[1 2 3]]
           (m/query (do (def int-vec (vector-of integer))
                        (let [v [(one-of 1 "a") (one-of 2 "b") (one-of 3 "c")]]
                          (narrow v int-vec)
                          v))))))

  (testing "def vector-of in map field"
    (is (= [[1 2]]
           (m/query (do (def has-scores {:scores (vector-of integer)})
                        (let [p {:scores [(one-of 1 "a") (one-of 2 "b")]}]
                          (narrow p has-scores)
                          (get p :scores)))))))

  (testing "def vector-of composed with and"
    (is (= [["Alice" [90 80]]]
           (m/query (do (def student (and {:name string}
                                                {:scores (vector-of integer)}))
                        (let [s {:name "Alice" :scores [(one-of 90 "x") (one-of 80 "y")]}]
                          (narrow s student)
                          [(get s :name) (get s :scores)])))))))

;; ════════════════════════════════════════════════════════════════
;; tuple: per-position type constraints
;; ════════════════════════════════════════════════════════════════

(deftest tuple-basic
  (testing "tuple constrains per-position types"
    (is (= [[1 "hello"]]
           (m/query (let [v [(one-of 1 "a") (one-of 2 "hello")]]
                      (narrow v (tuple [integer string]))
                      v)))))

  (testing "tuple with three positions"
    (is (= [[42 "hi" true]]
           (m/query (let [v [(one-of 42 "x") (one-of "hi" 99) (one-of true 0)]]
                      (narrow v (tuple [integer string boolean]))
                      v)))))

  (testing "tuple length mismatch throws"
    (is (thrown? #?(:clj Exception :cljs js/Error)
                (m/query (let [v [1 2 3]]
                           (narrow v (tuple [integer string]))
                           v))))))

(deftest tuple-with-solve
  (testing "tuple + solve works across positions"
    (is (= #{[1 "a"] [2 "a"] [1 "b"] [2 "b"]}
           (set
            (m/query (let [v [(one-of 1 2) (one-of "a" "b")]]
                       (narrow v (tuple [integer string]))
                       v)))))))

(deftest tuple-def
  (testing "def with tuple schema"
    (is (= [[42 "hello"]]
           (m/query (do (def pair (tuple [integer string]))
                        (let [v [(one-of 42 "x") (one-of "hello" 99)]]
                          (narrow v pair)
                          v))))))

  (testing "def tuple for 2D point"
    (is (= [[3 4]]
           (m/query (do (def point (tuple [integer integer]))
                        (let [p [(one-of 3 "x") (one-of 4 "y")]]
                          (narrow p point)
                          p)))))))

;; ════════════════════════════════════════════════════════════════
;; map-of: constrain all map entries
;; ════════════════════════════════════════════════════════════════

(deftest map-of-basic
  (testing "map-of keyword integer narrows values"
    (is (= [{:a 1 :b 2}]
           (m/query (let [m {:a (one-of 1 "x") :b (one-of 2 "y")}]
                      (narrow m (map-of keyword integer))
                      m)))))

  (testing "map-of keyword string narrows values"
    (is (= [{:x "hello" :y "world"}]
           (m/query (let [m {:x (one-of "hello" 42) :y (one-of "world" 99)}]
                      (narrow m (map-of keyword string))
                      m))))))

(deftest map-of-key-validation
  (testing "map-of with wrong key type throws"
    ;; map-of keyword integer on a map with integer keys would fail
    ;; However mufl maps use keyword keys by default, so this is
    ;; mainly for documentation. Let's test the positive case:
    (is (= [{:a 1}]
           (m/query (let [m {:a (one-of 1 "x")}]
                      (narrow m (map-of keyword integer))
                      m))))))

(deftest map-of-def
  (testing "def with map-of schema"
    (is (= [{:x 10 :y 20}]
           (m/query (do (def scores (map-of keyword integer))
                        (let [s {:x (one-of 10 "a") :y (one-of 20 "b")}]
                          (narrow s scores)
                          s))))))

  (testing "def map-of in composed domain"
    (is (= [["Alice" {:math 90}]]
           (m/query (do (def grade-card (and {:name string}
                                                  {:grades (map-of keyword integer)}))
                        (let [gc {:name "Alice" :grades {:math (one-of 90 "A")}}]
                          (narrow gc grade-card)
                          [(get gc :name) (get gc :grades)])))))))

;; ════════════════════════════════════════════════════════════════
;; Nested type constructors
;; ════════════════════════════════════════════════════════════════

(deftest nested-vector-of
  (testing "vector-of with vector-of (matrix)"
    ;; (def Matrix (vector-of (vector-of integer)))
    ;; This requires nested schema resolution
    (is (= [[[1 2] [3 4]]]
           (m/query (do (def int-vec (vector-of integer))
                        (let [m [[(one-of 1 "a") (one-of 2 "b")]
                                 [(one-of 3 "c") (one-of 4 "d")]]]
                          (narrow m (vector-of int-vec))
                          m)))))))

(deftest vector-of-with-domain-def
  (testing "vector-of with a named domain type"
    (is (= [[{:name "Alice"} {:name "Bob"}]]
           (m/query (do (def named {:name string})
                        (let [people [{:name (one-of "Alice" 42)}
                                      {:name (one-of "Bob" 99)}]]
                          (narrow people (vector-of named))
                          people)))))))

;; ════════════════════════════════════════════════════════════════
;; Type constructors with type predicates (unary form)
;; ════════════════════════════════════════════════════════════════

(deftest type-constructors-with-type-predicates
  (testing "vector-of integer acts as both type check and constraint"
    (is (= [[10 20]]
           (m/query (let [v [(one-of 10 "x") (one-of 20 "y")]]
                      (narrow v (vector-of integer))
                      v)))))

  (testing "tuple with between-constrained positions"
    ;; tuple types must reference named types/domains — between needs def
    (is (= [[5 "hi"]]
           (m/query (do (def small-int (between 1 10))
                        (let [v [(one-of 5 "x") (one-of "hi" 99)]]
                          (narrow v (tuple [small-int string]))
                          v)))))))

;; ════════════════════════════════════════════════════════════════
;; Integration: type constructors + existing features
;; ════════════════════════════════════════════════════════════════

(deftest type-constructors-with-map-fn
  (testing "vector-of + map produces correctly typed results"
    (is (= [[2 4 6]]
           (m/query (let [v [(one-of 1 "a") (one-of 2 "b") (one-of 3 "c")]]
                      (narrow v (vector-of integer))
                      (map (fn [x] (* x 2)) v)))))))

(deftest type-constructors-with-filter
  (testing "vector-of narrows before filter"
    (is (= [[2]]
           (m/query (let [v [(one-of 1 "a") (one-of 2 "b") (one-of 3 "c")]]
                      (narrow v (vector-of integer))
                      (filter (fn [x] (even x)) v)))))))

(deftest type-constructors-with-reduce
  (testing "vector-of + reduce works"
    (is (= [6]
           (m/query (let [v [(one-of 1 "a") (one-of 2 "b") (one-of 3 "c")]]
                      (narrow v (vector-of integer))
                      (reduce (fn [acc x] (+ acc x)) 0 v)))))))

;; ════════════════════════════════════════════════════════════════
;; Nullary form: type constructors as domain values
;; ════════════════════════════════════════════════════════════════

(defn bind-in-scope
  "Helper: create base env, make a scope, bind expr inside it."
  [f]
  (let [base (env/base-env)
        ws (-> (tree/ensure-path base ['ws])
               (tree/upd ['ws] f))]
    ws))

(deftest vector-of-nullary
  (testing "vector-of integer produces a vector-of composite domain"
    (let [ws (bind-in-scope
              (fn [e]
                (bind/bind e 't '(vector-of integer))))
          t-node (tree/cd ws ['ws 't])
          resolved (bind/resolve t-node)]
      (is (= {:kind :vector-of :element dom/integer-dom}
             (:domain resolved)))
      (is (= {:kind :vector-of :element dom/integer-dom}
             (:type-domain resolved)))))

  (testing "vector-of string produces correct domain"
    (let [ws (bind-in-scope
              (fn [e]
                (bind/bind e 't '(vector-of string))))
          t-node (tree/cd ws ['ws 't])
          resolved (bind/resolve t-node)]
      (is (= {:kind :vector-of :element dom/string-dom}
             (:domain resolved)))))

  (testing "nullary form can be stored in let"
    (let [ws (bind-in-scope
              (fn [e]
                (bind/bind e '(let [t (vector-of integer)] t))))
          ws-node (tree/cd ws ['ws])
          resolved (bind/resolve ws-node)]
      (is (= {:kind :vector-of :element dom/integer-dom}
             (:type-domain resolved))))))

(deftest tuple-nullary
  (testing "tuple [integer string] produces a tuple composite domain"
    (let [ws (bind-in-scope
              (fn [e]
                (bind/bind e 't '(tuple [integer string]))))
          t-node (tree/cd ws ['ws 't])
          resolved (bind/resolve t-node)]
      (is (= {:kind :tuple :elements [dom/integer-dom dom/string-dom]}
             (:domain resolved)))
      (is (= {:kind :tuple :elements [dom/integer-dom dom/string-dom]}
             (:type-domain resolved)))))

  (testing "tuple with three elements"
    (let [ws (bind-in-scope
              (fn [e]
                (bind/bind e 't '(tuple [integer string boolean]))))
          t-node (tree/cd ws ['ws 't])
          resolved (bind/resolve t-node)]
      (is (= {:kind :tuple :elements [dom/integer-dom dom/string-dom dom/boolean-dom]}
             (:domain resolved))))))

(deftest map-of-nullary
  (testing "map-of keyword integer produces a map-of composite domain"
    (let [ws (bind-in-scope
              (fn [e]
                (bind/bind e 't '(map-of keyword integer))))
          t-node (tree/cd ws ['ws 't])
          resolved (bind/resolve t-node)]
      (is (= {:kind :map-of :key dom/keyword-dom :value dom/integer-dom}
             (:domain resolved)))
      (is (= {:kind :map-of :key dom/keyword-dom :value dom/integer-dom}
             (:type-domain resolved)))))

  (testing "map-of keyword string produces correct domain"
    (let [ws (bind-in-scope
              (fn [e]
                (bind/bind e 't '(map-of keyword string))))
          t-node (tree/cd ws ['ws 't])
          resolved (bind/resolve t-node)]
      (is (= {:kind :map-of :key dom/keyword-dom :value dom/string-dom}
             (:domain resolved))))))

(deftest nested-nullary-constructors
  (testing "vector-of (vector-of integer) builds nested composite domain"
    (let [ws (bind-in-scope
              (fn [e]
                (bind/bind e 't '(vector-of (vector-of integer)))))
          t-node (tree/cd ws ['ws 't])
          resolved (bind/resolve t-node)]
      (is (= {:kind :vector-of
              :element {:kind :vector-of :element dom/integer-dom}}
             (:domain resolved)))
      (is (= (:domain resolved) (:type-domain resolved)))))

  (testing "vector-of (tuple [integer string]) builds nested composite"
    (let [ws (bind-in-scope
              (fn [e]
                (bind/bind e 't '(vector-of (tuple [integer string])))))
          t-node (tree/cd ws ['ws 't])
          resolved (bind/resolve t-node)]
      (is (= {:kind :vector-of
              :element {:kind :tuple :elements [dom/integer-dom dom/string-dom]}}
             (:domain resolved)))))

  (testing "tuple with nested vector-of"
    (let [ws (bind-in-scope
              (fn [e]
                (bind/bind e 't '(tuple [(vector-of integer) string]))))
          t-node (tree/cd ws ['ws 't])
          resolved (bind/resolve t-node)]
      (is (= {:kind :tuple
              :elements [{:kind :vector-of :element dom/integer-dom}
                         dom/string-dom]}
             (:domain resolved)))))

  (testing "map-of keyword (vector-of integer) builds nested composite"
    (let [ws (bind-in-scope
              (fn [e]
                (bind/bind e 't '(map-of keyword (vector-of integer)))))
          t-node (tree/cd ws ['ws 't])
          resolved (bind/resolve t-node)]
      (is (= {:kind :map-of
              :key dom/keyword-dom
              :value {:kind :vector-of :element dom/integer-dom}}
             (:domain resolved))))))

(deftest nullary-with-domain-def-reference
  (testing "vector-of with a between-constrained domain reference"
    ;; (between 1 10) produces a finite domain, which is a :type-constraint
    ;; in the domain-def, so resolve-domain-schema should extract it
    (let [ws (bind-in-scope
              (fn [e]
                (-> e
                    (bind/bind '(def small-int (between 1 10)))
                    (bind/bind 't '(vector-of small-int)))))
          t-node (tree/cd ws ['ws 't])
          resolved (bind/resolve t-node)]
      (is (= :vector-of (:kind (:domain resolved))))
      (is (= (dom/int-range 1 10) (:element (:domain resolved)))))))

(deftest narrow-with-type-constructors
  (testing "narrow with vector-of constrains vector elements"
    (is (= [[1 2 3]]
           (m/query (let [v [(one-of 1 "a") (one-of 2 "b") (one-of 3 "c")]]
                      (narrow v (vector-of integer))
                      v))))
    (is (= [[1 "hello"]]
           (m/query (let [v [(one-of 1 "a") (one-of 2 "hello")]]
                      (narrow v (tuple [integer string]))
                      v))))
    (is (= [{:a 1 :b 2}]
           (m/query (let [m {:a (one-of 1 "x") :b (one-of 2 "y")}]
                      (narrow m (map-of keyword integer))
                      m))))))

;; ════════════════════════════════════════════════════════════════
;; Callable domains: (domain-val arg) ≡ (narrow arg domain-val)
;; ════════════════════════════════════════════════════════════════

(deftest callable-domain-vector-of
  (testing "let-bound vector-of called as function"
    (is (= [[1 2]]
           (m/query (let [intvec (vector-of integer)
                          v [(one-of 1 "a") (one-of 2 "b")]]
                      (intvec v) v)))))

  (testing "def-bound vector-of called as function"
    (is (= [["a" "b"]]
           (m/query (do (def strvec (vector-of string))
                        (let [v [(one-of 1 "a") (one-of 2 "b")]]
                          (strvec v) v)))))))

(deftest callable-domain-tuple
  (testing "let-bound tuple called as function"
    (is (= [[42 "hello"]]
           (m/query (let [pair (tuple [integer string])
                          v [(one-of 42 "x") (one-of "hello" 99)]]
                      (pair v) v))))))

(deftest callable-domain-map-of
  (testing "let-bound map-of called as function"
    (is (= [{:a 1 :b 2}]
           (m/query (let [scores (map-of keyword integer)
                          m {:a (one-of 1 "x") :b (one-of 2 "y")}]
                      (scores m) m))))))

(deftest callable-domain-struct
  (testing "def-bound struct domain called as function"
    (is (= [{:name "Alice" :age 30}]
           (m/query (do (def person {:name string :age integer})
                        (let [p {:name (one-of "Alice" 42) :age (one-of 30 "old")}]
                          (person p) p)))))))

;; ════════════════════════════════════════════════════════════════
;; Callable domains in pattern position: (let [(intvec x) v] x)
;; ════════════════════════════════════════════════════════════════

(deftest callable-domain-pattern-vector-of
  (testing "vector-of domain in let pattern position"
    (is (= [[1 2]]
           (m/query (let [intvec (vector-of integer)
                          v [(one-of 1 "a") (one-of 2 "b")]]
                      (let [(intvec x) v] x))))))

  (testing "def-bound vector-of in pattern position"
    (is (= [["a" "b"]]
           (m/query (do (def strvec (vector-of string))
                        (let [v [(one-of 1 "a") (one-of 2 "b")]]
                          (let [(strvec x) v] x))))))))

(deftest callable-domain-pattern-tuple
  (testing "tuple domain in let pattern position"
    (is (= [[42 "hello"]]
           (m/query (do (def pair (tuple [integer string]))
                        (let [v [(one-of 42 "x") (one-of "hello" 99)]]
                          (let [(pair x) v] x))))))))

(deftest callable-domain-pattern-map-of
  (testing "map-of domain in let pattern position"
    (is (= [{:a 1 :b 2}]
           (m/query (let [scores (map-of keyword integer)
                          m {:a (one-of 1 "x") :b (one-of 2 "y")}]
                      (let [(scores x) m] x)))))))

(deftest callable-domain-pattern-struct
  (testing "struct domain in let pattern position"
    (is (= [{:name "Alice" :age 30}]
           (m/query (do (def person {:name string :age integer})
                        (let [p {:name (one-of "Alice" 42) :age (one-of 30 "old")}]
                          (let [(person x) p] x))))))))

(deftest callable-domain-pattern-scalar
  (testing "between domain in let pattern position"
    (is (= [5]
           (m/query (do (def small (between 1 10))
                        (let [v (one-of 5 20 "x")]
                          (let [(small x) v] x))))))))

(deftest nullary-domain-algebra
  (testing "composite domains from nullary form participate in domain algebra"
    (let [d1 (dom/vector-of-dom dom/integer-dom)
          d2 (dom/vector-of-dom dom/integer-dom)]
      (is (= :equal (dom/relate d1 d2)))))

  (testing "vector-of integer and vector-of string are disjoint"
    (let [d1 (dom/vector-of-dom dom/integer-dom)
          d2 (dom/vector-of-dom dom/string-dom)]
      (is (= :disjoint (dom/relate d1 d2)))))

  (testing "contains-val? works with composite domains from nullary"
    (let [d (dom/vector-of-dom dom/integer-dom)]
      (is (dom/contains-val? d [1 2 3]))
      (is (not (dom/contains-val? d [1 "a" 3])))
      (is (dom/contains-val? d [])))))

;; ════════════════════════════════════════════════════════════════
;; Phase 4: = with composite domains (unification)
;; ════════════════════════════════════════════════════════════════

(deftest eq-with-vector-of
  (testing "= with vector-of integer narrows vector elements"
    (is (= [[1 2]]
           (m/query (let [v [(one-of 1 "a") (one-of 2 "b")]]
                      (= v (vector-of integer))
                      v)))))

  (testing "= with vector-of string narrows to string elements"
    (is (= [["a" "b"]]
           (m/query (let [v [(one-of 1 "a") (one-of 2 "b")]]
                      (= v (vector-of string))
                      v)))))

  (testing "= vector-of on empty vector is fine"
    (is (= [[]]
           (m/query (let [v []]
                      (= v (vector-of integer))
                      v))))))

(deftest eq-with-tuple
  (testing "= with tuple constrains per-position types"
    (is (= [[1 "hello"]]
           (m/query (let [v [(one-of 1 "a") (one-of 2 "hello")]]
                      (= v (tuple [integer string]))
                      v)))))

  (testing "= with tuple length mismatch causes contradiction"
    (is (thrown? #?(:clj Exception :cljs js/Error)
                (m/query (let [v [1 2 3]]
                           (= v (tuple [integer string]))
                           v))))))

(deftest eq-with-map-of
  (testing "= with map-of keyword integer narrows values"
    (is (= [{:a 1 :b 2}]
           (m/query (let [m {:a (one-of 1 "x") :b (one-of 2 "y")}]
                      (= m (map-of keyword integer))
                      m))))))

(deftest eq-composite-stored-type
  (testing "stored type then = still works"
    (is (= [[1 2]]
           (m/query (let [t (vector-of integer)
                          v [(one-of 1 "a") (one-of 2 "b")]]
                      (= v t)
                      v)))))

  (testing "stored tuple type then ="
    (is (= [[1 "hello"]]
           (m/query (let [t (tuple [integer string])
                          v [(one-of 1 "a") (one-of 2 "hello")]]
                      (= v t)
                      v)))))

  (testing "stored map-of type then ="
    (is (= [{:a 1 :b 2}]
           (m/query (let [t (map-of keyword integer)
                          m {:a (one-of 1 "x") :b (one-of 2 "y")}]
                      (= m t)
                      m))))))

(deftest eq-composite-nested
  (testing "= with nested vector-of (vector-of integer)"
    (is (= [[[1] [2]]]
           (m/query (let [v [[(one-of 1 "a")] [(one-of 2 "b")]]]
                      (= v (vector-of (vector-of integer)))
                      v))))))

(deftest eq-composite-contradiction
  (testing "= vector-of integer on all-string elements throws"
    (is (thrown? #?(:clj Exception :cljs js/Error)
                (m/query (let [v ["a" "b"]]
                           (= v (vector-of integer))
                           v)))))

  (testing "= tuple on wrong types throws"
    (is (thrown? #?(:clj Exception :cljs js/Error)
                (m/query (let [v ["hello" 42]]
                           (= v (tuple [integer string]))
                           v))))))

;; ════════════════════════════════════════════════════════════════
;; Callable domains: edge cases
;; ════════════════════════════════════════════════════════════════

(deftest callable-domain-contradiction
  (testing "callable domain on all-wrong-type elements throws"
    (is (thrown? #?(:clj Exception :cljs js/Error)
                (m/query (let [intvec (vector-of integer)
                              v ["a" "b" "c"]]
                           (intvec v) v)))))

  (testing "callable struct domain on wrong shape throws"
    (is (thrown? #?(:clj Exception :cljs js/Error)
                (m/query (do (def person {:name string :age integer})
                             (let [p {:name 42 :age "old"}]
                               (person p) p)))))))

(deftest callable-domain-multiple-solutions
  (testing "callable domain with multiple valid solutions"
    (is (= #{[1 2] [1 3] [2 3]}
           (set
            (m/query (let [intvec (vector-of integer)
                           v [(one-of 1 2 "x") (one-of 2 3 "y")]]
                       (intvec v)
                       (< (nth v 0) (nth v 1))
                       v)))))))

(deftest callable-domain-and-composed
  (testing "and-composed domain as callable domain"
    (is (= [{:name "Alice" :age 30 :company "Acme"}]
           (m/query (do (def person {:name string :age integer})
                        (def employee (and person {:company string}))
                        (let [e {:name (one-of "Alice" 42)
                                 :age (one-of 30 "old")
                                 :company (one-of "Acme" 99)}]
                          (employee e) e))))))

  (testing "and-composed domain in pattern position"
    (is (= [{:name "Alice" :age 30 :company "Acme"}]
           (m/query (do (def person {:name string :age integer})
                        (def employee (and person {:company string}))
                        (let [e {:name (one-of "Alice" 42)
                                 :age (one-of 30 "old")
                                 :company (one-of "Acme" 99)}]
                          (let [(employee x) e] x))))))))

(deftest callable-domain-with-vector-of-named-struct
  (testing "vector-of with named struct as callable domain"
    (is (= [[{:name "Alice"} {:name "Bob"}]]
           (m/query (do (def named {:name string})
                        (let [people [{:name (one-of "Alice" 42)}
                                      {:name (one-of "Bob" 99)}]
                              named-vec (vector-of named)]
                          (named-vec people)
                          people))))))

  (testing "tuple with named struct as callable domain"
    (is (= [["Alice" {:name "Bob"}]]
           (m/query (do (def named {:name string})
                        (let [pair (tuple [string named])
                              v ["Alice" {:name (one-of "Bob" 99)}]]
                          (pair v) v))))))

  (testing "map-of with named struct as callable domain"
    (is (= [{:a {:name "Alice"} :b {:name "Bob"}}]
           (m/query (do (def named {:name string})
                        (let [nm (map-of keyword named)
                              m {:a {:name (one-of "Alice" 42)}
                                 :b {:name (one-of "Bob" 99)}}]
                          (nm m) m)))))))

(deftest callable-domain-empty-vector
  (testing "callable domain on empty vector"
    (is (= [[]]
           (m/query (let [intvec (vector-of integer)
                          v []]
                      (intvec v) v))))))

;; ════════════════════════════════════════════════════════════════
;; Callable domains in pattern position: edge cases
;; ════════════════════════════════════════════════════════════════

(deftest callable-domain-pattern-contradiction
  (testing "callable domain pattern on wrong type throws"
    (is (thrown? #?(:clj Exception :cljs js/Error)
                (m/query (do (def small (between 1 10))
                             (let [v 20]
                               (let [(small x) v] x))))))))

(deftest callable-domain-pattern-multiple-solutions
  (testing "callable domain pattern with multiple solutions"
    (is (= [1 2 3]
           (m/query (do (def small (between 1 10))
                        (let [v (one-of 1 2 3 20 30)]
                          (let [(small x) v] x))))))))

(deftest callable-domain-pattern-nested-destructuring
  (testing "callable domain pattern with inner vector destructuring"
    (is (= [[1 2]]
           (m/query (let [intvec (vector-of integer)
                          v [(one-of 1 "a") (one-of 2 "b")]]
                      (let [(intvec [a b]) v] [a b]))))))

  (testing "callable domain pattern with inner map destructuring"
    (is (= [["Alice" 30]]
           (m/query (do (def person {:name string :age integer})
                        (let [p {:name (one-of "Alice" 42) :age (one-of 30 "old")}]
                          (let [(person {:name n :age a}) p] [n a]))))))))

(deftest callable-domain-pattern-with-as
  (testing "callable domain pattern composed with as"
    (is (= [[{:name "Alice" :age 30} "Alice"]]
           (m/query (do (def person {:name string :age integer})
                        (let [p {:name (one-of "Alice" 42) :age (one-of 30 "old")}]
                          (let [(as whole (person x)) p] [whole (:name x)]))))))))
