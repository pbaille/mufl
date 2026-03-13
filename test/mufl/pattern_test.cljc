(ns mufl.pattern-test
  "Comprehensive tests for pattern destructuring.
   Merged from destructuring_test.clj, destructure_review_test.clj,
   bind_destructuring_test.clj, and dot_syntax_test.clj."
  (:require [clojure.test :refer [deftest testing is]]
            [mufl.core :as m]))

;; ════════════════════════════════════════════════════════════════════════
;; FROM destructuring_test.clj
;; ════════════════════════════════════════════════════════════════════════

;; ── Map destructuring ──────────────────────────────────────────

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

;; ── Vector destructuring ───────────────────────────────────────

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

;; ── Error cases ────────────────────────────────────────────────

(deftest destructuring-errors
  (testing "map destructuring on a vector throws"
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) #"not a map"
          (m/query (let [{:x x} [1 2 3]] x)))))

  (testing "vector destructuring on a map throws"
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) #"not a vector"
          (m/query (let [[a b] {:x 1 :y 2}] a)))))

  (testing "map destructuring with missing key throws"
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) #"key not found"
          (m/query (let [{:z z} {:x 1 :y 2}] z)))))

  (testing "vector destructuring with too many bindings throws"
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) #"index out of bounds"
          (m/query (let [[a b c] [1 2]] c)))))

  (testing "{:keys [...]} gives helpful error"
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) #"not supported"
          (m/query (let [{:keys [x]} {:x 1}] x))))))

;; ── Combined / integration ────────────────────────────────────

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

(deftest multiple-destructurings-basic
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

;; ════════════════════════════════════════════════════════════════════════
;; FROM bind_destructuring_test.clj
;; ════════════════════════════════════════════════════════════════════════

;; ── ks ─────────────────────────────────────────────────────────

(deftest ks-basic
  (testing "ks extracts keys by symbol name"
    (is (= [3] (m/query (let [(ks x y) {:x 1 :y 2}] (+ x y))))))

  (testing "ks with single key"
    (is (= [42] (m/query (let [(ks x) {:x 42 :y 99}] x)))))

  (testing "ks with domain values"
    (is (= [2 3]
           (m/query (let [(ks x) {:x (one-of 1 2 3)}]
                      (and (> x 1) x))))))

  (testing "ks with three keys"
    (is (= [6] (m/query (let [(ks a b c) {:a 1 :b 2 :c 3}]
                           (+ a (+ b c))))))))

;; ── as ─────────────────────────────────────────────────────────

(deftest as-basic
  (testing "as binds the whole value and destructures inner pattern"
    (is (= [1]
           (m/query (let [(as m (ks x y)) {:x 1 :y 2}] x)))))

  (testing "as returns the whole value"
    (is (= [{:x 1 :y 2}]
           (m/query (let [(as m (ks x y)) {:x 1 :y 2}] m)))))

  (testing "as with vector inner pattern"
    (is (= [10]
           (m/query (let [(as v [a b c]) [10 20 30]] a)))))

  (testing "as returns whole vector"
    (is (= [[10 20 30]]
           (m/query (let [(as v [a b c]) [10 20 30]] v))))))

;; ── or pattern ─────────────────────────────────────────────────

(deftest or-pattern
  (testing "or in pattern position tries first pattern, falls back to second"
    (is (= [42]
           (m/query (let [(or (ks x) [x]) {:x 42}] x))))))

;; ── Constructor destructuring ──────────────────────────────────

(deftest domain-destructuring
  (testing "defn constructor destructuring extracts fields"
    (is (= [1]
           (m/query (do (defn point [x y] {:x (integer x) :y (integer y)})
                        (let [(point a b) {:x 1 :y 2}] a))))))

  (testing "defn constructor destructuring returns both fields"
    (is (= [[1 2]]
           (m/query (do (defn point [x y] {:x (integer x) :y (integer y)})
                        (let [(point a b) {:x 1 :y 2}] [a b]))))))

  (testing "defn constructor destructuring narrows values"
    (is (= [[1 10] [2 10] [3 10]]
           (m/query (do (defn small-point [x y]
                          (and (>= x 1) (<= x 3)
                               {:x (integer x) :y (integer y)}))
                        (let [(small-point a b) {:x (one-of 0 1 2 3 4) :y 10}]
                          [a b]))))))

  (testing "domain as expression constraint still works"
    (is (= [1 2 3]
           (m/query (def small (between 1 3))
                    (let [x (one-of 0 1 2 3 4)]
                      (and (narrow x small) x)))))))

;; ── Composition ────────────────────────────────────────────────

(deftest nested-destructuring
  (testing "ks inside vector pattern"
    (is (= [1]
           (m/query (let [[(ks x) b] [{:x 1} 20]] x)))))

  (testing "as with ks inside"
    (is (= [{:x 1 :y 2}]
           (m/query (let [(as whole (ks x y)) {:x 1 :y 2}] whole)))))

  (testing "dot rest with nested ks on head"
    (is (= [1]
           (m/query (let [[h . remaining] [{:x 1} 2 3]
                          (ks x) h]
                      x))))))

;; ════════════════════════════════════════════════════════════════════════
;; FROM dot_syntax_test.clj
;; ════════════════════════════════════════════════════════════════════════

;; ── Vector dot syntax ──────────────────────────────────────────

(deftest vector-dot-basic
  (testing "dot rest captures remaining elements"
    (is (= [[3 4 5]]
           (m/query (let [[a b . xs] [1 2 3 4 5]] xs)))))

  (testing "dot rest with first element"
    (is (= [[10 [20 30]]]
           (m/query (let [[a . xs] [10 20 30]] [a xs])))))

  (testing "dot rest returns two-element rest"
    (is (= [[2 3]]
           (m/query (let [[a . xs] [1 2 3]] xs)))))

  (testing "dot rest returns single-element rest"
    (is (= [[20]]
           (m/query (let [[a . xs] [10 20]] xs)))))

  (testing "dot rest returns empty vector when nothing left"
    (is (= [[]]
           (m/query (let [[a . xs] [42]] xs))))))

(deftest vector-dot-bare
  (testing "bare dot ignores rest (no binding after dot)"
    (is (= [[1 2]]
           (m/query (let [[a b .] [1 2 3 4]] [a b]))))))

(deftest vector-dot-with-as
  (testing "dot syntax + as operator binds both rest and whole"
    (is (= [[[3 4 5] [1 2 3 4 5]]]
           (m/query (let [(as v [a b . xs]) [1 2 3 4 5]] [xs v])))))

  (testing "as operator works with vector pattern"
    (is (= [[10 20]]
           (m/query (let [(as v [a b]) [10 20]] v))))))

(deftest vector-dot-nested-rest
  (testing "dot rest with nested vector destructuring"
    (is (= [[3 4]]
           (m/query (let [[a b . [c d]] [1 2 3 4]] [c d])))))

  (testing "dot rest with nested [h . t] on rest"
    (is (= [3]
           (m/query (let [[a b . [h . t]] [1 2 3 4 5]] h)))))

  (testing "dot rest element count"
    (is (= [3]
           (m/query (let [[a b . xs] [1 2 3 4 5]] (count xs)))))))

(deftest vector-dot-with-constraints
  (testing "constraint on positional element with dot rest"
    (is (= [2 3]
           (m/query (let [[a . xs] [(one-of 1 2 3) 10 20]]
                      (and (> a 1) a))))))

  (testing "positional elements + rest coexist"
    (is (= [1]
           (m/query (let [[a b . xs] [1 2 3 4 5]] a)))))

  (testing "rest has correct elements"
    (is (= [3]
           (m/query (let [[a b . xs] [1 2 3 4 5]
                          [c d e] xs]
                      c))))))

;; ── Head/middle/tail decomposition ─────────────────────────────

(deftest vector-dot-head-middle-tail
  (testing "[a . mid c] — basic head/middle/last"
    (is (= [[1 [2 3 4] 5]]
           (m/query (let [[a . mid c] [1 2 3 4 5]] [a mid c])))))

  (testing "[a b . mid c d] — two head, two tail"
    (is (= [[1 2 [3 4 5] 6 7]]
           (m/query (let [[a b . mid c d] [1 2 3 4 5 6 7]] [a b mid c d])))))

  (testing "[. mid c] — no head, middle + tail"
    (is (= [[[1 2 3 4] 5]]
           (m/query (let [[. mid c] [1 2 3 4 5]] [mid c])))))

  (testing "[a . mid c] on exact-fit (empty middle)"
    (is (= [[1 [] 2]]
           (m/query (let [[a . mid c] [1 2]] [a mid c])))))

  (testing "middle count correct"
    (is (= [3]
           (m/query (let [[a . mid c] [1 2 3 4 5]] (count mid)))))))

(deftest vector-dot-head-middle-tail-with-constraints
  (testing "constraint on head with tail positionals"
    (is (= [2 3]
           (m/query (let [[a . mid c] [(one-of 1 2 3) 10 20 99]]
                      (and (> a 1) a))))))

  (testing "constraint on tail positional"
    (is (= [[1 [2 3] 4]]
           (m/query (let [[a . mid c] [1 2 3 (one-of 4 5 6)]]
                      (and (< c 5) [a mid c])))))))

(deftest vector-dot-head-middle-tail-with-as
  (testing "(as v [a . mid c]) — as wrapping head/middle/tail"
    (is (= [[[1 2 3 4 5] 1 [2 3 4] 5]]
           (m/query (let [(as v [a . mid c]) [1 2 3 4 5]]
                      [v a mid c]))))))

;; ── General map destructuring ──────────────────────────────────

(deftest map-general-basic
  (testing "general map pattern with symbol values"
    (is (= [[1 2]]
           (m/query (let [{:x a :y b} {:x 1 :y 2}] [a b])))))

  (testing "single key general pattern"
    (is (= [42]
           (m/query (let [{:x a} {:x 42 :y 99}] a)))))

  (testing "general map pattern with nested vector destructuring"
    (is (= [[1 2 3]]
           (m/query (let [{:x a :y [b c]} {:x 1 :y [2 3]}] [a b c])))))

  (testing "general map pattern with nested ks"
    (is (= [10]
           (m/query (let [{:x (ks y)} {:x {:y 10}}] y))))))

(deftest map-general-with-as
  (testing "general map pattern with as operator"
    (is (= [{:x 1 :y 2}]
           (m/query (let [(as m {:x a}) {:x 1 :y 2}] m))))))

;; ── Map dot syntax ─────────────────────────────────────────────

(deftest map-dot-basic
  (testing "dot rest captures remaining keys"
    (is (= [{:y 2 :z 3}]
           (m/query (let [{:x a . rest} {:x 1 :y 2 :z 3}] rest)))))

  (testing "dot rest with multiple named keys"
    (is (= [{:z 3}]
           (m/query (let [{:x a :y b . rest} {:x 1 :y 2 :z 3}] rest)))))

  (testing "dot rest with no remaining keys gives empty map"
    (is (= [{}]
           (m/query (let [{:x a :y b . rest} {:x 1 :y 2}] rest))))))

(deftest map-dot-nested-rest
  (testing "dot rest with nested ks destructuring"
    (is (= [[1 2 3]]
           (m/query (let [{:x a . (ks y z)} {:x 1 :y 2 :z 3}] [a y z]))))))

(deftest map-dot-with-as
  (testing "dot rest + as operator"
    (is (= [[{:y 2 :z 3} {:x 1 :y 2 :z 3}]]
           (m/query (let [(as m {:x a . rest}) {:x 1 :y 2 :z 3}] [rest m]))))))

(deftest map-dot-with-constraints
  (testing "constraint on destructured value with dot rest"
    (is (= [2 3]
           (m/query (let [{:x a . rest} {:x (one-of 1 2 3) :y 10}]
                      (and (> a 1) a)))))))

;; ── drop and dissoc primitives ─────────────────────────────────

(deftest drop-primitive
  (testing "drop 0 returns all elements"
    (is (= [[1 2 3]]
           (m/query (let [v [1 2 3]] (drop 0 v))))))

  (testing "drop 1 same as rest"
    (is (= [[2 3]]
           (m/query (let [v [1 2 3]] (drop 1 v))))))

  (testing "drop 2 returns from index 2"
    (is (= [[3 4 5]]
           (m/query (let [v [1 2 3 4 5]] (drop 2 v))))))

  (testing "drop all returns empty vector"
    (is (= [[]]
           (m/query (let [v [1 2]] (drop 2 v)))))))

(deftest dissoc-primitive
  (testing "dissoc single key"
    (is (= [{:y 2}]
           (m/query (let [m {:x 1 :y 2}] (dissoc m :x))))))

  (testing "dissoc multiple keys"
    (is (= [{:z 3}]
           (m/query (let [m {:x 1 :y 2 :z 3}] (dissoc m :x :y))))))

  (testing "dissoc all keys"
    (is (= [{}]
           (m/query (let [m {:x 1 :y 2}] (dissoc m :x :y)))))))

;; ════════════════════════════════════════════════════════════════════════
;; FROM destructure_review_test.clj
;; ════════════════════════════════════════════════════════════════════════

;; ── 1. Empty patterns ──────────────────────────────────────────

(deftest empty-vector-pattern
  (testing "[] empty vector pattern binds nothing, body still works"
    (is (= [42] (m/query (let [[] [1 2 3]] 42)))))

  (testing "[] empty vector pattern with domain value"
    (is (= [99] (m/query (let [[] [(one-of 1 2 3)]] 99))))))

(deftest empty-map-pattern
  (testing "{} empty map pattern binds nothing, body still works"
    (is (= [42] (m/query (let [{} {:x 1 :y 2}] 42)))))

  (testing "{} empty map pattern with domain value"
    (is (= [99] (m/query (let [{} {:x (one-of 1 2)}] 99))))))

;; ── 2. Single-element patterns ─────────────────────────────────

(deftest single-element-vector
  (testing "[x] single element vector"
    (is (= [42] (m/query (let [[x] [42]] x)))))

  (testing "[x] with domain"
    (is (= [1 2 3] (m/query (let [[x] [(one-of 1 2 3)]] x)))))

  (testing "[x] with constraint"
    (is (= [2 3] (m/query (let [[x] [(one-of 1 2 3)]]
                             (and (> x 1) x)))))))

(deftest single-key-map
  (testing "{:x x} single key map"
    (is (= [1] (m/query (let [{:x x} {:x 1 :y 2}] x)))))

  (testing "{:x x} with domain"
    (is (= [1 2 3] (m/query (let [{:x x} {:x (one-of 1 2 3) :y 10}] x)))))

  (testing "{:x x} with constraint"
    (is (= [2 3] (m/query (let [{:x x} {:x (one-of 1 2 3) :y 10}]
                             (and (> x 1) x)))))))

;; ── 3. Deeply nested patterns ──────────────────────────────────

(deftest deeply-nested-map-vector
  (testing "map containing vector: {:x [a b]}"
    (is (= [[1 2]] (m/query (let [{:x [a b]} {:x [1 2]}] [a b])))))

  (testing "map containing vector with dot rest: {:x [a . xs]}"
    (is (= [[1 [2 3]]]
           (m/query (let [{:x [a . xs]} {:x [1 2 3]}] [a xs])))))

  (testing "map nested in map: {:x {:y z}}"
    (is (= [42] (m/query (let [{:x {:y z}} {:x {:y 42}}] z)))))

  (testing "vector nested in vector: [[a b] c]"
    (is (= [[1 2 3]]
           (m/query (let [[[a b] c] [[1 2] 3]] [a b c])))))

  (testing "three levels: {:x {:y [a b . c]}}"
    (is (= [[1 2 [3 4]]]
           (m/query (let [{:x {:y [a b . c]}} {:x {:y [1 2 3 4]}}]
                      [a b c]))))))

(deftest deep-constraint-propagation
  (testing "constraint on deeply destructured var propagates"
    (is (= [2 3]
           (m/query (let [{:x {:y [a b]}} {:x {:y [(one-of 1 2 3) 10]}}]
                      (and (> a 1) a))))))

  (testing "constraint through nested map+vector with as"
    (is (= [{:x [2 10]} {:x [3 10]}]
           (m/query (let [(as m {:x [a b]}) {:x [(one-of 1 2 3) 10]}]
                      (and (> a 1) m)))))))

;; ── 4. Dot rest edge cases ─────────────────────────────────────

(deftest vector-dot-at-start
  (testing "[. xs] dot at position 0 — everything goes to rest"
    (is (= [[1 2 3]]
           (m/query (let [[. xs] [1 2 3]] xs)))))

  (testing "[. xs] with empty vector"
    (is (= [[]]
           (m/query (let [[. xs] []] xs))))))

(deftest vector-bare-dot-only
  (testing "[.] bare dot only — ignores everything"
    (is (= [42] (m/query (let [[.] [1 2 3]] 42))))))

(deftest vector-dot-rest-empty
  (testing "[a . xs] when rest is empty"
    (is (= [[]]
           (m/query (let [[a . xs] [42]] xs)))))

  (testing "[a b . xs] when rest is empty"
    (is (= [[]]
           (m/query (let [[a b . xs] [1 2]] xs))))))

(deftest review-vector-head-middle-tail
  (testing "[a . mid c] — head/middle/last decomposition"
    (is (= [[1 [2 3 4] 5]]
           (m/query (let [[a . mid c] [1 2 3 4 5]] [a mid c])))))

  (testing "[a . mid x y] — head + middle + two tail positionals"
    (is (= [[1 [2 3 4] 5 6]]
           (m/query (let [[a . mid x y] [1 2 3 4 5 6]] [a mid x y])))))

  (testing "[a b . mid c] — two head + middle + one tail"
    (is (= [[1 2 [3 4] 5]]
           (m/query (let [[a b . mid c] [1 2 3 4 5]] [a b mid c])))))

  (testing "[a . mid c] with empty middle"
    (is (= [[1 [] 2]]
           (m/query (let [[a . mid c] [1 2]] [a mid c])))))

  (testing "[a . mid c] with single-element middle"
    (is (= [[1 [2] 3]]
           (m/query (let [[a . mid c] [1 2 3]] [a mid c])))))

  (testing "too few elements for head+tail count errors"
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo)
                          #"pattern requires at least"
          (m/query (let [[a b . mid c d] [1 2 3]] [a b mid c d])))))

  (testing "nested pattern in tail position: [a . mid {:x x}]"
    (is (= [[1 [2 3] 4]]
           (m/query (let [[a . mid {:x x}] [1 2 3 {:x 4}]] [a mid x])))))

  (testing "[a . mid [b c]] — nested vector in tail position"
    (is (= [[1 [2 3] 4 5]]
           (m/query (let [[a . mid [b c]] [1 2 3 [4 5]]] [a mid b c])))))

  (testing "bare dot in middle: [a . . c] not supported — dot consumes pattern"
    ))

(deftest vector-dot-rest-nested-destructure
  (testing "[a . [b c]] — rest is destructured as vector"
    (is (= [[1 2 3]]
           (m/query (let [[a . [b c]] [1 2 3]] [a b c])))))

  (testing "[a . (ks x)] — rest destructured with ks (should fail, rest is vector not map)"
    (is (thrown? #?(:clj Exception :cljs js/Error)
          (m/query (let [[a . (ks x)] [1 {:x 2}]] [a x]))))))

(deftest map-dot-bare
  (testing "{. rest} — map with only dot rest, no named keys"
    (is (= [{:x 1 :y 2}]
           (m/query (let [{. rest} {:x 1 :y 2}] rest)))))

  (testing "{. rest} on empty map"
    (is (= [{}]
           (m/query (let [{. rest} {}] rest))))))

(deftest map-dot-rest-with-constraint
  (testing "constraint on map-extracted key with dot rest"
    (is (= [2 3]
           (m/query (let [{:x a . rest} {:x (one-of 1 2 3) :y 10}]
                      (and (> a 1) a))))))

  (testing "map dot rest with nested destructure on rest"
    (is (= [[1 2 3]]
           (m/query (let [{:x a . (ks y z)} {:x 1 :y 2 :z 3}]
                      [a y z]))))))

;; ── 5. Operator composition: as + dot rest ─────────────────────

(deftest as-with-dot-rest
  (testing "(as v [a b . xs]) — whole value + positional + rest"
    (is (= [[[1 2 3 4 5] 1 2 [3 4 5]]]
           (m/query (let [(as v [a b . xs]) [1 2 3 4 5]]
                      [v a b xs])))))

  (testing "(as v [a b . xs]) with constraint on positional"
    (is (= [[[2 10 20 30] 2 10 [20 30]] [[3 10 20 30] 3 10 [20 30]]]
           (m/query (let [(as v [a b . xs]) [(one-of 1 2 3) 10 20 30]]
                      (and (> a 1) [v a b xs])))))))

(deftest as-with-ks
  (testing "(as m (ks x y)) — whole value + keyword extraction"
    (is (= [[{:x 1 :y 2} 1 2]]
           (m/query (let [(as m (ks x y)) {:x 1 :y 2}]
                      [m x y])))))

  (testing "(as m (ks x y)) with constraint on extracted key"
    (is (= [[{:x 2 :y 10} 2] [{:x 3 :y 10} 3]]
           (m/query (let [(as m (ks x)) {:x (one-of 1 2 3) :y 10}]
                      (and (> x 1) [m x])))))))

(deftest as-with-map-dot-rest
  (testing "(as m {:x a . rest}) — as wrapping map with dot rest"
    (is (= [[{:x 1 :y 2 :z 3} 1 {:y 2 :z 3}]]
           (m/query (let [(as m {:x a . rest}) {:x 1 :y 2 :z 3}]
                      [m a rest]))))))

;; ── 6. Or operator in pattern position ─────────────────────────

(deftest or-pattern-different-shapes
  (testing "(or (ks x) [x]) — map fallback to vector"
    (is (= [42]
           (m/query (let [(or (ks x) [x]) {:x 42}] x))))
    (is (= [42]
           (m/query (let [(or (ks x) [x]) [42]] x))))))

(deftest or-pattern-with-as
  (testing "(or (as m (ks x)) [a]) — complex fallback"
    (is (= [[{:x 42} 42]]
           (m/query (let [(or (as m (ks x)) [x]) {:x 42}] [m x]))))))

(deftest or-all-patterns-fail
  (testing "or in pattern position where all patterns fail"
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) #"All or-patterns failed"
          (m/query (let [(or [a b] (ks x)) "not a map or vec"] a))))))

;; ── 7. Constructor + destructuring composition ─────────────────

(deftest domain-as-composition
  (testing "(as p (point x y)) — as wrapping constructor destructor"
    (is (= [[{:x 5 :y 10} 5 10]]
           (m/query (do (defn point [x y] {:x (integer x) :y (integer y)})
                        (let [(as p (point x y)) {:x 5 :y 10}]
                          [p x y]))))))

  (testing "constructor constraint narrows through as"
    (is (= [[{:x 1 :y 2} 1 2] [{:x 2 :y 2} 2 2] [{:x 3 :y 2} 3 2]]
           (m/query (do (defn small-point [x y]
                          (and (>= x 1) (<= x 3)
                               {:x (integer x) :y (integer y)}))
                        (let [(as p (small-point x y)) {:x (one-of 0 1 2 3 4) :y 2}]
                          [p x y])))))))

(deftest domain-with-map-pattern
  (testing "defn constructor destructures to map fields"
    (is (= [[1 2]]
           (m/query (do (defn point [x y] {:x (integer x) :y (integer y)})
                        (let [(point x y) {:x 1 :y 2}]
                          [x y])))))))

;; ── 8. Multiple dot rests in nested positions ──────────────────

(deftest nested-dot-rests
  (testing "vector dot rest inside map pattern value"
    (is (= [[1 [2 3]]]
           (m/query (let [{:x [a . b]} {:x [1 2 3]}] [a b])))))

  (testing "map dot rest + vector dot rest on extracted value"
    (is (= [[1 [2 3 4] {:z 5}]]
           (m/query (let [{:x [a . rest-v] . rest-m} {:x [1 2 3 4] :z 5}]
                      [a rest-v rest-m])))))

  (testing "vector with nested map in rest"
    (is (= [[1 2 {:x 3}]]
           (m/query (let [[a b . xs] [1 2 {:x 3}]] [a b (nth xs 0)]))))))

;; ── 9. Error messages for removed features ─────────────────────

(deftest keys-error-message
  (testing "{:keys [...]} gives helpful error"
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo)
                          #"not supported"
          (m/query (let [{:keys [x]} {:x 1}] x)))))

  (testing "{:keys [...]} with other entries still errors"
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo)
                          #"not supported"
          (m/query (let [{:keys [x] :y y} {:x 1 :y 2}] x))))))

;; ── 10. Error cases: invalid patterns ──────────────────────────

(deftest non-keyword-map-keys-error
  (testing "map pattern with string key errors"
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo)
                          #"must be keywords"
          (m/query (let [{"name" n} {"name" "Alice"}] n)))))

  (testing "map pattern with integer key errors"
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo)
                          #"must be keywords"
          (m/query (let [{0 a} {0 "first"}] a))))))

(deftest as-non-symbol-name-error
  (testing "(as 42 pattern) — numeric name should error"
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo)
                          #"must be a symbol"
          (m/query (let [(as 42 (ks x)) {:x 1}] x)))))

  (testing "(as :m pattern) — keyword name should error"
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo)
                          #"must be a symbol"
          (m/query (let [(as :m (ks x)) {:x 1}] x))))))

(deftest unresolved-pattern-head-error
  (testing "unresolved symbol in list pattern"
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo)
                          #"Unresolved pattern head"
          (m/query (let [(nonexistent x) {:x 1}] x))))))

(deftest invalid-destructuring-pattern-error
  (testing "literal number as pattern"
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo)
                          #"Invalid destructuring pattern"
          (m/query (let [42 100] 42))))))

;; ── 11. ks operator edge cases ─────────────────────────────────

(deftest ks-single-key
  (testing "(ks x) with single key"
    (is (= [42] (m/query (let [(ks x) {:x 42 :y 99}] x))))))

(deftest ks-many-keys
  (testing "(ks a b c d) with four keys"
    (is (= [[1 2 3 4]]
           (m/query (let [(ks a b c d) {:a 1 :b 2 :c 3 :d 4}]
                      [a b c d]))))))

(deftest ks-zero-args
  (testing "(ks) with zero args — binds nothing, body works"
    (is (= [42] (m/query (let [(ks) {:x 1}] 42))))))

(deftest ks-with-domain-values
  (testing "ks with domain values and constraint"
    (is (= [[2 10] [3 10]]
           (m/query (let [(ks x y) {:x (one-of 1 2 3) :y 10}]
                      (and (> x 1) [x y])))))))

;; ── 12. Constraint propagation through patterns ────────────────

(deftest constraint-propagation-map
  (testing "constraint on extracted key propagates to whole map via as"
    (is (= [{:x 2 :y 10} {:x 3 :y 10}]
           (m/query (let [(as m {:x x}) {:x (one-of 1 2 3) :y 10}]
                      (and (> x 1) m)))))))

(deftest constraint-propagation-vector
  (testing "constraint on vector element propagates to whole vector via as"
    (is (= #{[2 10] [3 10]}
           (set (m/query (let [(as v [a b]) [(one-of 1 2 3) 10]]
                           (and (> a 1) v))))))))

(deftest constraint-propagation-cross-keys
  (testing "relational constraint between two destructured map keys"
    (is (= [[1 2] [1 3] [2 3]]
           (m/query (let [{:x x :y y} {:x (one-of 1 2 3) :y (one-of 1 2 3)}]
                      (and (< x y) [x y])))))))

(deftest constraint-propagation-cross-elements
  (testing "relational constraint between two destructured vector elements"
    (is (= [[1 2] [1 3] [2 3]]
           (m/query (let [[x y] [(one-of 1 2 3) (one-of 1 2 3)]]
                      (and (< x y) [x y])))))))

(deftest constraint-propagation-deep-as
  (testing "constraint on deeply nested value reflects in top-level as"
    (is (= [{:data {:items [2 10]} :label "test"}
            {:data {:items [3 10]} :label "test"}]
           (m/query (let [(as top {:data {:items [a b]}})
                          {:data {:items [(one-of 1 2 3) 10]} :label "test"}]
                      (and (> a 1) top)))))))

;; ── 13. Integration: destructuring + functions ─────────────────

(deftest destructuring-function-return
  (testing "destructuring a function's returned map"
    (is (= [[1 2]]
           (m/query (let [mk (fn [a b] {:x a :y b})
                          {:x x :y y} (mk 1 2)]
                      [x y])))))

  (testing "destructuring a function's returned vector with dot rest"
    (is (= [[1 [2 3]]]
           (m/query (let [mk (fn [a b c] [a b c])
                          [h . t] (mk 1 2 3)]
                      [h t]))))))

;; ── 14. Integration: destructuring + branching ─────────────────

(deftest destructuring-with-if
  (testing "destructured var in if condition"
    (is (= [1 10]
           (sort (m/query (let [{:x x} {:x (one-of 1 2 3)}]
                            (if (< x 2) x 10)))))))

  (testing "destructured var from vector in cond"
    (is (= #{:small :big}
           (set (m/query (let [[x] [(one-of 1 5)]]
                           (cond (< x 3) :small
                                 :else :big))))))))

;; ── 15. Multiple destructurings in same let ────────────────────

(deftest multiple-destructurings-review
  (testing "two map destructurings in same let"
    (is (= [30]
           (m/query (let [{:x x} {:x 10}
                          {:y y} {:y 20}]
                      (+ x y))))))

  (testing "vector + map destructuring in same let"
    (is (= [15]
           (m/query (let [[a b] [5 10]
                          {:c c} {:c 0}]
                      (+ a b c))))))

  (testing "symbol binding then destructuring that uses it"
    (is (= [42]
           (m/query (let [v [42 99]
                          [x y] v]
                      x))))))

;; ── 16. Dot rest with domain constraints ───────────────────────

(deftest dot-rest-domain-constraints
  (testing "constraint on positional element with dot rest"
    (is (= [2 3]
           (m/query (let [[a . xs] [(one-of 1 2 3) 10 20]]
                      (and (> a 1) a))))))

  (testing "dot rest element count matches"
    (is (= [3]
           (m/query (let [[a b . xs] [1 2 3 4 5]]
                      (count xs))))))

  (testing "dot rest elements accessible via further destructure"
    (is (= [3]
           (m/query (let [[a b . xs] [1 2 3 4 5]
                          [c d e] xs]
                      c))))))

;; ── 17. Map dot rest edge cases ────────────────────────────────

(deftest map-dot-empty-rest
  (testing "map dot rest when all keys are named — empty map rest"
    (is (= [{}]
           (m/query (let [{:x a :y b . rest} {:x 1 :y 2}] rest))))))

(deftest map-dot-many-remaining
  (testing "map dot rest with many remaining keys"
    (is (= [{:b 2 :c 3 :d 4}]
           (m/query (let [{:a a . rest} {:a 1 :b 2 :c 3 :d 4}] rest))))))

;; ── 18. Nested operator combinations ──────────────────────────

(deftest ks-inside-vector
  (testing "[(ks x) b] — ks inside vector pattern"
    (is (= [1]
           (m/query (let [[(ks x) b] [{:x 1} 20]] x))))))

(deftest as-inside-vector
  (testing "[(as m (ks x)) b] — as inside vector"
    (is (= [[{:x 1 :y 2} 1 20]]
           (m/query (let [[(as m (ks x)) b] [{:x 1 :y 2} 20]]
                      [m x b]))))))

(deftest domain-inside-vector
  (testing "[point b] where point is constructor-destructured"
    (is (= [[1 2 99]]
           (m/query (do (defn point [x y] {:x (integer x) :y (integer y)})
                        (let [[(point x y) b] [{:x 1 :y 2} 99]]
                          [x y b])))))))

;; ── 19. Metadata propagation ──────────────────────────────────

(deftest or-destructuring-vs-expression
  (testing "or in destructuring position uses pattern semantics"
    (is (= [42]
           (m/query (let [(or (ks x) [x]) {:x 42}] x)))))

  (testing "or in expression position uses domain union semantics"
    (is (= #{3 7}
           (set (m/query (let [x (between 1 10)]
                           (and (or (= x 3) (= x 7)) x))))))))

;; ── 20. drop and dissoc edge cases ─────────────────────────────

(deftest drop-beyond-length
  (testing "drop beyond vector length returns empty vector"
    (is (= [[]]
           (m/query (let [v [1 2]] (drop 5 v))))))

  (testing "drop 0 returns all elements"
    (is (= [[1 2 3]]
           (m/query (let [v [1 2 3]] (drop 0 v)))))))

(deftest dissoc-nonexistent-key
  (testing "dissoc key not in map returns same map"
    (is (= [{:x 1 :y 2}]
           (m/query (let [m {:x 1 :y 2}] (dissoc m :z))))))

  (testing "dissoc all keys returns empty map"
    (is (= [{}]
           (m/query (let [m {:x 1 :y 2}] (dissoc m :x :y)))))))

(deftest dissoc-with-no-keys
  (testing "dissoc with no keys returns original map"
    (is (= [{:x 1 :y 2}]
           (m/query (let [m {:x 1 :y 2}] (dissoc m)))))))

;; ── 21. Stress: deeply nested + constraints ────────────────────

(deftest complex-nested-destructure
  (testing "deeply nested with constraint propagation through all levels"
    (is (= [[2 [3 4] {:z 5}] [3 [3 4] {:z 5}]]
           (m/query (let [{:data [a . rest] . map-rest}
                          {:data [(one-of 1 2 3) 3 4] :z 5}]
                      (and (> a 1) [a rest map-rest]))))))

  (testing "as wrapping map wrapping vector with constraints"
    (is (= [[{:items [3 10]} 3] [{:items [4 10]} 4]]
           (m/query (let [(as m {:items [a b]})
                          {:items [(one-of 1 2 3 4 5) 10]}]
                      (and (> a 2) (< a 5) [m a])))))))

;; ── 22. Nested map patterns in value position ──────────────────

(deftest map-value-is-nested-pattern
  (testing "map value pattern is another map"
    (is (= [42]
           (m/query (let [{:x {:y z}} {:x {:y 42}}] z)))))

  (testing "map value pattern is a vector"
    (is (= [[1 2]]
           (m/query (let [{:x [a b]} {:x [1 2]}] [a b])))))

  (testing "map value pattern is ks"
    (is (= [10]
           (m/query (let [{:x (ks y)} {:x {:y 10}}] y))))))

;; ── 23. Symbol pattern edge case ──────────────────────────────

(deftest symbol-as-pattern-in-let
  (testing "simple symbol binding (baseline)"
    (is (= [42] (m/query (let [x 42] x)))))

  (testing "symbol binding then destructuring"
    (is (= [1]
           (m/query (let [m {:x 1}
                          {:x x} m]
                      x))))))

;; ── 24. Defdomain destructuring edge cases ─────────────────────

(deftest domain-destructure-narrows-values
  (testing "constructor constraint narrows out-of-range values"
    (is (= [[1 10] [2 10] [3 10]]
           (m/query (do (defn small-x [x y]
                          (and (>= x 1) (<= x 3)
                               {:x (integer x) :y (integer y)}))
                        (let [(small-x x y) {:x (one-of 0 1 2 3 4) :y 10}]
                          [x y]))))))

  (testing "constructor + as — narrows and captures whole"
    (is (= [[{:x 2 :y 10} 2 10] [{:x 3 :y 10} 3 10]]
           (m/query (do (defn small-x [x y]
                          (and (>= x 1) (<= x 3)
                               {:x (integer x) :y (integer y)}))
                        (let [(as m (small-x x y)) {:x (one-of 0 1 2 3 4) :y 10}]
                          (and (> x 1) [m x y]))))))))
