(ns mufl.bind-destructuring-test
  "Tests for destructuring operators implemented via :destruct protocol.
   ks, as, or (pattern), and domain destructuring."
  (:require [clojure.test :refer [deftest testing is]]
            [mufl.core :as m]))

;; ════════════════════════════════════════════════════════════════
;; ks — keyword-symbol map destructuring
;; ════════════════════════════════════════════════════════════════

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

;; ════════════════════════════════════════════════════════════════
;; as — bind whole + inner destructure
;; ════════════════════════════════════════════════════════════════

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

;; ════════════════════════════════════════════════════════════════
;; or — pattern-position fallback
;; ════════════════════════════════════════════════════════════════

(deftest or-pattern
  (testing "or in pattern position tries first pattern, falls back to second"
    ;; {:x 42} matches ks (has :x key)
    (is (= [42]
           (m/query (let [(or (ks x) [x]) {:x 42}] x))))))

;; ════════════════════════════════════════════════════════════════
;; Constructor destructuring through defn :construct
;; ════════════════════════════════════════════════════════════════

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

  ;; Domain in expression position still works via narrow
  (testing "domain as expression constraint still works"
    (is (= [1 2 3]
           (m/query (def small (between 1 3))
                    (let [x (one-of 0 1 2 3 4)]
                      (and (narrow x small) x)))))))

;; ════════════════════════════════════════════════════════════════
;; Composition — nested destructuring operators
;; ════════════════════════════════════════════════════════════════

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
