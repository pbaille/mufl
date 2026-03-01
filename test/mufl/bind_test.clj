(ns mufl.bind-test
  (:require [clojure.test :refer [deftest testing is]]
            [mufl.core :as m]
            [mufl.tree :as tree]
            [mufl.domain :as dom]
            [mufl.bind :as bind]
            [mufl.env :as env]))

(defn bind-in-scope
  "Helper: create base env, make a scope, bind expr inside it."
  [f]
  (let [base (env/base-env)
        ws (-> (tree/ensure-path base ['ws])
               (tree/upd ['ws] f))]
    ws))

(defn domain-at [env path]
  (:domain (tree/cd env path)))

;; ════════════════════════════════════════════════════════════════
;; Propagation tests
;; ════════════════════════════════════════════════════════════════

(deftest lt-propagation
  (testing "< narrows both sides"
    (let [ws (bind-in-scope
              (fn [e]
                (-> e
                    (bind/bind 'x '(one-of 1 2 3 4 5))
                    (bind/bind 'y '(one-of 1 2 3 4 5))
                    (bind/bind '(< x y)))))]
      ;; x can't be 5 (nothing bigger), y can't be 1 (nothing smaller)
      (is (= #{1 2 3 4} (dom/members (domain-at ws ['ws 'x]))))
      (is (= #{2 3 4 5} (dom/members (domain-at ws ['ws 'y])))))))

(deftest eq-propagation
  (testing "= narrows to intersection"
    (let [ws (bind-in-scope
              (fn [e]
                (-> e
                    (bind/bind 'x '(one-of 1 2 3))
                    (bind/bind 'y '(one-of 2 3 4))
                    (bind/bind '(= x y)))))]
      (is (= #{2 3} (dom/members (domain-at ws ['ws 'x]))))
      (is (= #{2 3} (dom/members (domain-at ws ['ws 'y])))))))

(deftest neq-propagation
  (testing "!= narrows when one side is singleton"
    (let [ws (bind-in-scope
              (fn [e]
                (-> e
                    (bind/bind 'x '(one-of 1 2 3))
                    (bind/bind 'y '(one-of 2))
                    (bind/bind '(!= x y)))))]
      (is (= #{1 3} (dom/members (domain-at ws ['ws 'x])))))))

(deftest cascading-propagation
  (testing "narrowing cascades through constraint watchers"
    (let [ws (bind-in-scope
              (fn [e]
                (-> e
                    (bind/bind 'x '(one-of 1 2 3))
                    (bind/bind 'y '(one-of 1 2 3))
                    ;; After (= x y): both {1,2,3}
                    (bind/bind '(= x y))
                    ;; After (> x 2): x narrows to {3}
                    ;; which cascades through = to narrow y to {3}
                    (bind/bind '(> x 2)))))]
      (is (= #{3} (dom/members (domain-at ws ['ws 'x]))))
      (is (= #{3} (dom/members (domain-at ws ['ws 'y])))))))

(deftest contradiction-detection
  (testing "contradictory constraints detected at bind time"
    (is (thrown? clojure.lang.ExceptionInfo
                 (bind-in-scope
                  (fn [e]
                    (-> e
                        (bind/bind 'x '(one-of 1 2))
                        (bind/bind 'y '(one-of 3 4))
                        (bind/bind '(= x y)))))))))

(deftest arithmetic-propagation
  (testing "+ constraint creates derived domain"
    (let [ws (bind-in-scope
              (fn [e]
                (-> e
                    (bind/bind 'x '(one-of 1 2))
                    (bind/bind 'y '(one-of 10 20))
                    (bind/bind 's '(+ x y)))))]
      (is (= #{11 12 21 22} (dom/members (domain-at ws ['ws 's])))))))

;; ════════════════════════════════════════════════════════════════
;; Bind edge cases (from edge_cases_test section 2)
;; ════════════════════════════════════════════════════════════════

(deftest symbol-shadowing
  (testing "inner let shadows outer binding"
    (is (= [2] (m/query (let [x 1]
                          (let [x 2] x)))))))

(deftest nested-let-scopes
  (testing "nested let can reference outer bindings"
    (is (= [1] (m/query (let [x 1]
                          (let [y x] y)))))))

(deftest literal-constraint-true
  (testing "(< 1 2) — constraint between literals that holds"
    (is (some? (m/query (let [x 1] (and (< 1 2) x)))))))

(deftest literal-constraint-false
  (testing "(< 3 1) — constraint between literals that fails"
    (is (thrown? clojure.lang.ExceptionInfo
                 (m/query (let [x 1] (and (< 3 1) x)))))))

(deftest literal-equality-contradiction
  (testing "(= 3 4) — contradicts immediately"
    (is (thrown? clojure.lang.ExceptionInfo
                 (m/query (and (= 3 4) 1))))))

(deftest literal-equality-success
  (testing "(= 3 3) — trivially holds"
    (is (some? (m/query (let [x 1] (and (= 3 3) x)))))))

(deftest nested-arithmetic-constraint
  (testing "(< (+ x 1) (* y 2)) — nested arithmetic in constraints"
    (is (= #{[1 2] [1 3] [2 2] [2 3] [3 3]}
           (set (m/query (let [x (one-of 1 2 3)
                               y (one-of 1 2 3)]
                           (and (< (+ x 1) (* y 2))
                                [x y]))))))))

(deftest empty-and
  (testing "(and) — no arguments"
    (is (some? (m/query (let [x 1] (and)))))))

(deftest empty-let-bindings
  (testing "(let [] expr) — empty bindings"
    (is (= [42] (m/query (let [] 42))))))

(deftest let-with-many-bindings
  (testing "let with many sequential bindings referencing each other"
    ;; a=1, b=a+1=2, c=a+b=1+2=3
    (is (= [3] (m/query (let [a 1 b (+ a 1) c (+ a b)] c))))))

;; ════════════════════════════════════════════════════════════════
;; Stale domain on link nodes (from edge_cases_test section 13)
;; ════════════════════════════════════════════════════════════════

(deftest link-domain-follows-target
  (testing "binding y to x, then narrowing x — y's resolved domain reflects narrowing"
    (let [results (m/query (let [x (one-of 1 2 3 4 5)
                                 y x]
                             (and (> x 3)
                                  [x y])))]
      (is (every? (fn [[x y]] (and (= x y) (> x 3))) results))
      (is (= #{[4 4] [5 5]} (set results))))))

(deftest link-node-has-no-domain
  (testing "link node carries no :domain — domain is resolved by following the link"
    (let [ws (-> (env/base-env)
                 (tree/ensure-path ['ws])
                 (tree/upd ['ws] (fn [e]
                                   (-> e
                                       (bind/bind 'x '(one-of 1 2 3 4 5))
                                       (bind/bind 'y 'x)
                                       (bind/bind '(> x 3))))))]
      (let [y-node (tree/cd ws ['ws 'y])
            y-own-domain (:domain y-node)
            y-resolved-domain (:domain (bind/resolve y-node))]
        ;; Link nodes should NOT carry their own :domain
        (is (nil? y-own-domain) "Link node should have no :domain of its own")
        ;; Resolved domain correctly follows the link
        (is (= #{4 5} (dom/members y-resolved-domain)))))))

;; ════════════════════════════════════════════════════════════════
;; Constraint on expressions (from edge_cases_test section 14)
;; ════════════════════════════════════════════════════════════════

(deftest constraint-on-sum-expression
  (testing "constraint directly on arithmetic expression"
    (is (= #{[1 4] [2 3]}
           (set (m/query (let [x (one-of 1 2 3 4)
                               y (one-of 1 2 3 4)]
                           (and (= (+ x y) 5)
                                (< x y)
                                [x y]))))))))
