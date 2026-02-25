(ns mufl.edge-cases-test
  (:require [clojure.test :refer [deftest testing is]]
            [mufl.core :as m]
            [mufl.domain :as dom]
            [mufl.tree :as tree]
            [mufl.bind :as bind]
            [mufl.env :as env]))

;; ════════════════════════════════════════════════════════════════
;; 1. DOMAIN EDGE CASES
;; ════════════════════════════════════════════════════════════════

(deftest empty-one-of
  (testing "(one-of) — empty domain returns no solutions"
    (is (= [] (m/query (let [x (one-of)] x))))))

(deftest inverted-between
  (testing "(between 5 3) — inverted between returns no solutions"
    (is (= [] (m/query (let [x (between 5 3)] x))))))

(deftest single-nil-domain
  (testing "nil as a domain value"
    (is (= [nil] (m/query nil)))
    (is (= [nil] (m/query (let [x nil] x))))))

(deftest single-element-between
  (testing "(between 3 3) — single element between"
    (is (= [3] (m/query (let [x (between 3 3)] x))))))

(deftest large-domain-performance
  (testing "(between 1 100) with constraints doesn't blow up"
    (let [results (m/query (let [x (between 1 100)]
                             (and (> x 95) x)))]
      (is (= [96 97 98 99 100] results)))))

(deftest domain-with-negative-between
  (testing "(between -3 3) includes negatives and zero"
    (is (= [-3 -2 -1 0 1 2 3]
           (m/query (let [x (between -3 3)] x))))))

;; ════════════════════════════════════════════════════════════════
;; 2. BIND EDGE CASES
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
;; 3. PROPAGATION EDGE CASES
;; ════════════════════════════════════════════════════════════════

(deftest circular-eq-propagation
  (testing "(= x y) + (= y x) — circular doesn't loop forever"
    (is (= #{[1 1] [2 2] [3 3]}
           (set (m/query (let [x (one-of 1 2 3)
                               y (one-of 1 2 3)]
                           (and (= x y) (= y x)
                                [x y]))))))))

(deftest long-propagation-chain
  (testing "narrowing cascades through chain: a=b, b=c, c=d, d>2"
    (let [results (m/query (let [a (one-of 1 2 3 4 5)
                                 b (one-of 1 2 3 4 5)
                                 c (one-of 1 2 3 4 5)
                                 d (one-of 1 2 3 4 5)]
                             (and (= a b) (= b c) (= c d) (> d 2)
                                  [a b c d])))]
      (is (every? (fn [[a b c d]] (and (= a b c d) (> d 2))) results))
      (is (= #{[3 3 3 3] [4 4 4 4] [5 5 5 5]} (set results))))))

(deftest cascading-neq-narrowing
  (testing "neq cascades when singleton is produced"
    (let [results (m/query (let [x (one-of 1)
                                 y (one-of 1 2 3)
                                 z (one-of 1 2 3)]
                             (and (!= x y) (!= y z) (!= x z)
                                  [x y z])))]
      (is (= #{[1 2 3] [1 3 2]} (set results))))))

(deftest eq-transitivity
  (testing "equality propagates transitively: a=b, b=c implies a=c domains"
    (let [results (m/query (let [a (one-of 1 2 3 4)
                                 b (one-of 2 3 4 5)
                                 c (one-of 3 4 5 6)]
                             (and (= a b) (= b c)
                                  [a b c])))]
      ;; Intersection: a∩b = {2,3,4}, b∩c = {3,4,5}
      ;; Then a=b=c must all share domain → {3,4}
      (is (= #{[3 3 3] [4 4 4]} (set results))))))

(deftest neq-singleton-cascade
  (testing "neq cascade: x=1 forces y!=1, if y was {1,2} → y=2, then z!=2 forces z∈{1,3}"
    (let [results (m/query (let [x (one-of 1)
                                 y (one-of 1 2)
                                 z (one-of 1 2 3)]
                             (and (!= x y) (!= y z)
                                  [x y z])))]
      ;; x=1, y must be 2 (only option after !=x), z must be != 2 → z∈{1,3}
      (is (= #{[1 2 1] [1 2 3]} (set results))))))

;; ════════════════════════════════════════════════════════════════
;; 4. SOLVE EDGE CASES
;; ════════════════════════════════════════════════════════════════

(deftest no-solutions
  (testing "all branches contradict during solve"
    (is (thrown? clojure.lang.ExceptionInfo
                 (m/query (let [x (one-of 1 2 3)]
                            (and (> x 5) x)))))))

(deftest single-solution-already-ground
  (testing "all variables ground after propagation — one solution"
    (is (= [5] (m/query (let [x (one-of 5)] x))))))

(deftest solve-no-variables
  (testing "solve on scope with no variables — literal return"
    (is (= [42] (m/query 42)))
    (is (= [true] (m/query true)))
    (is (= ["hello"] (m/query "hello")))))

(deftest ground-literal-expressions
  (testing "literal arithmetic returns single result"
    (is (= [6] (m/query (+ 2 4))))
    (is (= [2] (m/query (- 5 3))))))

(deftest solve-cartesian-product
  (testing "unconstrained variables enumerate full cartesian product"
    (let [results (m/query (let [x (one-of 1 2)
                                 y (one-of :a :b)]
                             [x y]))]
      (is (= 4 (count results)))
      (is (= #{[1 :a] [1 :b] [2 :a] [2 :b]} (set results))))))

;; ════════════════════════════════════════════════════════════════
;; 5. IF / COND / OR EDGE CASES
;; ════════════════════════════════════════════════════════════════

(deftest if-different-return-shapes
  (testing "if with different return shapes per branch"
    (let [results (m/query (let [x (one-of 1 2 3 4 5)]
                             (if (< x 3) [x] x)))]
      (is (some #(vector? %) results))
      (is (some #(number? %) results)))))

(deftest nested-if
  (testing "nested if: (if (< x 3) (if (> x 1) x 0) 10)"
    (let [results (m/query (let [x (between 1 5)]
                             (if (< x 3) (if (> x 1) x 0) 10)))]
      (is (= #{0 2 10} (set results))))))

(deftest if-boolean-literal-true
  (testing "(if true x y) — only then branch"
    (is (= #{1 2 3}
           (set (m/query (let [x (one-of 1 2 3)]
                           (if true x 99))))))))

(deftest if-boolean-literal-false
  (testing "(if false x y) — only else branch"
    (is (= [99]
           (m/query (let [x (one-of 1 2 3)]
                      (if false x 99)))))))

(deftest or-single-branch
  (testing "(or (= x 1)) — single branch"
    (is (= [1] (m/query (let [x (one-of 1 2 3)]
                          (and (or (= x 1)) x)))))))

(deftest when-contradicting-body
  (testing "when with body that contradicts — should not crash outer"
    (let [results (m/query (let [x (between 1 5)]
                             (when (< x 3) (> x 5))))]
      (is (some? results)))))

(deftest if-with-nil-else
  (testing "if with nil else branch"
    (let [results (m/query (let [x (one-of 1 2 3)]
                             (if (= x 1) :found nil)))]
      (is (some #{:found} results))
      (is (some nil? results)))))

(deftest cond-boolean-literal-test
  (testing "cond with true as test"
    (is (= #{1 2 3}
           (set (m/query (let [x (one-of 1 2 3)]
                           (cond true x))))))))

(deftest cond-false-literal-test
  (testing "cond with false as test — branch is skipped"
    (is (= [99]
           (m/query (let [x (one-of 1 2 3)]
                      (cond false x
                            :else 99)))))))

;; ════════════════════════════════════════════════════════════════
;; 6. FN EDGE CASES
;; ════════════════════════════════════════════════════════════════

(deftest fn-multiple-calls-different-args
  (testing "same function called with different args"
    (is (= #{4 6}
           (set (m/query (let [double (fn [x] (+ x x))
                               a (one-of 2 3)]
                           (double a))))))))

(deftest fn-returning-vector
  (testing "function returning a vector"
    (is (= [[1 2]] (m/query (let [pair (fn [a b] [a b])]
                              (pair 1 2)))))))

(deftest fn-wrong-arity
  (testing "function called with too few args"
    (is (thrown? clojure.lang.ExceptionInfo
                 (m/query (let [f (fn [x y] (+ x y))]
                            (f 1))))))
  (testing "function called with too many args"
    (is (thrown? clojure.lang.ExceptionInfo
                 (m/query (let [f (fn [x] (+ x 1))]
                            (f 1 2 3)))))))

(deftest fn-higher-order
  (testing "higher-order function application works"
    ;; (fn [f] (f 5)) — f resolves to double via link chain,
    ;; and :mufl-fn is found on the resolved target
    (is (= [10]
           (m/query (let [apply-to-5 (fn [f] (f 5))
                          double (fn [x] (+ x x))]
                      (apply-to-5 double)))))))

(deftest fn-with-closure
  (testing "fn can reference variables from enclosing scope"
    (is (= [11] (m/query (let [base 10
                               add-base (fn [x] (+ x base))]
                           (add-base 1)))))))

(deftest fn-two-independent-calls
  (testing "two calls to same fn with different args, both results used"
    (is (= [[4 6]] (m/query (let [double (fn [x] (+ x x))
                                  a (double 2)
                                  b (double 3)]
                              [a b]))))))

;; ════════════════════════════════════════════════════════════════
;; 7. MIN/MAX EDGE CASES
;; ════════════════════════════════════════════════════════════════

(deftest min-max-basic
  (testing "max of two domains"
    (is (= #{3 4 5}
           (set (m/query (let [x (one-of 3 5)
                               y (one-of 2 4)]
                           (max x y)))))))
  (testing "min/max with singletons"
    (is (= [2] (m/query (min 3 2))))
    (is (= [5] (m/query (max 3 5))))))

(deftest min-max-same-values
  (testing "min of equal singletons"
    (is (= [5] (m/query (min 5 5)))))
  (testing "max of equal singletons"
    (is (= [5] (m/query (max 5 5))))))

(deftest min-max-no-inverse-propagation
  (testing "min/max don't propagate back (known limitation)"
    ;; (min x y) = 1 doesn't narrow x or y
    ;; This is documented — min/max are compute-only, not constraint-backed
    (let [results (m/query (let [x (one-of 1 2 3)
                                 y (one-of 1 2 3)
                                 m (min x y)]
                             (and (= m 1) [x y])))]
      ;; All pairs where min(x,y) = 1 should appear
      ;; But since min doesn't create a constraint, m is just a computed domain
      ;; and (= m 1) constrains m but doesn't propagate to x,y
      (is (pos? (count results))))))

;; ════════════════════════════════════════════════════════════════
;; 8. NOT EDGE CASES
;; ════════════════════════════════════════════════════════════════

(deftest not-with-non-relational
  (testing "(not true) — non-relational inner form should error gracefully"
    (is (thrown? Exception
                 (m/query (not true))))))

(deftest not-double-negation
  (testing "(not (not ...)) — double negation is not supported (known limitation)"
    ;; not only handles relational ops, not itself. Would need special handling.
    (is (thrown? clojure.lang.ExceptionInfo
                 (m/query (let [x (one-of 1 2 3)]
                            (and (not (not (= x 1))) x)))))))

(deftest not-negating-neq
  (testing "(not (!= x 1)) is equivalent to (= x 1)"
    (is (= [1] (m/query (let [x (one-of 1 2 3)]
                          (and (not (!= x 1)) x)))))))

;; ════════════════════════════════════════════════════════════════
;; 9. COMBINED / STRESS TESTS
;; ════════════════════════════════════════════════════════════════

(deftest send-more-money-style
  (testing "small crypto-arithmetic: A + B = C where all different, all in 1..5"
    (let [results (m/query (let [a (between 1 5)
                                 b (between 1 5)
                                 c (between 1 5)]
                             (and (= (+ a b) c)
                                  (distinct [a b c])
                                  (< a b)
                                  [a b c])))]
      (is (= #{[1 2 3] [1 3 4] [1 4 5] [2 3 5]} (set results))))))

(deftest deep-search-terminates
  (testing "5 variables with constraints — search terminates"
    (let [results (m/query (let [a (one-of 1 2)
                                 b (one-of 1 2)
                                 c (one-of 1 2)
                                 d (one-of 1 2)
                                 e (one-of 1 2)]
                             (and (= (+ a b) (+ c d))
                                  (!= a e)
                                  [a b c d e])))]
      (is (pos? (count results))))))

(deftest map-return
  (testing "map literal return"
    (is (= [{:a 1 :b 2}]
           (m/query {:a 1 :b 2})))))

(deftest vector-return
  (testing "vector literal return"
    (is (= [[1 2 3]]
           (m/query [1 2 3])))))

(deftest even-odd-on-already-narrow
  (testing "even on a singleton"
    (is (= [2] (m/query (let [x (one-of 2)] (and (even x) x))))))
  (testing "even contradiction on odd singleton"
    (is (thrown? clojure.lang.ExceptionInfo
                 (m/query (let [x (one-of 1)] (and (even x) x)))))))

(deftest abs-of-zero
  (testing "|0| = 0"
    (is (= [0] (m/query (let [x (one-of -1 0 1)]
                          (and (= (abs x) 0) x)))))))

;; ════════════════════════════════════════════════════════════════
;; 10. OR EDGE CASES
;; ════════════════════════════════════════════════════════════════

(deftest or-empty-branches
  (testing "(or) with no branches should error"
    (is (thrown? clojure.lang.ExceptionInfo
                 (m/query (let [x (one-of 1 2 3)]
                            (and (or) x)))))))

(deftest or-all-contradict
  (testing "or where all branches contradict"
    (is (thrown? clojure.lang.ExceptionInfo
                 (m/query (let [x (one-of 1 2 3)]
                            (and (or (= x 10) (= x 20)) x)))))))

(deftest or-loses-correlation
  (testing "or unions domains independently — loses branch correlation (known limitation)"
    ;; (or (and (= x 1) (= y 2)) (and (= x 3) (= y 4)))
    ;; should ideally give {[1,2], [3,4]} but gives {[1,2], [1,4], [3,2], [3,4]}
    ;; because or only unions x→{1,3}, y→{2,4} independently
    (let [results (m/query (let [x (one-of 1 2 3 4 5)
                                 y (one-of 1 2 3 4 5)]
                             (and (or (and (= x 1) (= y 2))
                                      (and (= x 3) (= y 4)))
                                  [x y])))]
      ;; We get overapproximation due to domain union
      (is (>= (count results) 2))
      ;; But the correct pairs ARE in the results
      (is (some #{[1 2]} results))
      (is (some #{[3 4]} results)))))

;; ════════════════════════════════════════════════════════════════
;; 11. COND EDGE CASES
;; ════════════════════════════════════════════════════════════════

(deftest cond-with-else-only
  (testing "cond with only :else branch"
    (is (= #{1 2 3}
           (set (m/query (let [x (one-of 1 2 3)]
                           (cond :else x))))))))

(deftest cond-empty-no-branches
  (testing "(cond) with no branches returns no fork (empty partition)"
    ;; cond with no args → empty :branches, solve finds no viable branches
    (let [results (m/query (let [x (one-of 1 2 3)]
                             (cond)))]
      (is (= [] results)))))

;; ════════════════════════════════════════════════════════════════
;; 12. DEEPLY NESTED IF
;; ════════════════════════════════════════════════════════════════

(deftest triple-nested-if
  (testing "three levels of nested if"
    (let [results (m/query (let [x (between 1 10)]
                             (if (< x 4)
                               (if (< x 2) :tiny :small)
                               (if (> x 7) :big :medium))))]
      (is (= #{:tiny :small :medium :big} (set results))))))

;; ════════════════════════════════════════════════════════════════
;; 13. STALE DOMAIN ON LINK NODES (architectural issue)
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
;; 14. CONSTRAINT ON EXPRESSIONS (not just symbols)
;; ════════════════════════════════════════════════════════════════

(deftest constraint-on-sum-expression
  (testing "constraint directly on arithmetic expression"
    (is (= #{[1 4] [2 3]}
           (set (m/query (let [x (one-of 1 2 3 4)
                               y (one-of 1 2 3 4)]
                           (and (= (+ x y) 5)
                                (< x y)
                                [x y]))))))))

;; ════════════════════════════════════════════════════════════════
;; 15. MINUS PROPAGATION
;; ════════════════════════════════════════════════════════════════

(deftest minus-constraint
  (testing "subtraction with constraint"
    ;; d = x-y, d > 1: all pairs where x-y > 1
    ;; (3,1)→2✓ (4,1)→3✓ (4,2)→2✓ (5,1)→4✓ (5,2)→3✓ (5,3)→2✓
    (is (= #{[3 1] [4 1] [4 2] [5 1] [5 2] [5 3]}
           (set (m/query (let [x (one-of 3 4 5)
                               y (one-of 1 2 3)
                               d (- x y)]
                           (and (> d 1)
                                [x y]))))))))

;; ════════════════════════════════════════════════════════════════
;; 16. MULTIPLICATION PROPAGATION
;; ════════════════════════════════════════════════════════════════

(deftest times-constraint
  (testing "multiplication with constraint"
    (is (= #{[2 3] [3 2]}
           (set (m/query (let [x (one-of 1 2 3 4)
                               y (one-of 1 2 3 4)]
                           (and (= (* x y) 6)
                                [x y]))))))))

;; ════════════════════════════════════════════════════════════════
;; 17. DOMAIN ALGEBRA EDGE CASES (unit level)
;; ════════════════════════════════════════════════════════════════

(deftest domain-operations-on-void
  (testing "algebra with void"
    (is (= dom/void (dom/intersect dom/void dom/any)))
    (is (= dom/void (dom/intersect dom/void dom/void)))
    (is (= dom/any (dom/unite dom/any dom/void)))
    (is (= dom/void (dom/subtract dom/void (dom/single 1))))))

(deftest domain-operations-identity
  (testing "intersect with self"
    (let [d (dom/finite #{1 2 3})]
      (is (= d (dom/intersect d d)))))
  (testing "unite with self"
    (let [d (dom/finite #{1 2 3})]
      (is (= d (dom/unite d d))))))

(deftest domain-operations-single
  (testing "single contains-val?"
    (is (dom/contains-val? (dom/single 42) 42))
    (is (not (dom/contains-val? (dom/single 42) 43))))
  (testing "single nil"
    (is (dom/singleton? (dom/single nil)))
    (is (= nil (dom/singleton-val (dom/single nil))))))

(deftest domain-mod-div-edge-cases
  (testing "mod with zero in domain is handled"
    (is (= #{0 1} (dom/members (dom/domain-mod (dom/finite #{3 4}) (dom/finite #{0 3}))))))
  (testing "div with zero in domain is handled"
    (is (= #{1} (dom/members (dom/domain-div (dom/finite #{3 4}) (dom/finite #{0 3})))))))

(deftest domain-arithmetic-with-void
  (testing "arithmetic on void domains"
    (is (= dom/void (dom/domain-add dom/void (dom/finite #{1 2}))))
    (is (= dom/void (dom/domain-sub (dom/finite #{1}) dom/void)))))

(deftest domain-void-members
  (testing "void has empty member set"
    (is (= #{} (dom/members dom/void)))
    (is (= 0 (dom/size dom/void)))))

(deftest domain-any-members
  (testing "any has nil members (open domain)"
    (is (nil? (dom/members dom/any)))
    (is (nil? (dom/size dom/any)))
    (is (not (dom/finite? dom/any)))))

(deftest domain-finite-normalization
  (testing "finite with one element normalizes to single"
    (is (dom/singleton? (dom/finite #{42})))
    (is (= 42 (dom/singleton-val (dom/finite #{42})))))
  (testing "finite with zero elements normalizes to void"
    (is (dom/void? (dom/finite #{})))))

(deftest domain-contains-val
  (testing "contains-val? on various domains"
    (is (dom/contains-val? dom/any 42))
    (is (not (dom/contains-val? dom/void 42)))
    (is (dom/contains-val? (dom/finite #{1 2 3}) 2))
    (is (not (dom/contains-val? (dom/finite #{1 2 3}) 4)))))

(deftest domain-ordered-ops-with-non-numeric
  (testing "domain-min/max return nil for non-numeric domains"
    (is (nil? (dom/domain-min (dom/finite #{:a :b :c}))))
    (is (nil? (dom/domain-max (dom/finite #{:a :b :c})))))
  (testing "domain-below/above with mixed types filter correctly"
    (let [d (dom/finite #{1 2 :a 3})]
      ;; domain-below only keeps numbers < v
      (is (= #{1 2} (dom/members (dom/domain-below d 3)))))))

;; ════════════════════════════════════════════════════════════════
;; 18. BOOLEAN AND KEYWORD DOMAINS
;; ════════════════════════════════════════════════════════════════

(deftest keyword-domain
  (testing "keyword as domain value"
    (is (= [:a] (m/query :a)))
    (is (= #{:red :blue :green}
           (set (m/query (let [c (one-of :red :blue :green)] c)))))))

(deftest boolean-domain
  (testing "boolean as domain value"
    (is (= [true] (m/query true)))
    (is (= [false] (m/query false)))))

;; ════════════════════════════════════════════════════════════════
;; 19. EVEN / ODD INTERACTION WITH CONSTRAINTS
;; ════════════════════════════════════════════════════════════════

(deftest even-then-gt-constraint
  (testing "even followed by relational constraint preserves evenness"
    (is (= [8 10] (m/query (let [x (between 1 10)]
                             (and (even x) (> x 6) x)))))))

(deftest even-with-equality-constraint
  (testing "even combined with arithmetic equality"
    ;; x even, x+y=7: x=2→y=5, x=4→y=3, x=6→y=1
    (is (= #{[2 5] [4 3] [6 1]}
           (set (m/query (let [x (between 1 10)
                               y (between 1 10)]
                           (and (even x)
                                (= (+ x y) 7)
                                [x y]))))))))

(deftest odd-with-or
  (testing "odd combined with or"
    (is (= #{1 7 9}
           (set (m/query (let [x (between 1 10)]
                           (and (odd x)
                                (or (= x 1) (> x 6))
                                x))))))))

;; ════════════════════════════════════════════════════════════════
;; 20. ABS EDGE CASES
;; ════════════════════════════════════════════════════════════════

(deftest abs-of-zero
  (testing "|0| = 0"
    (is (= [0] (m/query (let [x (one-of -1 0 1)]
                          (and (= (abs x) 0) x)))))))

(deftest abs-negative-only
  (testing "abs on negative-only domain"
    (is (= #{1 2 3}
           (set (m/query (let [x (one-of -3 -2 -1)]
                           (abs x))))))))

(deftest abs-constraint-propagation
  (testing "constraining abs result propagates back to operand"
    (is (= #{-2 2}
           (set (m/query (let [x (one-of -3 -2 -1 0 1 2 3)]
                           (and (= (abs x) 2) x))))))))

;; ════════════════════════════════════════════════════════════════
;; 21. MOD / QUOT EDGE CASES
;; ════════════════════════════════════════════════════════════════

(deftest mod-zero-divisor
  (testing "mod where divisor domain contains 0 — 0 is filtered out"
    (is (= #{0 1 2}
           (set (m/query (let [x (between 1 5)
                               d (one-of 0 3)]
                           (mod x d))))))))

(deftest quot-zero-divisor
  (testing "quot where divisor domain contains 0 — 0 is filtered out"
    (is (= #{0 1}
           (set (m/query (let [x (between 1 5)
                               d (one-of 0 3)]
                           (quot x d))))))))

(deftest mod-with-constraint-back-propagation
  (testing "constraining mod result narrows operand"
    (is (= #{3 6 9}
           (set (m/query (let [x (between 1 10)]
                           (and (= (mod x 3) 0) x))))))))

;; ════════════════════════════════════════════════════════════════
;; 22. DISTINCT EDGE CASES
;; ════════════════════════════════════════════════════════════════

(deftest distinct-two-vars
  (testing "distinct with two variables"
    (let [results (m/query (let [x (one-of 1 2)
                                 y (one-of 1 2)]
                             (and (distinct [x y]) [x y])))]
      (is (= #{[1 2] [2 1]} (set results))))))

(deftest distinct-singleton-cascade
  (testing "distinct with singleton cascades to narrow others"
    (let [results (m/query (let [a (one-of 1)
                                 b (one-of 1 2)
                                 c (one-of 1 2 3)]
                             (and (distinct [a b c]) [a b c])))]
      ;; a=1, b must be 2, c must be 3
      (is (= [[1 2 3]] results)))))

;; ════════════════════════════════════════════════════════════════
;; 23. TREE NAVIGATION EDGE CASES
;; ════════════════════════════════════════════════════════════════

(deftest tree-cd-empty-path
  (testing "cd with empty path returns same node"
    (let [t {:domain (dom/single 1)}]
      (is (= t (tree/cd t []))))))

(deftest tree-cd-nonexistent
  (testing "cd to nonexistent path returns nil"
    (is (nil? (tree/cd {} ['nonexistent])))))

(deftest tree-root-of-root
  (testing "root of root is itself"
    (let [t {:domain (dom/single 1)}]
      (is (= t (tree/root t))))))

(deftest tree-position-of-root
  (testing "position of root is empty vector"
    (is (= [] (tree/position {})))))

(deftest tree-find-walks-upward
  (testing "find walks up parent chain"
    (let [env (-> {}
                  (tree/ensure-path ['a])
                  (tree/put ['a] :domain (dom/single 42))
                  (tree/ensure-path ['a 'b 'c]))]
      ;; Navigate to c, find a
      (let [at-c (tree/cd env ['a 'b 'c])
            found (tree/find at-c ['a])]
        ;; Should find 'a' by walking up from c → b → a's parent
        ;; Actually, find at c looks for [a] as child of c, then child of b, etc.
        ;; It won't find the top-level 'a' because find looks for a path, 
        ;; and 'a' is at root level but we're inside a/b/c
        ;; This is correct lexical scoping behavior
        (is (some? found))))))

;; ════════════════════════════════════════════════════════════════
;; 24. MIXED OPERATIONS
;; ════════════════════════════════════════════════════════════════

(deftest constraint-chain-plus-minus
  (testing "chaining + and - constraints"
    ;; a=1, s=a+b=4 → b=3, d=s-a=4-1=3
    (is (= #{[1 3 4 3]}
           (set (m/query (let [a (between 1 5)
                               b (between 1 5)
                               s (+ a b)
                               d (- s a)]
                           (and (= s 4) (= a 1)
                                [a b s d]))))))))

(deftest multiple-constraints-same-var
  (testing "multiple constraints all narrow the same variable"
    (is (= [5]
           (m/query (let [x (between 1 10)]
                      (and (> x 3) (< x 7) (odd x) x)))))))

(deftest nested-fn-call-with-constraint
  (testing "fn call result used in constraint"
    (is (= [3]
           (m/query (let [double (fn [x] (+ x x))
                          n (between 1 5)]
                      (and (= (double n) 6) n)))))))

;; ════════════════════════════════════════════════════════════════
;; 25. PERFORMANCE / SCALE TESTS
;; ════════════════════════════════════════════════════════════════

(deftest large-domain-with-tight-constraint
  (testing "between 1..500 narrowed to 1 value is fast"
    (let [t0 (System/nanoTime)
          results (m/query (let [x (between 1 500)]
                             (and (= x 42) x)))
          elapsed-ms (/ (- (System/nanoTime) t0) 1e6)]
      (is (= [42] results))
      (is (< elapsed-ms 500) "Should complete in under 500ms"))))

(deftest four-queens-solvable
  (testing "4-queens is solvable with our constraint system"
    (let [results (m/query (let [q1 (between 1 4)
                                 q2 (between 1 4)
                                 q3 (between 1 4)
                                 q4 (between 1 4)]
                             (and (distinct [q1 q2 q3 q4])
                                  ;; No diagonal attacks
                                  (!= (abs (- q1 q2)) 1)
                                  (!= (abs (- q1 q3)) 2)
                                  (!= (abs (- q1 q4)) 3)
                                  (!= (abs (- q2 q3)) 1)
                                  (!= (abs (- q2 q4)) 2)
                                  (!= (abs (- q3 q4)) 1)
                                  [q1 q2 q3 q4])))]
      (is (= #{[2 4 1 3] [3 1 4 2]} (set results))))))

;; ════════════════════════════════════════════════════════════════
;; 26. EDGE: SINGLE VALUE DOMAIN OPERATIONS  
;; ════════════════════════════════════════════════════════════════

(deftest single-value-arithmetic
  (testing "arithmetic on single-value domains"
    (is (= [7] (m/query (+ 3 4))))
    (is (= [1] (m/query (- 5 4))))
    (is (= [12] (m/query (* 3 4))))))

(deftest equality-of-same-var
  (testing "(= x x) is trivially true"
    (is (= #{1 2 3}
           (set (m/query (let [x (one-of 1 2 3)]
                           (and (= x x) x))))))))

(deftest neq-of-same-var
  (testing "(!= x x) is contradiction"
    (is (thrown? clojure.lang.ExceptionInfo
                 (m/query (let [x (one-of 1)]
                            (and (!= x x) x)))))))
