(ns mufl.bind-test
  (:require [clojure.test :refer [deftest testing is]]
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
