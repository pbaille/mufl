(ns mufl.tree-test
  (:require [clojure.test :refer [deftest testing is]]
            [mufl.tree :as tree]
            [mufl.domain :as dom]))

;; ════════════════════════════════════════════════════════════════
;; Tree navigation edge cases
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
