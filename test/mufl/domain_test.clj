(ns mufl.domain-test
  (:require [clojure.test :refer [deftest testing is]]
            [mufl.domain :as d]))

(deftest construction
  (testing "single"
    (is (d/singleton? (d/single 1)))
    (is (= 1 (d/singleton-val (d/single 1)))))

  (testing "finite normalizes"
    (is (= d/void (d/finite #{})))
    (is (d/singleton? (d/finite #{42})))
    (is (= 42 (d/singleton-val (d/finite #{42}))))
    (is (= #{1 2 3} (d/members (d/finite #{1 2 3})))))

  (testing "void and any"
    (is (d/void? d/void))
    (is (d/any? d/any))
    (is (not (d/void? d/any)))
    (is (not (d/any? d/void)))))

(deftest algebra
  (testing "intersect"
    (is (= (d/finite #{2 3}) (d/intersect (d/finite #{1 2 3}) (d/finite #{2 3 4}))))
    (is (d/void? (d/intersect (d/finite #{1 2}) (d/finite #{3 4}))))
    (is (= (d/single 2) (d/intersect (d/finite #{1 2 3}) (d/single 2))))
    (is (= d/void (d/intersect d/void (d/finite #{1 2})))))

  (testing "unite"
    (is (= (d/finite #{1 2 3 4}) (d/unite (d/finite #{1 2 3}) (d/finite #{2 3 4}))))
    (is (= (d/finite #{1 2}) (d/unite d/void (d/finite #{1 2}))))
    (is (= d/any (d/unite d/any (d/finite #{1 2})))))

  (testing "subtract"
    (is (= (d/single 1) (d/subtract (d/finite #{1 2 3}) (d/finite #{2 3}))))
    (is (d/void? (d/subtract (d/finite #{1 2}) (d/finite #{1 2 3}))))))

(deftest ordered-ops
  (testing "domain-below"
    (is (= #{1 2} (d/members (d/domain-below (d/finite #{1 2 3 4 5}) 3)))))

  (testing "domain-above"
    (is (= #{4 5} (d/members (d/domain-above (d/finite #{1 2 3 4 5}) 3)))))

  (testing "domain-min/max"
    (is (= 1 (d/domain-min (d/finite #{1 2 3}))))
    (is (= 3 (d/domain-max (d/finite #{1 2 3}))))))

(deftest arithmetic-ops
  (testing "domain-add"
    (is (= #{3 4 5} (d/members (d/domain-add (d/finite #{1 2}) (d/finite #{2 3}))))))

  (testing "domain-sub"
    (is (= #{-2 -1 0 1} (d/members (d/domain-sub (d/finite #{1 2}) (d/finite #{1 2 3}))))))

  (testing "domain-mul"
    (is (= #{2 3 4 6} (d/members (d/domain-mul (d/finite #{1 2}) (d/finite #{2 3})))))))
