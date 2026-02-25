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

;; ════════════════════════════════════════════════════════════════
;; relate tests
;; ════════════════════════════════════════════════════════════════

(deftest relate-void-any
  (testing "void relates"
    (is (= :equal   (d/relate d/void d/void)))
    (is (= :subset  (d/relate d/void d/any)))
    (is (= :subset  (d/relate d/void (d/single 1))))
    (is (= :subset  (d/relate d/void (d/finite #{1 2 3}))))
    (is (= :subset  (d/relate d/void d/integer-dom))))

  (testing "any relates"
    (is (= :equal    (d/relate d/any d/any)))
    (is (= :superset (d/relate d/any d/void)))
    (is (= :superset (d/relate d/any (d/single 1))))
    (is (= :superset (d/relate d/any (d/finite #{1 2 3}))))
    (is (= :superset (d/relate d/any d/string-dom)))))

(deftest relate-single
  (testing "single vs single"
    (is (= :equal    (d/relate (d/single 1) (d/single 1))))
    (is (= :disjoint (d/relate (d/single 1) (d/single 2)))))

  (testing "single vs finite"
    (is (= :subset   (d/relate (d/single 2) (d/finite #{1 2 3}))))
    (is (= :disjoint (d/relate (d/single 5) (d/finite #{1 2 3})))))

  (testing "single vs type"
    (is (= :subset   (d/relate (d/single 42) d/integer-dom)))
    (is (= :subset   (d/relate (d/single "hi") d/string-dom)))
    (is (= :disjoint (d/relate (d/single 42) d/string-dom)))))

(deftest relate-finite
  (testing "finite vs finite"
    (is (= :equal    (d/relate (d/finite #{1 2 3}) (d/finite #{1 2 3}))))
    (is (= :subset   (d/relate (d/finite #{1 2}) (d/finite #{1 2 3}))))
    (is (= :superset (d/relate (d/finite #{1 2 3}) (d/finite #{1 2}))))
    (is (= :disjoint (d/relate (d/finite #{1 2}) (d/finite #{3 4}))))
    (is (= :overlap  (d/relate (d/finite #{1 2 3}) (d/finite #{2 3 4})))))

  (testing "finite vs type"
    (is (= :subset   (d/relate (d/finite #{1 2 3}) d/integer-dom)))
    (is (= :disjoint (d/relate (d/finite #{1 2 3}) d/string-dom)))
    (is (= :overlap  (d/relate (d/finite #{1 2 "x"}) d/integer-dom)))))

(deftest relate-type
  (testing "type vs type"
    (is (= :equal    (d/relate d/integer-dom d/integer-dom)))
    (is (= :equal    (d/relate d/string-dom d/string-dom)))
    (is (= :subset   (d/relate d/integer-dom d/number-dom)))
    (is (= :superset (d/relate d/number-dom d/integer-dom)))
    (is (= :disjoint (d/relate d/string-dom d/integer-dom)))
    (is (= :disjoint (d/relate d/keyword-dom d/number-dom))))

  (testing "type vs single (via flip)"
    (is (= :superset (d/relate d/integer-dom (d/single 42))))
    (is (= :disjoint (d/relate d/string-dom (d/single 42)))))

  (testing "type vs finite (via flip)"
    (is (= :superset (d/relate d/integer-dom (d/finite #{1 2 3}))))
    (is (= :disjoint (d/relate d/string-dom (d/finite #{1 2 3}))))
    (is (= :overlap  (d/relate d/integer-dom (d/finite #{1 2 "x"})))))

;; ════════════════════════════════════════════════════════════════
;; Composite domain tests
;; ════════════════════════════════════════════════════════════════

(deftest composite-construction
  (testing "vector-of-dom"
    (is (= {:kind :vector-of, :element d/integer-dom}
           (d/vector-of-dom d/integer-dom))))
  (testing "tuple-dom"
    (is (= {:kind :tuple, :elements [d/integer-dom d/string-dom]}
           (d/tuple-dom [d/integer-dom d/string-dom]))))
  (testing "map-of-dom"
    (is (= {:kind :map-of, :key d/keyword-dom, :value d/integer-dom}
           (d/map-of-dom d/keyword-dom d/integer-dom))))
  (testing "equality — pure data maps"
    (is (= (d/vector-of-dom d/integer-dom) (d/vector-of-dom d/integer-dom)))
    (is (= (d/tuple-dom [d/integer-dom d/string-dom])
           (d/tuple-dom [d/integer-dom d/string-dom])))
    (is (= (d/map-of-dom d/keyword-dom d/integer-dom)
           (d/map-of-dom d/keyword-dom d/integer-dom)))))

(deftest composite-members-finite
  (testing "composite domains are non-enumerable"
    (is (nil? (d/members (d/vector-of-dom d/integer-dom))))
    (is (nil? (d/members (d/tuple-dom [d/integer-dom d/string-dom]))))
    (is (nil? (d/members (d/map-of-dom d/keyword-dom d/integer-dom)))))
  (testing "composite domains are not finite"
    (is (false? (d/finite? (d/vector-of-dom d/integer-dom))))
    (is (false? (d/finite? (d/tuple-dom [d/integer-dom]))))
    (is (false? (d/finite? (d/map-of-dom d/string-dom d/string-dom))))))

(deftest composite-contains-val
  (testing "vector-of contains-val?"
    (is (d/contains-val? (d/vector-of-dom d/integer-dom) [1 2 3]))
    (is (d/contains-val? (d/vector-of-dom d/integer-dom) []))
    (is (not (d/contains-val? (d/vector-of-dom d/integer-dom) [1 "x"])))
    (is (not (d/contains-val? (d/vector-of-dom d/integer-dom) '(1 2 3))))
    (is (not (d/contains-val? (d/vector-of-dom d/integer-dom) "hello"))))

  (testing "tuple contains-val?"
    (is (d/contains-val? (d/tuple-dom [d/integer-dom d/string-dom]) [1 "hi"]))
    (is (not (d/contains-val? (d/tuple-dom [d/integer-dom d/string-dom]) ["hi" 1])))
    (is (not (d/contains-val? (d/tuple-dom [d/integer-dom d/string-dom]) [1])))
    (is (not (d/contains-val? (d/tuple-dom [d/integer-dom d/string-dom]) [1 "hi" 3])))
    (is (not (d/contains-val? (d/tuple-dom [d/integer-dom]) "hello"))))

  (testing "map-of contains-val?"
    (is (d/contains-val? (d/map-of-dom d/keyword-dom d/integer-dom) {:a 1 :b 2}))
    (is (d/contains-val? (d/map-of-dom d/keyword-dom d/integer-dom) {}))
    (is (not (d/contains-val? (d/map-of-dom d/keyword-dom d/integer-dom) {:a "x"})))
    (is (not (d/contains-val? (d/map-of-dom d/keyword-dom d/integer-dom) {"a" 1})))
    (is (not (d/contains-val? (d/map-of-dom d/keyword-dom d/integer-dom) [1 2]))))

  (testing "nested composite contains-val?"
    (let [matrix (d/vector-of-dom (d/vector-of-dom d/integer-dom))]
      (is (d/contains-val? matrix [[1 2] [3 4]]))
      (is (d/contains-val? matrix []))
      (is (d/contains-val? matrix [[]]))
      (is (not (d/contains-val? matrix [[1 "x"]]))))))

(deftest relate-vector-of
  (testing "vector-of vs vector-of — covariant"
    (is (= :equal    (d/relate (d/vector-of-dom d/integer-dom) (d/vector-of-dom d/integer-dom))))
    (is (= :subset   (d/relate (d/vector-of-dom d/integer-dom) (d/vector-of-dom d/number-dom))))
    (is (= :superset (d/relate (d/vector-of-dom d/number-dom) (d/vector-of-dom d/integer-dom))))
    (is (= :disjoint (d/relate (d/vector-of-dom d/integer-dom) (d/vector-of-dom d/string-dom)))))

  (testing "vector-of vs flat → disjoint"
    (is (= :disjoint (d/relate (d/vector-of-dom d/integer-dom) d/string-dom)))
    (is (= :disjoint (d/relate (d/vector-of-dom d/integer-dom) d/integer-dom)))
    (is (= :disjoint (d/relate (d/vector-of-dom d/integer-dom) (d/single 42))))
    (is (= :disjoint (d/relate (d/vector-of-dom d/integer-dom) (d/finite #{1 2 3})))))

  (testing "vector-of vs void/any"
    (is (= :subset   (d/relate (d/vector-of-dom d/integer-dom) d/any)))
    (is (= :superset (d/relate (d/vector-of-dom d/integer-dom) d/void)))
    (is (= :superset (d/relate d/any (d/vector-of-dom d/integer-dom))))
    (is (= :subset   (d/relate d/void (d/vector-of-dom d/integer-dom))))))

(deftest relate-tuple
  (testing "tuple vs tuple — same length, product relation"
    (is (= :equal    (d/relate (d/tuple-dom [d/integer-dom d/string-dom])
                               (d/tuple-dom [d/integer-dom d/string-dom]))))
    (is (= :subset   (d/relate (d/tuple-dom [d/integer-dom d/string-dom])
                               (d/tuple-dom [d/number-dom d/string-dom]))))
    (is (= :superset (d/relate (d/tuple-dom [d/number-dom d/string-dom])
                               (d/tuple-dom [d/integer-dom d/string-dom]))))
    (is (= :disjoint (d/relate (d/tuple-dom [d/integer-dom d/string-dom])
                               (d/tuple-dom [d/string-dom d/integer-dom]))))
    (is (= :disjoint (d/relate (d/tuple-dom [d/number-dom d/string-dom])
                               (d/tuple-dom [d/integer-dom d/keyword-dom]))))
    (is (= :overlap  (d/relate (d/tuple-dom [d/number-dom d/integer-dom])
                               (d/tuple-dom [d/integer-dom d/number-dom])))))

  (testing "tuple vs tuple — different lengths → disjoint"
    (is (= :disjoint (d/relate (d/tuple-dom [d/integer-dom])
                               (d/tuple-dom [d/integer-dom d/integer-dom]))))
    (is (= :disjoint (d/relate (d/tuple-dom [d/integer-dom d/string-dom d/boolean-dom])
                               (d/tuple-dom [d/integer-dom d/string-dom])))))

  (testing "tuple vs flat → disjoint"
    (is (= :disjoint (d/relate (d/tuple-dom [d/integer-dom]) d/integer-dom)))
    (is (= :disjoint (d/relate (d/tuple-dom [d/integer-dom]) (d/single 1))))))

(deftest relate-map-of
  (testing "map-of vs map-of — product of key and value relations"
    (is (= :equal    (d/relate (d/map-of-dom d/keyword-dom d/integer-dom)
                               (d/map-of-dom d/keyword-dom d/integer-dom))))
    (is (= :subset   (d/relate (d/map-of-dom d/keyword-dom d/integer-dom)
                               (d/map-of-dom d/keyword-dom d/number-dom))))
    (is (= :superset (d/relate (d/map-of-dom d/keyword-dom d/number-dom)
                               (d/map-of-dom d/keyword-dom d/integer-dom))))
    (is (= :disjoint (d/relate (d/map-of-dom d/keyword-dom d/integer-dom)
                               (d/map-of-dom d/string-dom d/string-dom))))
    (is (= :overlap  (d/relate (d/map-of-dom d/keyword-dom d/number-dom)
                               (d/map-of-dom d/keyword-dom (d/finite #{1 2 "x"}))))))

  (testing "map-of vs flat → disjoint"
    (is (= :disjoint (d/relate (d/map-of-dom d/keyword-dom d/integer-dom) d/string-dom)))
    (is (= :disjoint (d/relate (d/map-of-dom d/keyword-dom d/integer-dom) (d/single 42))))))

(deftest relate-cross-composite
  (testing "vector-of vs tuple → disjoint"
    (is (= :disjoint (d/relate (d/vector-of-dom d/integer-dom)
                               (d/tuple-dom [d/integer-dom d/integer-dom])))))
  (testing "vector-of vs map-of → disjoint"
    (is (= :disjoint (d/relate (d/vector-of-dom d/integer-dom)
                               (d/map-of-dom d/keyword-dom d/integer-dom)))))
  (testing "tuple vs map-of → disjoint"
    (is (= :disjoint (d/relate (d/tuple-dom [d/integer-dom])
                               (d/map-of-dom d/keyword-dom d/integer-dom))))))

(deftest composite-intersect
  (testing "vector-of ∩ vector-of"
    (is (= (d/vector-of-dom d/integer-dom)
           (d/intersect (d/vector-of-dom d/integer-dom) (d/vector-of-dom d/number-dom))))
    (is (= (d/vector-of-dom d/integer-dom)
           (d/intersect (d/vector-of-dom d/number-dom) (d/vector-of-dom d/integer-dom))))
    (is (d/void? (d/intersect (d/vector-of-dom d/integer-dom) (d/vector-of-dom d/string-dom)))))

  (testing "tuple ∩ tuple"
    (is (= (d/tuple-dom [d/integer-dom d/string-dom])
           (d/intersect (d/tuple-dom [d/number-dom d/string-dom])
                        (d/tuple-dom [d/integer-dom d/string-dom]))))
    (is (d/void? (d/intersect (d/tuple-dom [d/integer-dom]) (d/tuple-dom [d/integer-dom d/string-dom])))))

  (testing "map-of ∩ map-of"
    (is (= (d/map-of-dom d/keyword-dom d/integer-dom)
           (d/intersect (d/map-of-dom d/keyword-dom d/number-dom)
                        (d/map-of-dom d/keyword-dom d/integer-dom)))))

  (testing "composite ∩ flat → void"
    (is (d/void? (d/intersect (d/vector-of-dom d/integer-dom) d/string-dom)))
    (is (d/void? (d/intersect d/integer-dom (d/tuple-dom [d/integer-dom])))))

  (testing "composite ∩ any/void"
    (is (= (d/vector-of-dom d/integer-dom)
           (d/intersect (d/vector-of-dom d/integer-dom) d/any)))
    (is (d/void? (d/intersect (d/vector-of-dom d/integer-dom) d/void)))))

(deftest composite-unite
  (testing "vector-of ∪ vector-of"
    (is (= (d/vector-of-dom d/number-dom)
           (d/unite (d/vector-of-dom d/integer-dom) (d/vector-of-dom d/number-dom))))
    (is (= (d/vector-of-dom d/integer-dom)
           (d/unite (d/vector-of-dom d/integer-dom) (d/vector-of-dom d/integer-dom)))))

  (testing "tuple ∪ tuple — same length"
    (is (= (d/tuple-dom [d/number-dom d/string-dom])
           (d/unite (d/tuple-dom [d/integer-dom d/string-dom])
                    (d/tuple-dom [d/number-dom d/string-dom])))))

  (testing "tuple ∪ tuple — different lengths → any"
    (is (= d/any (d/unite (d/tuple-dom [d/integer-dom])
                          (d/tuple-dom [d/integer-dom d/string-dom])))))

  (testing "map-of ∪ map-of"
    (is (= (d/map-of-dom d/keyword-dom d/number-dom)
           (d/unite (d/map-of-dom d/keyword-dom d/integer-dom)
                    (d/map-of-dom d/keyword-dom d/number-dom)))))

  (testing "composite ∪ void/any"
    (is (= (d/vector-of-dom d/integer-dom)
           (d/unite (d/vector-of-dom d/integer-dom) d/void)))
    (is (= d/any (d/unite (d/vector-of-dom d/integer-dom) d/any)))))

(deftest composite-subtract
  (testing "vector-of - vector-of"
    (is (d/void? (d/subtract (d/vector-of-dom d/integer-dom) (d/vector-of-dom d/number-dom))))
    (is (d/void? (d/subtract (d/vector-of-dom d/integer-dom) (d/vector-of-dom d/integer-dom))))
    (is (= (d/vector-of-dom d/integer-dom)
           (d/subtract (d/vector-of-dom d/integer-dom) (d/vector-of-dom d/string-dom)))))

  (testing "superset subtract — approximation"
    (is (= (d/vector-of-dom d/number-dom)
           (d/subtract (d/vector-of-dom d/number-dom) (d/vector-of-dom d/integer-dom)))))

  (testing "composite - void → self"
    (is (= (d/vector-of-dom d/integer-dom)
           (d/subtract (d/vector-of-dom d/integer-dom) d/void))))

  (testing "void - composite → void"
    (is (d/void? (d/subtract d/void (d/vector-of-dom d/integer-dom))))))

;; ── Extended symmetry test with composite samples ───────────────

(deftest relate-symmetry
  (testing "relate is consistent under flip"
    (let [domains [(d/single 1) (d/finite #{1 2 3}) (d/finite #{2 3 4})
                   d/integer-dom d/string-dom d/number-dom
                   d/void d/any
                   ;; composite domains
                   (d/vector-of-dom d/integer-dom)
                   (d/vector-of-dom d/number-dom)
                   (d/vector-of-dom d/string-dom)
                   (d/tuple-dom [d/integer-dom d/string-dom])
                   (d/tuple-dom [d/number-dom d/string-dom])
                   (d/tuple-dom [d/integer-dom])
                   (d/map-of-dom d/keyword-dom d/integer-dom)
                   (d/map-of-dom d/keyword-dom d/number-dom)
                   (d/map-of-dom d/string-dom d/string-dom)]]
      (doseq [a domains, b domains]
        (let [r1 (d/relate a b)
              r2 (d/relate b a)
              expected-flip (case r1
                              :equal :equal
                              :subset :superset
                              :superset :subset
                              :disjoint :disjoint
                              :overlap :overlap)]
          (is (= expected-flip r2)
              (str "relate symmetry: " (pr-str a) " vs " (pr-str b)
                   " → " r1 " but reverse → " r2))))))))
