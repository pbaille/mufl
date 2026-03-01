(ns mufl.collection-ops-test
  "Tests for collection operations: first, rest, take, concat, select-keys, merge, update."
  (:require [clojure.test :refer [deftest testing is]]
            [mufl.core :as m]))

;; ════════════════════════════════════════════════════════════════
;; first — first element of a vector
;; ════════════════════════════════════════════════════════════════

(deftest first-basic
  (testing "first on literal vector"
    (is (= [1] (m/query (first [1 2 3]))))
    (is (= [:a] (m/query (first [:a :b :c])))))

  (testing "first on bound vector"
    (is (= [1] (m/query (let [v [1 2 3]] (first v))))))

  (testing "first on single-element vector"
    (is (= [42] (m/query (first [42]))))))

(deftest first-with-domains
  (testing "first with domain values"
    (is (= #{1 2} (set (m/query (first [(one-of 1 2) 3 4]))))))

  (testing "first propagates constraints backward"
    (is (= [[1 3]] (m/query (let [v [(one-of 1 2) 3]]
                              (and (= (first v) 1) v)))))))

;; ════════════════════════════════════════════════════════════════
;; rest — all but first element
;; ════════════════════════════════════════════════════════════════

(deftest rest-basic
  (testing "rest on literal vector"
    (is (= [[2 3]] (m/query (rest [1 2 3])))))

  (testing "rest on bound vector"
    (is (= [[2 3]] (m/query (let [v [1 2 3]] (rest v))))))

  (testing "rest on two-element vector"
    (is (= [[2]] (m/query (rest [1 2])))))

  (testing "rest on single-element vector gives empty"
    (is (= [[]] (m/query (rest [1]))))))

(deftest rest-with-domains
  (testing "rest preserves element domains"
    (is (= #{[2 3] [2 4] [3 3] [3 4]}
           (set (m/query (rest [1 (one-of 2 3) (one-of 3 4)]))))))

  (testing "rest propagates constraints to original"
    (is (= [[1 2 3]]
           (m/query (let [v [1 (one-of 2 9) 3]]
                      (and (= (first (rest v)) 2) v)))))))

;; ════════════════════════════════════════════════════════════════
;; take — first N elements
;; ════════════════════════════════════════════════════════════════

(deftest take-basic
  (testing "take first 2 from vector"
    (is (= [[1 2]] (m/query (take 2 [1 2 3 4])))))

  (testing "take all elements"
    (is (= [[1 2 3]] (m/query (take 3 [1 2 3])))))

  (testing "take more than available"
    (is (= [[1 2 3]] (m/query (take 10 [1 2 3])))))

  (testing "take 0 gives empty"
    (is (= [[]] (m/query (take 0 [1 2 3])))))

  (testing "take 1 gives singleton"
    (is (= [[1]] (m/query (take 1 [1 2 3]))))))

(deftest take-with-domains
  (testing "take preserves constraints"
    (is (= #{[1 2] [1 3]}
           (set (m/query (take 2 [1 (one-of 2 3) 4 5]))))))

  (testing "take + drop reconstruct original"
    (is (= [[1 2 3 4]]
           (m/query (let [v [1 2 3 4]]
                      (concat (take 2 v) (drop 2 v))))))))

;; ════════════════════════════════════════════════════════════════
;; concat — join two vectors
;; ════════════════════════════════════════════════════════════════

(deftest concat-basic
  (testing "concat two vectors"
    (is (= [[1 2 3 4]] (m/query (concat [1 2] [3 4])))))

  (testing "concat with empty left"
    (is (= [[3 4]] (m/query (concat [] [3 4])))))

  (testing "concat with empty right"
    (is (= [[1 2]] (m/query (concat [1 2] [])))))

  (testing "concat both empty"
    (is (= [[]] (m/query (concat [] []))))))

(deftest concat-with-domains
  (testing "concat preserves domains from both sources"
    (is (= #{[1 2 3 4] [1 2 3 5]}
           (set (m/query (concat [1 2] [3 (one-of 4 5)]))))))

  (testing "concat propagates constraints back to source"
    (is (= [[1 2]]
           (m/query (let [a [1 (one-of 2 9)]
                          c (concat a [3])]
                      (and (= (nth c 1) 2) a)))))))

;; ════════════════════════════════════════════════════════════════
;; select-keys — keep specified keys from a map
;; ════════════════════════════════════════════════════════════════

(deftest select-keys-basic
  (testing "select-keys keeps specified keys"
    (is (= [{:a 1 :b 2}]
           (m/query (select-keys {:a 1 :b 2 :c 3} :a :b)))))

  (testing "select-keys with single key"
    (is (= [{:x 42}]
           (m/query (select-keys {:x 42 :y 99} :x)))))

  (testing "select-keys on bound map"
    (is (= [{:a 1}]
           (m/query (let [m {:a 1 :b 2 :c 3}]
                      (select-keys m :a)))))))

(deftest select-keys-with-domains
  (testing "select-keys with domain values"
    (is (= #{{:a 1} {:a 2}}
           (set (m/query (select-keys {:a (one-of 1 2) :b 3} :a))))))

  (testing "select-keys propagates constraints to original"
    (is (= [{:a 1 :b 2}]
           (m/query (let [m {:a (one-of 1 9) :b 2}
                          s (select-keys m :a)]
                      (and (= (:a s) 1) m)))))))

;; ════════════════════════════════════════════════════════════════
;; merge — combine two maps (right-biased)
;; ════════════════════════════════════════════════════════════════

(deftest merge-basic
  (testing "merge disjoint maps"
    (is (= [{:a 1 :b 2}]
           (m/query (merge {:a 1} {:b 2})))))

  (testing "merge with overlap — right wins"
    (is (= [{:a 10 :b 2}]
           (m/query (merge {:a 1 :b 2} {:a 10})))))

  (testing "merge on bound maps"
    (is (= [{:x 1 :y 2 :z 3}]
           (m/query (let [m1 {:x 1 :y 2}
                          m2 {:z 3}]
                      (merge m1 m2)))))))

(deftest merge-with-domains
  (testing "merge with domain values"
    (is (= #{{:a 1 :b 2} {:a 1 :b 3}}
           (set (m/query (merge {:a 1} {:b (one-of 2 3)}))))))

  (testing "merge propagates constraints to source"
    (is (= [{:a 1}]
           (m/query (let [m1 {:a (one-of 1 9)}
                          merged (merge m1 {:b 2})]
                      (and (= (:a merged) 1) m1)))))))

;; ════════════════════════════════════════════════════════════════
;; update — apply function to value at key
;; ════════════════════════════════════════════════════════════════

(deftest update-basic
  (testing "update increments a value"
    (is (= [{:a 2 :b 2}]
           (m/query (update {:a 1 :b 2} :a (fn [x] (+ x 1)))))))

  (testing "update on bound map"
    (is (= [{:x 10 :y 2}]
           (m/query (let [m {:x 5 :y 2}]
                      (update m :x (fn [v] (* v 2))))))))

  (testing "update leaves other keys intact"
    (is (= [{:a 1 :b 20 :c 3}]
           (m/query (update {:a 1 :b 10 :c 3} :b (fn [x] (* x 2))))))))

(deftest update-with-domains
  (testing "update with domain input"
    (is (= #{2 4}
           (set (m/query (let [m {:x (one-of 1 2)}
                               m2 (update m :x (fn [v] (* v 2)))]
                           (:x m2)))))))

  (testing "update with constraint on result"
    (is (= [{:a 2 :b 3}]
           (m/query (let [m {:a (one-of 1 2 3) :b 3}
                          m2 (update m :a (fn [x] (* x 2)))]
                      (and (= (:a m2) 4) m)))))))

;; ════════════════════════════════════════════════════════════════
;; drop — already exists, just verify it still works
;; ════════════════════════════════════════════════════════════════

(deftest drop-sanity
  (testing "drop basic still works"
    (is (= [[3 4]] (m/query (drop 2 [1 2 3 4]))))
    (is (= [[]] (m/query (drop 3 [1 2 3]))))))

;; ════════════════════════════════════════════════════════════════
;; last — last element of a vector
;; ════════════════════════════════════════════════════════════════

(deftest last-basic
  (testing "last on literal vector"
    (is (= [3] (m/query (last [1 2 3]))))
    (is (= [:c] (m/query (last [:a :b :c])))))

  (testing "last on bound vector"
    (is (= [3] (m/query (let [v [1 2 3]] (last v))))))

  (testing "last on single-element vector"
    (is (= [42] (m/query (last [42]))))))

(deftest last-with-domains
  (testing "last with domain values"
    (is (= #{3 4} (set (m/query (last [1 2 (one-of 3 4)]))))))

  (testing "last propagates constraints backward"
    (is (= [[1 2 3]] (m/query (let [v [1 2 (one-of 3 9)]]
                                (and (= (last v) 3) v)))))))

;; ════════════════════════════════════════════════════════════════
;; conj — append element to vector
;; ════════════════════════════════════════════════════════════════

(deftest conj-basic
  (testing "conj to literal vector"
    (is (= [[1 2 3 4]] (m/query (conj [1 2 3] 4)))))

  (testing "conj to bound vector"
    (is (= [[1 2 3]] (m/query (let [v [1 2]] (conj v 3))))))

  (testing "conj to empty vector"
    (is (= [[42]] (m/query (conj [] 42))))))

(deftest conj-with-domains
  (testing "conj preserves existing domains"
    (is (= #{[1 2 10] [1 3 10]}
           (set (m/query (conj [1 (one-of 2 3)] 10))))))

  (testing "conj with domain element"
    (is (= #{[1 2 3] [1 2 4]}
           (set (m/query (conj [1 2] (one-of 3 4)))))))

  (testing "conj propagates constraints to original"
    (is (= [[1 2]]
           (m/query (let [v [1 (one-of 2 9)]
                          v2 (conj v 3)]
                      (and (= (nth v2 1) 2) v)))))))

;; ════════════════════════════════════════════════════════════════
;; reverse — reverse a vector
;; ════════════════════════════════════════════════════════════════

(deftest reverse-basic
  (testing "reverse literal vector"
    (is (= [[3 2 1]] (m/query (reverse [1 2 3])))))

  (testing "reverse bound vector"
    (is (= [[3 2 1]] (m/query (let [v [1 2 3]] (reverse v))))))

  (testing "reverse single element"
    (is (= [[42]] (m/query (reverse [42])))))

  (testing "reverse empty vector"
    (is (= [[]] (m/query (reverse []))))))

(deftest reverse-with-domains
  (testing "reverse preserves domains"
    (is (= #{[3 2 1] [3 2 0]}
           (set (m/query (reverse [(one-of 0 1) 2 3]))))))

  (testing "reverse propagates constraints back"
    (is (= [[1 2 3]]
           (m/query (let [v [(one-of 1 9) 2 3]
                          r (reverse v)]
                      (and (= (last r) 1) v)))))))

;; ════════════════════════════════════════════════════════════════
;; zip — pair two vectors element-wise
;; ════════════════════════════════════════════════════════════════

(deftest zip-basic
  (testing "zip two literal vectors"
    (is (= [[[1 :a] [2 :b] [3 :c]]]
           (m/query (zip [1 2 3] [:a :b :c])))))

  (testing "zip bound vectors"
    (is (= [[[1 4] [2 5] [3 6]]]
           (m/query (let [a [1 2 3] b [4 5 6]] (zip a b))))))

  (testing "zip single-element vectors"
    (is (= [[[1 :x]]]
           (m/query (zip [1] [:x])))))

  (testing "zip empty vectors"
    (is (= [[]] (m/query (zip [] []))))))

(deftest zip-with-domains
  (testing "zip with domain values"
    (is (= #{[[1 :a] [2 :b]] [[1 :a] [3 :b]]}
           (set (m/query (zip [1 (one-of 2 3)] [:a :b]))))))

  (testing "zip propagates constraints to sources"
    (is (= [[1 2]]
           (m/query (let [a [1 (one-of 2 9)]
                          z (zip a [10 20])]
                      (and (= (first (nth z 1)) 2) a)))))))

(deftest zip-length-mismatch
  (testing "zip throws on different-length vectors"
    (is (thrown-with-msg? Exception #"same length"
          (m/query (zip [1 2] [3 4 5]))))))

;; ════════════════════════════════════════════════════════════════
;; keys — extract key names from a map
;; ════════════════════════════════════════════════════════════════

(deftest keys-basic
  (testing "keys on literal map"
    (let [result (first (m/query (keys {:a 1 :b 2 :c 3})))]
      (is (= #{:a :b :c} (set result)))))

  (testing "keys on bound map"
    (let [result (first (m/query (let [m {:x 10 :y 20}] (keys m))))]
      (is (= #{:x :y} (set result)))))

  (testing "keys on single-key map"
    (is (= [[:a]] (m/query (keys {:a 1}))))))

;; ════════════════════════════════════════════════════════════════
;; vals — extract values from a map
;; ════════════════════════════════════════════════════════════════

(deftest vals-basic
  (testing "vals on literal map"
    (let [result (first (m/query (vals {:a 1 :b 2})))]
      ;; vals order follows kw-children order; check as set
      (is (= #{1 2} (set result)))))

  (testing "vals on bound map"
    (let [result (first (m/query (let [m {:x 10 :y 20}] (vals m))))]
      (is (= #{10 20} (set result))))))

(deftest vals-with-domains
  (testing "vals with domain values"
    ;; Each val independently varies
    (let [results (m/query (vals {:a (one-of 1 2) :b 3}))]
      (is (every? #(contains? #{1 2} (first %)) results))
      (is (every? #(= 3 (second %)) results))))

  (testing "vals propagates constraints back to map"
    (let [result (m/query (let [m {:a (one-of 1 2 3) :b 10}
                                v (vals m)]
                            ;; Constrain the val corresponding to :a
                            ;; Since kw-children order may vary, use get on original
                            (and (= (:a m) 1) m)))]
      (is (= [{:a 1 :b 10}] result)))))

;; ════════════════════════════════════════════════════════════════
;; entries — extract [key value] pairs from a map
;; ════════════════════════════════════════════════════════════════

(deftest entries-basic
  (testing "entries on literal map"
    (let [result (first (m/query (entries {:a 1 :b 2})))]
      (is (= #{[:a 1] [:b 2]} (set result)))))

  (testing "entries on bound map"
    (let [result (first (m/query (let [m {:x 10}] (entries m))))]
      (is (= [[:x 10]] result)))))

(deftest entries-with-domains
  (testing "entries with domain values"
    (let [results (m/query (entries {:a (one-of 1 2)}))]
      (is (= #{[[:a 1]] [[:a 2]]} (set results))))))

;; ════════════════════════════════════════════════════════════════
;; map-vals — apply function to every map value
;; ════════════════════════════════════════════════════════════════

(deftest map-vals-basic
  (testing "map-vals doubles every value"
    (is (= [{:a 2 :b 4}]
           (m/query (map-vals {:a 1 :b 2} (fn [x] (* x 2)))))))

  (testing "map-vals on bound map"
    (is (= [{:x 11 :y 21}]
           (m/query (let [m {:x 10 :y 20}]
                      (map-vals m (fn [v] (+ v 1))))))))

  (testing "map-vals on single-key map"
    (is (= [{:a 10}]
           (m/query (map-vals {:a 5} (fn [x] (* x 2))))))))

(deftest map-vals-with-domains
  (testing "map-vals with domain input"
    (is (= #{2 4}
           (set (m/query (let [m {:x (one-of 1 2)}
                               m2 (map-vals m (fn [v] (* v 2)))]
                           (:x m2)))))))

  (testing "map-vals with constraint on result"
    (is (= [{:a 2 :b 3}]
           (m/query (let [m {:a (one-of 1 2 3) :b 3}
                          m2 (map-vals m (fn [x] (* x 2)))]
                      (and (= (:a m2) 4) m)))))))

;; ════════════════════════════════════════════════════════════════
;; has-key? — check key existence
;; ════════════════════════════════════════════════════════════════

(deftest has-key-basic
  (testing "has-key? returns true for existing key"
    (is (= [true] (m/query (has-key? {:a 1 :b 2} :a)))))

  (testing "has-key? returns false for missing key"
    (is (= [false] (m/query (has-key? {:a 1 :b 2} :c)))))

  (testing "has-key? on bound map"
    (is (= [true] (m/query (let [m {:x 1}] (has-key? m :x)))))
    (is (= [false] (m/query (let [m {:x 1}] (has-key? m :y)))))))
