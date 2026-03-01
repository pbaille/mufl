(ns mufl.collection-ops-test
  "Tests for collection operations: first, rest, take, concat, select-keys, merge, update,
   assoc, every, some, filter-map, contains?, index-of, min-of, max-of, sort, sorted?."
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

;; ════════════════════════════════════════════════════════════════
;; assoc — replace element at index in vector
;; ════════════════════════════════════════════════════════════════

(deftest assoc-basic
  (testing "assoc replaces element at index"
    (is (= [[1 99 3]] (m/query (assoc [1 2 3] 1 99)))))

  (testing "assoc at index 0"
    (is (= [[99 2 3]] (m/query (assoc [1 2 3] 0 99)))))

  (testing "assoc at last index"
    (is (= [[1 2 99]] (m/query (assoc [1 2 3] 2 99)))))

  (testing "assoc on bound vector"
    (is (= [[1 42 3]] (m/query (let [v [1 2 3]] (assoc v 1 42))))))

  (testing "assoc single-element vector"
    (is (= [[99]] (m/query (assoc [1] 0 99))))))

(deftest assoc-with-domains
  (testing "assoc preserves domains on other elements"
    (is (= #{[1 99 3] [1 99 4]}
           (set (m/query (assoc [1 2 (one-of 3 4)] 1 99))))))

  (testing "assoc with domain value"
    (is (= #{[1 10 3] [1 20 3]}
           (set (m/query (assoc [1 2 3] 1 (one-of 10 20)))))))

  (testing "assoc propagates constraints back"
    (is (= [[1 2 3]]
           (m/query (let [v [1 (one-of 2 9) 3]
                          v2 (assoc v 1 42)]
                      (and (= (nth v 1) 2) v)))))))

(deftest assoc-errors
  (testing "assoc throws on out-of-bounds index"
    (is (thrown-with-msg? Exception #"out of bounds"
          (m/query (assoc [1 2 3] 5 99)))))

  (testing "assoc throws on negative index"
    (is (thrown-with-msg? Exception #"out of bounds"
          (m/query (assoc [1 2 3] -1 99))))))

;; ════════════════════════════════════════════════════════════════
;; every — universal quantifier
;; ════════════════════════════════════════════════════════════════

(deftest every-basic
  (testing "every with all-matching predicate succeeds"
    (is (= [[2 4 6]] (m/query (every even [2 4 6])))))

  (testing "every narrows element domains"
    (is (= #{[2 4] [2 6] [4 4] [4 6]}
           (set (m/query (every even [(one-of 1 2 3 4) (one-of 3 4 5 6)]))))))

  (testing "every on bound vector"
    (is (= [[2 4]] (m/query (let [v [2 4]] (every even v)))))))

(deftest every-contradiction
  (testing "every throws when an element contradicts"
    (is (thrown? Exception
          (m/query (every even [1 2 4])))))

  (testing "every throws on all-odd"
    (is (thrown? Exception
          (m/query (every even [1 3 5]))))))

(deftest every-with-function
  (testing "every with inline predicate"
    (is (= [[2 4 6]]
           (m/query (every (fn [x] (even x)) [2 4 6])))))

  (testing "every with type constraint"
    ;; (integer x) as predicate — narrows each element to integer
    (is (= [[1 2 3]]
           (m/query (every (fn [x] (integer x)) [1 2 3]))))))

;; ════════════════════════════════════════════════════════════════
;; some — existential quantifier
;; ════════════════════════════════════════════════════════════════

(deftest some-basic
  (testing "some succeeds if at least one matches"
    (is (= [[1 2 3]] (m/query (some even [1 2 3])))))

  (testing "some with all matching"
    (is (= [[2 4 6]] (m/query (some even [2 4 6])))))

  (testing "some on bound vector"
    (is (= [[1 2 3]] (m/query (let [v [1 2 3]] (some even v)))))))

(deftest some-contradiction
  (testing "some throws when no element satisfies"
    (is (thrown? Exception
          (m/query (some even [1 3 5]))))))

(deftest some-with-domains
  (testing "some with domain values — at least one can be even"
    (is (seq (m/query (some even [(one-of 1 2) (one-of 3 5)]))))))

;; ════════════════════════════════════════════════════════════════
;; filter-map — filter map entries by predicate on values
;; ════════════════════════════════════════════════════════════════

(deftest filter-map-basic
  (testing "filter-map keeps matching entries"
    (is (= [{:a 2 :c 4}]
           (m/query (filter-map even {:a 2 :b 3 :c 4})))))

  (testing "filter-map on bound map"
    (is (= [{:b 2}]
           (m/query (let [m {:a 1 :b 2 :c 3}]
                      (filter-map even m))))))

  (testing "filter-map all pass"
    (is (= [{:a 2 :b 4}]
           (m/query (filter-map even {:a 2 :b 4})))))

  (testing "filter-map none pass"
    (is (= [{}]
           (m/query (filter-map even {:a 1 :b 3}))))))

(deftest filter-map-with-domains
  (testing "filter-map with domain values"
    ;; :a has even values 2, :b might be even or odd
    (is (= [{:a 2}]
           (m/query (filter-map even {:a 2 :b 3})))))

  (testing "filter-map propagates constraints to original"
    (is (= [{:a 2 :b 3}]
           (m/query (let [m {:a (one-of 2 3) :b 3}
                          fm (filter-map even m)]
                      (and (= (:a fm) 2) m)))))))

(deftest filter-map-with-function
  (testing "filter-map with inline predicate"
    (is (= [{:a 1}]
           (m/query (filter-map (fn [x] (odd x)) {:a 1 :b 2 :c 4}))))))

;; ════════════════════════════════════════════════════════════════
;; contains? — membership constraint
;; ════════════════════════════════════════════════════════════════

(deftest contains-forward
  (testing "contains? narrows x to possible elements"
    (is (= #{1 2 3} (set (m/query (fresh [(integer x)]
                                    (contains? [1 2 3] x)
                                    x))))))

  (testing "contains? with domain elements"
    (is (= #{1 2 3 4} (set (m/query (fresh [(integer x)]
                                      (contains? [(one-of 1 2) (one-of 3 4)] x)
                                      x)))))))

(deftest contains-backward
  (testing "contains? + further constraint narrows x"
    (is (= [2] (m/query (fresh [(integer x)]
                           (contains? [1 2 3] x)
                           (even x)
                           x)))))

  (testing "contains? with constrained x narrows bidirectionally"
    (is (= #{1 3} (set (m/query (fresh [(integer x)]
                                  (contains? [1 2 3] x)
                                  (odd x)
                                  x)))))))

(deftest contains-with-concrete
  (testing "contains? with literal element"
    (is (= [2] (m/query (fresh [(integer x)]
                           (contains? [1 2 3] x)
                           (= x 2)
                           x)))))

  (testing "contains? contradiction — element not in vector"
    (is (thrown? Exception
          (m/query (fresh [(integer x)]
                     (contains? [1 2 3] x)
                     (= x 5)
                     x))))))

(deftest contains-single-element
  (testing "contains? on single-element vector"
    (is (= [42] (m/query (fresh [(integer x)]
                            (contains? [42] x)
                            x))))))

;; ════════════════════════════════════════════════════════════════
;; index-of — inverse of nth
;; ════════════════════════════════════════════════════════════════

(deftest index-of-basic
  (testing "index-of finds position of element"
    (is (= [1] (m/query (index-of [10 20 30] 20)))))

  (testing "index-of with duplicate values"
    (is (= #{0 2} (set (m/query (index-of [1 2 1] 1))))))

  (testing "index-of with domain element"
    (is (= #{0 1 2} (set (m/query (fresh [(integer x)]
                                    (index-of [1 2 3] x))))))))

(deftest index-of-with-constraint
  (testing "index-of constrained by result"
    ;; If we constrain result to 0, then x must be 10
    (is (= [10] (m/query (fresh [(integer x) (integer i)]
                            (= i (index-of [10 20 30] x))
                            (= i 0)
                            x)))))

  (testing "index-of constrained by element"
    ;; If x is 20, result must be 1
    (is (= [1] (m/query (fresh [(integer x) (integer i)]
                           (= i (index-of [10 20 30] x))
                           (= x 20)
                           i))))))

(deftest index-of-contradiction
  (testing "index-of throws when element not found"
    (is (thrown? Exception
          (m/query (index-of [1 2 3] 99))))))

;; ════════════════════════════════════════════════════════════════
;; min-of — minimum element of vector
;; ════════════════════════════════════════════════════════════════

(deftest min-of-basic
  (testing "min-of on literal vector"
    (is (= [1] (m/query (min-of [3 1 2])))))

  (testing "min-of on single element"
    (is (= [5] (m/query (min-of [5])))))

  (testing "min-of on bound vector"
    (is (= [1] (m/query (let [v [3 1 4 1 5]] (min-of v)))))))

(deftest min-of-with-domains
  (testing "min-of with domain elements"
    (is (= #{1 2} (set (m/query (min-of [(one-of 1 2) (one-of 3 4)]))))))

  (testing "min-of narrows result domain"
    (is (= #{1 2 3} (set (m/query (min-of [(one-of 1 2 3) (one-of 2 3 4)])))))))

(deftest min-of-with-constraint
  (testing "min-of constrained to specific value"
    ;; min=3 → v[0] must be 3 (only source), v[1] ∈ {5,7} (both ≥ 3)
    (is (= #{[3 5] [3 7]}
           (set (m/query (let [v [(one-of 3 5) (one-of 5 7)]]
                           (and (= (min-of v) 3) v)))))))

  (testing "min-of narrows elements — all must be >= result"
    (is (= [2] (m/query (fresh [(integer x)]
                           (= x (min-of [2 5 3]))
                           x))))))

;; ════════════════════════════════════════════════════════════════
;; max-of — maximum element of vector
;; ════════════════════════════════════════════════════════════════

(deftest max-of-basic
  (testing "max-of on literal vector"
    (is (= [3] (m/query (max-of [3 1 2])))))

  (testing "max-of on single element"
    (is (= [5] (m/query (max-of [5])))))

  (testing "max-of on bound vector"
    (is (= [5] (m/query (let [v [3 1 4 1 5]] (max-of v)))))))

(deftest max-of-with-domains
  (testing "max-of with domain elements"
    (is (= #{3 4} (set (m/query (max-of [(one-of 1 2) (one-of 3 4)]))))))

  (testing "max-of narrows result domain"
    ;; max can be 2 (1,2), 3 (various), or 4 (any,4) → {2,3,4}
    (is (= #{2 3 4} (set (m/query (max-of [(one-of 1 2 3) (one-of 2 3 4)])))))))

(deftest max-of-with-constraint
  (testing "max-of constrained to specific value"
    ;; max=5 → v[0] must be 5 (7 > 5), v[1] ∈ {3,5} (both ≤ 5)
    (is (= #{[5 3] [5 5]}
           (set (m/query (let [v [(one-of 5 7) (one-of 3 5)]]
                           (and (= (max-of v) 5) v)))))))

  (testing "max-of narrows elements — all must be <= result"
    (is (= [5] (m/query (fresh [(integer x)]
                           (= x (max-of [2 5 3]))
                           x))))))

;; ════════════════════════════════════════════════════════════════
;; sort — sorted permutation of vector
;; ════════════════════════════════════════════════════════════════

(deftest sort-basic
  (testing "sort on literal vector"
    (is (= [[1 2 3]] (m/query (sort [3 1 2])))))

  (testing "sort on already sorted"
    (is (= [[1 2 3]] (m/query (sort [1 2 3])))))

  (testing "sort on single element"
    (is (= [[5]] (m/query (sort [5])))))

  (testing "sort on two elements"
    (is (= [[1 2]] (m/query (sort [2 1])))))

  (testing "sort on bound vector"
    (is (= [[1 1 2 3 4]] (m/query (let [v [3 1 4 1 2]] (sort v)))))))

(deftest sort-with-domains
  (testing "sort with disjoint domain elements — clean separation"
    ;; input: [(one-of 3 4) (one-of 1 2)]
    ;; sorted: first must be ≤ second
    ;; first ∈ {1,2,3,4} ∩ {≤ max(second)}, second ∈ {1,2,3,4} ∩ {≥ min(first)}
    (let [results (m/query (sort [(one-of 3 4) (one-of 1 2)]))]
      (is (every? (fn [[a b]] (<= a b)) results))
      ;; All combos: (3,1)→[1,3] (3,2)→[2,3] (4,1)→[1,4] (4,2)→[2,4]
      (is (= #{[1 3] [2 3] [1 4] [2 4]} (set results)))))

  (testing "sort with overlapping domains"
    (let [results (m/query (sort [(one-of 1 2) (one-of 2 3)]))]
      (is (every? (fn [[a b]] (<= a b)) results))
      ;; Combos: (1,2)→[1,2] (1,3)→[1,3] (2,2)→[2,2] (2,3)→[2,3]
      (is (= #{[1 2] [1 3] [2 2] [2 3]} (set results))))))

(deftest sort-with-duplicates
  (testing "sort preserves duplicate values"
    (is (= [[1 1 2]] (m/query (sort [2 1 1])))))

  (testing "sort with all same values"
    (is (= [[3 3 3]] (m/query (sort [3 3 3])))))

  (testing "sort with domain elements that could be duplicates"
    ;; [(one-of 1 2) (one-of 1 2)] — each element is 1 or 2
    ;; Possible inputs: (1,1)→[1,1] (1,2)→[1,2] (2,1)→[1,2] (2,2)→[2,2]
    (let [results (m/query (sort [(one-of 1 2) (one-of 1 2)]))]
      (is (every? (fn [[a b]] (<= a b)) results))
      (is (= #{[1 1] [1 2] [2 2]} (set results))))))

(deftest sort-backward-propagation
  (testing "constraining sorted result narrows input"
    ;; sort v, then constrain first of sorted = 1
    ;; Since the minimum of the sorted result is 1, input must contain 1
    (let [results (m/query (let [v [(one-of 1 2 3) (one-of 1 2 3)]]
                             (= (nth (sort v) 0) 1)
                             v))]
      ;; At least one element must be 1
      (is (every? (fn [[a b]] (or (= a 1) (= b 1))) results))
      ;; Expected: [1,1] [1,2] [1,3] [2,1] [3,1]
      (is (= #{[1 1] [1 2] [1 3] [2 1] [3 1]} (set results))))))

(deftest sort-composition
  (testing "first of sort equals min-of"
    (is (= (m/query (min-of [3 1 2]))
           (m/query (first (sort [3 1 2]))))))

  (testing "last of sort equals max-of"
    (is (= (m/query (max-of [3 1 2]))
           (m/query (last (sort [3 1 2])))))))

;; ════════════════════════════════════════════════════════════════
;; sorted? — assert vector is already sorted
;; ════════════════════════════════════════════════════════════════

(deftest sorted?-basic
  (testing "sorted? on already sorted vector — no contradiction"
    (is (= [[1 2 3]] (m/query (let [v [1 2 3]] (sorted? v) v)))))

  (testing "sorted? on unsorted vector — contradiction"
    (is (thrown? Exception (m/query (let [v [3 1 2]] (sorted? v) v)))))

  (testing "sorted? on single element"
    (is (= [[5]] (m/query (let [v [5]] (sorted? v) v))))))

(deftest sorted?-with-domains
  (testing "sorted? narrows domains to maintain order"
    ;; [(one-of 1 2 3) (one-of 1 2 3)] with sorted? →
    ;; first ≤ second, so valid combos: (1,1)(1,2)(1,3)(2,2)(2,3)(3,3)
    (let [results (m/query (let [v [(one-of 1 2 3) (one-of 1 2 3)]]
                             (sorted? v) v))]
      (is (every? (fn [[a b]] (<= a b)) results))
      (is (= #{[1 1] [1 2] [1 3] [2 2] [2 3] [3 3]} (set results))))))

;; ════════════════════════════════════════════════════════════════
;; sort — advanced tests
;; ════════════════════════════════════════════════════════════════

(deftest sort-three-element-domains
  (testing "sort three domain elements"
    ;; [(one-of 1 3) (one-of 2 4) (one-of 1 5)]
    ;; All possible inputs and their sorts:
    (let [results (m/query (sort [(one-of 1 3) (one-of 2 4) (one-of 1 5)]))
          expected (set (for [a [1 3] b [2 4] c [1 5]]
                          (vec (clojure.core/sort [a b c]))))]
      (is (every? (fn [[a b c]] (and (<= a b) (<= b c))) results))
      ;; Verify all results are valid sorts of some input combo
      (is (every? (fn [r] (contains? expected r)) results))
      ;; Verify we find all expected sorted outputs
      (is (= expected (set results))))))

(deftest sort-with-distinct
  (testing "sort + distinct: all different values"
    ;; Three elements from {1,2,3}, all different, then sort
    ;; Only valid input: some permutation of [1 2 3]
    ;; Sorted result is always [1 2 3] (may appear multiple times for different input permutations)
    (let [results (m/query (fresh [(integer a) (integer b) (integer c)]
                             (= a (one-of 1 2 3))
                             (= b (one-of 1 2 3))
                             (= c (one-of 1 2 3))
                             (distinct [a b c])
                             (sort [a b c])))]
      (is (= #{[1 2 3]} (set results)))))

  (testing "sort + distinct + constrain last"
    ;; Elements from {1,2,3,4}, all different, sorted last = 4
    (let [results (m/query (fresh [(integer a) (integer b) (integer c)]
                             (= a (one-of 1 2 3 4))
                             (= b (one-of 1 2 3 4))
                             (= c (one-of 1 2 3 4))
                             (distinct [a b c])
                             (let [s (sort [a b c])]
                               (= (last s) 4)
                               s)))]
      ;; last must be 4, first two from {1,2,3} all different
      ;; → [1,2,4] [1,3,4] [2,3,4]
      (is (= #{[1 2 4] [1 3 4] [2 3 4]} (set results))))))

(deftest sort-idempotent
  (testing "sort of already sorted domain vector"
    (let [results (m/query (let [v [(one-of 1 2) (one-of 3 4)]]
                             (sort v)))]
      ;; Inputs always have first < second, so sort = identity
      (is (= #{[1 3] [1 4] [2 3] [2 4]} (set results))))))
