(ns mufl.domain
  "Domain algebra for mufl.

   A domain is the set of possible values a name can take.
   A value is a singleton domain. There is no separate :value concept."
  (:refer-clojure :exclude [any?])
  (:require [clojure.set]))

;; ════════════════════════════════════════════════════════════════
;; Core domain representations
;; ════════════════════════════════════════════════════════════════
;;
;; A domain is a plain map with a :kind key:
;;   {:kind :void}                 — empty
;;   {:kind :any}                  — everything
;;   {:kind :single, :value v}     — exactly v
;;   {:kind :finite, :values #{…}} — finite set, |values| >= 2
;;
;; We keep it simple. Open types (integer, string, ...) can come later.
;; For Phase 1, we only need finite domains and singletons.
;; ════════════════════════════════════════════════════════════════

(def void {:kind :void})

(def any {:kind :any})

(defn single
  "Domain containing exactly one value."
  [v]
  {:kind :single :value v})

(defn finite
  "Domain from a set of values. Normalizes:
   empty → void, one element → single, else → finite."
  [values]
  (let [s (set values)]
    (case (count s)
      0 void
      1 (single (first s))
      {:kind :finite :values s})))

(defn int-range
  "Domain containing integers from lo to hi (inclusive)."
  [lo hi]
  (finite (set (clojure.core/range lo (inc hi)))))

;; ════════════════════════════════════════════════════════════════
;; Queries
;; ════════════════════════════════════════════════════════════════

(defn void? [d]
  (= :void (:kind d)))

(defn any? [d]
  (= :any (:kind d)))

(defn singleton?
  "Is this domain a single concrete value?"
  [d]
  (= :single (:kind d)))

(defn singleton-val
  "Extract the value from a singleton domain. Returns nil otherwise."
  [d]
  (when (singleton? d)
    (:value d)))

(defn members
  "Returns the set of values in a finite domain, or nil for open domains."
  [d]
  (case (:kind d)
    :void #{}
    :single #{(:value d)}
    :finite (:values d)
    :any nil
    nil))

(defn finite?
  "Is this domain finite (enumerable)?"
  [d]
  (boolean (members d)))

(defn size
  "Number of members, or nil for open domains."
  [d]
  (some-> (members d) count))

(defn contains-val?
  "Does domain d contain value v?"
  [d v]
  (case (:kind d)
    :void false
    :any true
    :single (= (:value d) v)
    :finite (contains? (:values d) v)
    false))

;; ════════════════════════════════════════════════════════════════
;; Algebra
;; ════════════════════════════════════════════════════════════════

(defn intersect
  "Narrowing — values that are in both d1 and d2."
  [d1 d2]
  (cond
    ;; Short circuits
    (void? d1) void
    (void? d2) void
    (any? d1) d2
    (any? d2) d1
    (= d1 d2) d1

    ;; Both finite — set intersection
    (and (members d1) (members d2))
    (finite (clojure.set/intersection (members d1) (members d2)))

    ;; One finite, one open — filter
    (members d1)
    d1  ;; TODO: filter by open domain's predicate when we add open types

    (members d2)
    d2

    ;; Both open — for now, just return d1 (TODO: proper open type intersection)
    :else d1))

(defn unite
  "Widening — values that are in either d1 or d2."
  [d1 d2]
  (cond
    (any? d1) any
    (any? d2) any
    (void? d1) d2
    (void? d2) d1
    (= d1 d2) d1

    ;; Both finite — set union
    (and (members d1) (members d2))
    (finite (clojure.set/union (members d1) (members d2)))

    :else any))

(defn subtract
  "Values in d1 that are not in d2."
  [d1 d2]
  (cond
    (void? d1) void
    (void? d2) d1
    (any? d2) void
    (= d1 d2) void

    (and (members d1) (members d2))
    (finite (clojure.set/difference (members d1) (members d2)))

    :else d1))

;; ════════════════════════════════════════════════════════════════
;; Ordered domain operations (for constraint propagation)
;; ════════════════════════════════════════════════════════════════

(defn- comparable-members
  "Returns sorted seq of comparable (number) members, or nil."
  [d]
  (when-let [ms (members d)]
    (when (every? number? ms)
      (sort ms))))

(defn domain-min
  "Smallest value in domain, or nil."
  [d]
  (first (comparable-members d)))

(defn domain-max
  "Largest value in domain, or nil."
  [d]
  (last (comparable-members d)))

(defn domain-below
  "Values in d that are strictly less than v."
  [d v]
  (if-let [ms (members d)]
    (finite (set (filter #(and (number? %) (< % v)) ms)))
    d))

(defn domain-above
  "Values in d that are strictly greater than v."
  [d v]
  (if-let [ms (members d)]
    (finite (set (filter #(and (number? %) (> % v)) ms)))
    d))

(defn domain-at-most
  "Values in d that are <= v."
  [d v]
  (if-let [ms (members d)]
    (finite (set (filter #(and (number? %) (<= % v)) ms)))
    d))

(defn domain-at-least
  "Values in d that are >= v."
  [d v]
  (if-let [ms (members d)]
    (finite (set (filter #(and (number? %) (>= % v)) ms)))
    d))

(defn domain-filter
  "Filter domain by predicate."
  [d pred]
  (if-let [ms (members d)]
    (finite (set (filter pred ms)))
    d))

;; ════════════════════════════════════════════════════════════════
;; Arithmetic on domains
;; ════════════════════════════════════════════════════════════════

(defn domain-add
  "All pairwise sums of values from d1 and d2."
  [d1 d2]
  (if (and (members d1) (members d2))
    (finite (set (for [a (members d1) b (members d2)] (+ a b))))
    any))

(defn domain-sub
  "All pairwise differences."
  [d1 d2]
  (if (and (members d1) (members d2))
    (finite (set (for [a (members d1) b (members d2)] (- a b))))
    any))

(defn domain-mul
  "All pairwise products."
  [d1 d2]
  (if (and (members d1) (members d2))
    (finite (set (for [a (members d1) b (members d2)] (* a b))))
    any))

(defn domain-mod
  "All pairwise remainders (mod a b) for a in d1, b in d2.
   Filters out b=0."
  [d1 d2]
  (if (and (members d1) (members d2))
    (finite (set (for [a (members d1)
                       b (members d2)
                       :when (not (zero? b))]
                   (mod a b))))
    any))

(defn domain-div
  "All pairwise integer quotients (quot a b).
   Filters out b=0."
  [d1 d2]
  (if (and (members d1) (members d2))
    (finite (set (for [a (members d1)
                       b (members d2)
                       :when (not (zero? b))]
                   (quot a b))))
    any))
