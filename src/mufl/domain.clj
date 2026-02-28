(ns mufl.domain
  "Domain algebra for mufl.

   A domain is the set of possible values a name can take.
   A value is a singleton domain. There is no separate :value concept.

   The algebra is built on a single primitive: `relate`, which returns
   the set-theoretic relation between two domains. `intersect`, `unite`,
   and `subtract` derive from `relate`."
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
;;   {:kind :type, :type :string}   — all strings
;;   {:kind :type, :type :integer}  — all integers
;;   {:kind :type, :type :number}   — all numbers
;;   {:kind :type, :type :keyword}  — all keywords
;;   {:kind :type, :type :boolean}  — all booleans (but finite: true/false)
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

(defn type-dom
  "Domain representing all values of a given type.
   Supported types: :string, :integer, :number, :keyword, :boolean."
  [t]
  {:kind :type :type t})

(def string-dom  (type-dom :string))
(def integer-dom (type-dom :integer))
(def number-dom  (type-dom :number))
(def keyword-dom (type-dom :keyword))
(def boolean-dom (type-dom :boolean))

(defn- type-predicate
  "Returns a predicate for a type domain."
  [t]
  (case t
    :string  string?
    :integer integer?
    :number  number?
    :keyword keyword?
    :boolean #(instance? Boolean %)))

(defn- type-compatible?
  "Check whether value v belongs to type t."
  [t v]
  ((type-predicate t) v))

;; ── Composite domain constructors ───────────────────────────────

(defn vector-of-dom
  "Domain of vectors whose every element belongs to element-dom."
  [element-dom]
  {:kind :vector-of, :element element-dom})

(defn tuple-dom
  "Domain of vectors with exactly (count element-doms) elements,
   each matching the corresponding domain."
  [element-doms]
  {:kind :tuple, :elements (vec element-doms)})

(defn map-of-dom
  "Domain of maps whose keys belong to key-dom and values to val-dom."
  [key-dom val-dom]
  {:kind :map-of, :key key-dom, :value val-dom})

;; ── Structural domain constructors ──────────────────────────────

(defn map-fields-dom
  "Domain of maps with specific named fields, each constrained to a domain.
   fields is a map of {keyword → domain}.
   Semantics: 'at least these fields' — fewer constraints = more values accepted."
  [fields]
  {:kind :map-fields, :fields fields})

(defn intersection-dom
  "Domain that is the intersection of N sub-domains.
   Flattens nested intersections for canonical form.
   With 0 domains → any, 1 domain → unwrap."
  [domains]
  (let [flat (mapcat (fn [d]
                       (if (= :intersection (:kind d))
                         (:domains d)
                         [d]))
                     domains)]
    (case (count flat)
      0 any
      1 (first flat)
      {:kind :intersection, :domains (vec flat)})))

;; ── Spiral domain (for unbounded integer enumeration) ───────

(defn spiral-dom
  "Domain that enumerates all integers in spiral order: 0, 1, -1, 2, -2, ...
   Internal domain kind — produced by stepping an unbounded range."
  ([] {:kind :spiral, :n 0, :sign :pos})
  ([n sign] {:kind :spiral, :n n, :sign sign}))

;; ── Range domain ────────────────────────────────────────────

(defn range-dom
  "Domain of integers from lo to hi (inclusive). nil bounds mean unbounded.
   Normalizes: lo > hi → void, lo = hi → single."
  [lo hi]
  (cond
    (and lo hi (> lo hi)) void
    (and lo hi (= lo hi)) (single lo)
    :else {:kind :range, :lo lo, :hi hi}))

(defn range?
  "Is this a :range domain?"
  [d]
  (= :range (:kind d)))

(defn int-range
  "Domain containing integers from lo to hi (inclusive).
   Now produces a :range domain instead of materializing."
  [lo hi]
  (range-dom lo hi))

;; ════════════════════════════════════════════════════════════════
;; Queries
;; ════════════════════════════════════════════════════════════════

(defn void? [d]
  (= :void (:kind d)))

(defn any? [d]
  (= :any (:kind d)))

(defn type? [d]
  (= :type (:kind d)))

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
  "Returns the set of values in a finite domain, or nil for open domains.
   Bounded ranges materialize to a set; unbounded ranges return nil."
  [d]
  (case (:kind d)
    :void #{}
    :single #{(:value d)}
    :finite (:values d)
    :range (let [{:keys [lo hi]} d]
             (when (and lo hi)
               (set (clojure.core/range lo (inc hi)))))
    :spiral nil  ;; infinite — not enumerable
    :any nil
    :type nil  ;; open types are not enumerable
    (:vector-of :tuple :map-of :map-fields :intersection) nil  ;; composite/structural domains are non-enumerable
    nil))

(defn finite?
  "Is this domain finite (enumerable)?"
  [d]
  (case (:kind d)
    :range (and (:lo d) (:hi d))  ;; bounded ranges are finite
    :spiral false
    (boolean (members d))))

(defn size
  "Number of members, or nil for open domains."
  [d]
  (case (:kind d)
    :range (let [{:keys [lo hi]} d]
             (when (and lo hi)
               (inc (- hi lo))))
    :spiral nil
    (some-> (members d) count)))

(defn materializable?
  "Can this domain be cheaply materialized to a set of members?
   Returns true for finite, single, void, and bounded ranges.
   Returns false for unbounded ranges, types, and composite domains."
  [d]
  (case (:kind d)
    (:void :single :finite) true
    :range (boolean (and (:lo d) (:hi d)))
    false))

(defn contains-val?
  "Does domain d contain value v?"
  [d v]
  (case (:kind d)
    :void false
    :any true
    :single (= (:value d) v)
    :finite (contains? (:values d) v)
    :type (type-compatible? (:type d) v)
    :range (and (integer? v)
                (or (nil? (:lo d)) (>= v (:lo d)))
                (or (nil? (:hi d)) (<= v (:hi d))))
    :spiral (integer? v)  ;; spiral represents all integers
    :vector-of (and (vector? v)
                    (every? #(contains-val? (:element d) %) v))
    :tuple (and (vector? v)
                (= (count v) (count (:elements d)))
                (every? true? (map #(contains-val? %1 %2) (:elements d) v)))
    :map-of (and (map? v)
                 (every? (fn [[k val]]
                           (and (contains-val? (:key d) k)
                                (contains-val? (:value d) val)))
                         v))
    :map-fields (and (map? v)
                     (every? (fn [[field-key field-dom]]
                               (and (contains? v field-key)
                                    (contains-val? field-dom (get v field-key))))
                             (:fields d)))
    :intersection (every? #(contains-val? % v) (:domains d))
    false))

;; ════════════════════════════════════════════════════════════════
;; Relate — the core primitive
;; ════════════════════════════════════════════════════════════════
;;
;; (relate d1 d2) returns the set-theoretic relation of d1 to d2:
;;   :equal    — same set of values
;;   :subset   — d1 ⊂ d2 (d1 is strictly smaller)
;;   :superset — d1 ⊃ d2 (d1 is strictly bigger)
;;   :disjoint — no overlap
;;   :overlap  — partial overlap, neither contains the other
;; ════════════════════════════════════════════════════════════════

(defn- flip
  "Flip a relation (swap perspective)."
  [rel]
  (case rel
    :equal    :equal
    :subset   :superset
    :superset :subset
    :disjoint :disjoint
    :overlap  :overlap
    nil       nil))

;; ── Per-kind relate functions ──────────────────────────────────
;; Each takes [this-domain other-domain] and returns a relation
;; or nil if it doesn't know how to compare with that kind.

(defn- single-relate
  "Relate a :single domain to another domain."
  [d1 d2]
  (if (contains-val? d2 (:value d1))
    ;; d1's value is in d2
    (if (and (singleton? d2) (= (:value d1) (:value d2)))
      :equal
      :subset)
    :disjoint))

(defn- finite-relate
  "Relate a :finite domain to another domain."
  [d1 d2]
  (let [s1 (:values d1)]
    (cond
      ;; Finite vs range — use contains-val? instead of materializing
      (range? d2)
      (let [in-range (filter #(contains-val? d2 %) s1)
            n-in (count in-range)]
        (cond
          (zero? n-in) :disjoint
          (= n-in (count s1))
          ;; All finite values are in range. Check if range equals the finite set.
          (let [{:keys [lo hi]} d2]
            (if (and lo hi (= (count s1) (inc (- hi lo))))
              :equal
              :subset))
          :else :overlap))

      ;; Both finite — classic set comparison
      (members d2)
      (let [s2 (members d2)
            inter (clojure.set/intersection s1 s2)]
        (cond
          (= s1 s2)              :equal
          (empty? inter)         :disjoint
          (= inter s1)           :subset    ;; all of d1 in d2
          (= inter s2)           :superset  ;; all of d2 in d1
          :else                  :overlap))

      ;; Finite vs type
      (type? d2)
      (let [pred (type-predicate (:type d2))
            matching (count (filter pred s1))]
        (cond
          (= matching (count s1)) :subset    ;; all match
          (zero? matching)        :disjoint
          :else                   :overlap))

      ;; Finite vs unknown open → can't determine
      :else nil)))

(defn- type-relate
  "Relate a :type domain to another domain."
  [d1 d2]
  (cond
    ;; Type vs type
    (type? d2)
    (let [t1 (:type d1) t2 (:type d2)]
      (cond
        (= t1 t2)                                         :equal
        (and (= t1 :integer) (= t2 :number))              :subset
        (and (= t1 :number)  (= t2 :integer))             :superset
        :else                                              :disjoint))

    ;; Type vs finite/single — delegate to flip (finite-relate or single-relate will handle)
    :else nil))

;; ── Composite relate functions ──────────────────────────────────
;; Forward declaration — these call `relate` which hasn't been defined yet
;; at this point in the file. We use declare + defn- pattern.

(declare relate intersect unite)

(defn- composite-kind?
  "Is this a composite domain kind?"
  [k]
  (#{:vector-of :tuple :map-of :map-fields :intersection} k))

(defn- combine-relations
  "Combine multiple positional relations into a product relation.
   Used for tuples (per-position) and map-of (key × value).
   Rules:
     - any :disjoint → :disjoint
     - all :equal → :equal
     - all :subset or :equal → :subset
     - all :superset or :equal → :superset
     - otherwise → :overlap"
  [rels]
  (cond
    (every? #{:equal} rels)                    :equal
    (some #{:disjoint} rels)                   :disjoint
    (every? #{:equal :subset} rels)            :subset
    (every? #{:equal :superset} rels)          :superset
    :else                                      :overlap))

(defn- vector-of-relate
  "Relate a :vector-of domain to another domain."
  [d1 d2]
  (cond
    (= :vector-of (:kind d2))
    (relate (:element d1) (:element d2))

    ;; Cross-kind composite or composite vs flat → disjoint
    (composite-kind? (:kind d2)) :disjoint
    :else nil))

(defn- tuple-relate
  "Relate a :tuple domain to another domain."
  [d1 d2]
  (cond
    (= :tuple (:kind d2))
    (let [es1 (:elements d1)
          es2 (:elements d2)]
      (if (not= (count es1) (count es2))
        :disjoint
        (combine-relations (mapv relate es1 es2))))

    ;; Cross-kind composite or composite vs flat → disjoint
    (composite-kind? (:kind d2)) :disjoint
    :else nil))

(defn- map-of-relate
  "Relate a :map-of domain to another domain."
  [d1 d2]
  (cond
    (= :map-of (:kind d2))
    (combine-relations [(relate (:key d1) (:key d2))
                        (relate (:value d1) (:value d2))])

    ;; Cross-kind composite or composite vs flat → disjoint
    (composite-kind? (:kind d2)) :disjoint
    :else nil))

;; ── Structural relate functions ──────────────────────────────────

(defn- map-fields-relate
  "Relate a :map-fields domain to another domain.
   Semantics: fewer field constraints = more values accepted (superset).
   {name: string, age: int} ⊂ {name: string} because the latter accepts more maps."
  [d1 d2]
  (cond
    (= :map-fields (:kind d2))
    (let [f1 (:fields d1)
          f2 (:fields d2)
          k1 (set (keys f1))
          k2 (set (keys f2))
          shared (clojure.set/intersection k1 k2)
          only-in-1 (clojure.set/difference k1 k2)
          only-in-2 (clojure.set/difference k2 k1)]
      ;; Compare shared fields
      (let [field-rels (mapv (fn [k] (relate (get f1 k) (get f2 k))) shared)]
        (cond
          ;; If any shared field is disjoint, the whole thing is disjoint
          (some #{:disjoint} field-rels) :disjoint

          ;; Same keys, compare field by field
          (and (empty? only-in-1) (empty? only-in-2))
          (combine-relations field-rels)

          ;; d1 has extra fields (more constrained → subset if shared fields allow)
          (and (seq only-in-1) (empty? only-in-2))
          (if (every? #{:equal :subset} field-rels) :subset :overlap)

          ;; d2 has extra fields (more constrained → d1 is superset if shared allow)
          (and (empty? only-in-1) (seq only-in-2))
          (if (every? #{:equal :superset} field-rels) :superset :overlap)

          ;; Both have unique fields → overlap
          :else :overlap)))

    ;; map-fields vs map-of: map-of constrains all keys/values, map-fields constrains specific fields
    ;; Generally overlap (map-of is a different structural shape)
    (= :map-of (:kind d2)) :overlap

    ;; Cross-kind composite → disjoint
    (composite-kind? (:kind d2)) :disjoint
    :else nil))

(defn- intersection-relate
  "Relate an :intersection domain to another domain.
   Conservative: compares by checking each component."
  [d1 d2]
  (cond
    ;; intersection vs intersection — conservative overlap
    (= :intersection (:kind d2))
    (if (= (:domains d1) (:domains d2)) :equal :overlap)

    ;; Check if d1 (intersection of components) relates to d2
    ;; d1 is a subset of each component, so if any component is subset/equal to d2, d1 is subset
    :else
    (let [component-rels (mapv #(relate % d2) (:domains d1))]
      (cond
        ;; If all components are superset/equal of d2, intersection is likely superset (conservative: overlap)
        ;; If any component is disjoint with d2, the intersection is disjoint
        (some #{:disjoint} component-rels) :disjoint
        ;; If any component is subset/equal to d2, intersection is also subset
        (some #{:subset :equal} component-rels) :subset
        :else :overlap))))

;; ── Range relate ────────────────────────────────────────────────

(defn- range-lo-le
  "Is lo1 <= lo2? nil means -∞."
  [lo1 lo2]
  (cond
    (nil? lo1) true    ;; -∞ <= anything
    (nil? lo2) false   ;; anything > -∞ is not <= -∞
    :else (<= lo1 lo2)))

(defn- range-hi-le
  "Is hi1 <= hi2? nil means +∞."
  [hi1 hi2]
  (cond
    (nil? hi2) true    ;; anything <= +∞
    (nil? hi1) false   ;; +∞ is not <= anything finite
    :else (<= hi1 hi2)))

(defn- range-relate
  "Relate a :range domain to another domain."
  [d1 d2]
  (cond
    ;; Range vs range
    (range? d2)
    (let [{lo1 :lo hi1 :hi} d1
          {lo2 :lo hi2 :hi} d2]
      ;; Check disjoint first
      (if (or (and lo1 hi2 (> lo1 hi2))
              (and lo2 hi1 (> lo2 hi1)))
        :disjoint
        ;; Check containment
        (let [d1-in-d2 (and (range-lo-le lo2 lo1) (range-hi-le hi1 hi2))
              d2-in-d1 (and (range-lo-le lo1 lo2) (range-hi-le hi2 hi1))]
          (cond
            (and d1-in-d2 d2-in-d1) :equal
            d1-in-d2                :subset
            d2-in-d1                :superset
            :else                   :overlap))))

    ;; Range vs type
    (type? d2)
    (case (:type d2)
      :integer (if (and (nil? (:lo d1)) (nil? (:hi d1)))
                 :equal    ;; unbounded range = all integers
                 :subset)  ;; bounded range ⊂ integers
      :number  :subset    ;; all integers ⊂ numbers
      :disjoint)           ;; range (integers) vs string/keyword/boolean

    ;; Range vs finite
    (= :finite (:kind d2))
    (let [{:keys [lo hi]} d1
          vals (:values d2)
          in-range (filter #(and (integer? %)
                                 (or (nil? lo) (>= % lo))
                                 (or (nil? hi) (<= % hi)))
                           vals)]
      (cond
        (empty? in-range) :disjoint
        ;; All finite vals are in range — range could be superset
        (= (count in-range) (count vals))
        (if (and lo hi (= (count vals) (inc (- hi lo))))
          :equal      ;; finite set exactly matches range
          :superset)
        :else :overlap))

    ;; Range vs single
    (singleton? d2)
    (if (contains-val? d1 (:value d2))
      (if (and (:lo d1) (:hi d1) (= (:lo d1) (:hi d1)))
        :equal      ;; won't happen (normalized to single), but safe
        :superset)
      :disjoint)

    ;; Range vs composite → disjoint
    (composite-kind? (:kind d2)) :disjoint

    :else nil))

;; ── Spiral relate ───────────────────────────────────────────────

(defn- spiral-relate
  "Relate a :spiral domain to another domain.
   Spiral represents all integers, so it relates like unbounded range."
  [d1 d2]
  (cond
    ;; Spiral vs spiral — equal (both represent all integers)
    (= :spiral (:kind d2)) :equal

    ;; Spiral vs range — spiral is all integers
    (range? d2)
    (let [{:keys [lo hi]} d2]
      (if (and (nil? lo) (nil? hi))
        :equal      ;; unbounded range = all integers = spiral
        :superset)) ;; bounded range ⊂ all integers

    ;; Spiral vs type
    (type? d2)
    (case (:type d2)
      :integer :equal     ;; spiral = all integers
      :number  :subset    ;; integers ⊂ numbers
      :disjoint)

    ;; Spiral vs finite/single — superset if all are integers
    (or (= :finite (:kind d2)) (singleton? d2))
    (let [vals (if (singleton? d2) #{(:value d2)} (:values d2))]
      (if (every? integer? vals) :superset :overlap))

    ;; Spiral vs composite → disjoint
    (composite-kind? (:kind d2)) :disjoint

    :else nil))

;; ── Dispatch table ─────────────────────────────────────────────

(def ^:private kind-relate
  "Dispatch table: :kind → relate function."
  {:single       single-relate
   :finite       finite-relate
   :type         type-relate
   :range        range-relate
   :spiral       spiral-relate
   :vector-of    vector-of-relate
   :tuple        tuple-relate
   :map-of       map-of-relate
   :map-fields   map-fields-relate
   :intersection intersection-relate})

(defn relate
  "Returns the set-theoretic relation of d1 to d2.
   One of: :equal, :subset, :superset, :disjoint, :overlap."
  [d1 d2]
  (cond
    (= d1 d2)  :equal
    (void? d1)  (if (void? d2) :equal :subset)
    (void? d2)  :superset
    (any? d1)   (if (any? d2) :equal :superset)
    (any? d2)   :subset
    :else
    (let [k1 (:kind d1)
          r1-fn (get kind-relate k1)
          r1 (when r1-fn (r1-fn d1 d2))]
      (or r1
          ;; d1 doesn't know — ask d2 and flip
          (let [k2 (:kind d2)
                r2-fn (get kind-relate k2)
                r2 (when r2-fn (r2-fn d2 d1))]
            (or (flip r2)
                ;; Neither knows — default to disjoint
                :disjoint))))))

;; ════════════════════════════════════════════════════════════════
;; Algebra — derived from relate
;; ════════════════════════════════════════════════════════════════

;; ── Overlap helpers (kind-specific narrowing) ──────────────────

(defn- range-intersect
  "Intersect two range domains. Returns a (possibly normalized) domain."
  [r1 r2]
  (let [lo (if (and (:lo r1) (:lo r2)) (max (:lo r1) (:lo r2))
               (or (:lo r1) (:lo r2)))
        hi (if (and (:hi r1) (:hi r2)) (min (:hi r1) (:hi r2))
               (or (:hi r1) (:hi r2)))]
    (range-dom lo hi)))

(defn- range-filter-finite
  "Filter a finite domain's values to those within a range's bounds."
  [{:keys [lo hi]} fin-d]
  (let [vals (if (= :finite (:kind fin-d)) (:values fin-d) (members fin-d))]
    (finite (set (filter #(and (integer? %)
                               (or (nil? lo) (>= % lo))
                               (or (nil? hi) (<= % hi)))
                         vals)))))

(defn- overlap-intersect
  "Intersect two domains that :overlap. Kind-specific."
  [d1 d2]
  (cond
    ;; range ∩ range
    (and (range? d1) (range? d2))
    (range-intersect d1 d2)

    ;; range ∩ finite (or finite ∩ range)
    (and (range? d1) (or (= :finite (:kind d2)) (singleton? d2)))
    (range-filter-finite d1 d2)

    (and (or (= :finite (:kind d1)) (singleton? d1)) (range? d2))
    (range-filter-finite d2 d1)

    ;; range ∩ type → range (range is already integers, subset of number/integer)
    (and (range? d1) (type? d2))
    (if (#{:integer :number} (:type d2)) d1 void)

    (and (type? d1) (range? d2))
    (if (#{:integer :number} (:type d1)) d2 void)

    ;; vector-of ∩ vector-of
    (and (= :vector-of (:kind d1)) (= :vector-of (:kind d2)))
    (vector-of-dom (intersect (:element d1) (:element d2)))

    ;; tuple ∩ tuple (same length — guaranteed by relate returning :overlap)
    (and (= :tuple (:kind d1)) (= :tuple (:kind d2)))
    (tuple-dom (mapv intersect (:elements d1) (:elements d2)))

    ;; map-of ∩ map-of
    (and (= :map-of (:kind d1)) (= :map-of (:kind d2)))
    (map-of-dom (intersect (:key d1) (:key d2))
                (intersect (:value d1) (:value d2)))

    ;; map-fields ∩ map-fields — merge fields, intersect overlapping
    (and (= :map-fields (:kind d1)) (= :map-fields (:kind d2)))
    (let [f1 (:fields d1)
          f2 (:fields d2)
          all-keys (clojure.set/union (set (keys f1)) (set (keys f2)))
          merged (into {} (map (fn [k]
                                 (let [v1 (get f1 k)
                                       v2 (get f2 k)]
                                   [k (cond
                                        (and v1 v2) (intersect v1 v2)
                                        v1 v1
                                        :else v2)]))
                               all-keys))]
      (map-fields-dom merged))

    ;; map-fields ∩ map-of (or vice versa) — use intersection domain
    (and (= :map-fields (:kind d1)) (= :map-of (:kind d2)))
    (intersection-dom [d1 d2])

    (and (= :map-of (:kind d1)) (= :map-fields (:kind d2)))
    (intersection-dom [d1 d2])

    ;; intersection ∩ anything — flatten into intersection
    (= :intersection (:kind d1))
    (intersection-dom (conj (:domains d1) d2))

    (= :intersection (:kind d2))
    (intersection-dom (conj (:domains d2) d1))

    ;; finite ∩ finite
    (and (members d1) (members d2))
    (finite (clojure.set/intersection (members d1) (members d2)))

    ;; type ∩ finite (or finite ∩ type) — filter by type predicate
    (and (type? d1) (members d2))
    (finite (set (filter (type-predicate (:type d1)) (members d2))))

    (and (members d1) (type? d2))
    (finite (set (filter (type-predicate (:type d2)) (members d1))))

    ;; Fallback for unknown overlap — conservative: return d1
    :else d1))

(defn- disjoint-unite
  "Unite two disjoint domains. Kind-specific or → any."
  [d1 d2]
  (cond
    ;; range ∪ range (disjoint)
    (and (range? d1) (range? d2))
    (let [{lo1 :lo hi1 :hi} d1
          {lo2 :lo hi2 :hi} d2]
      (if (or (and hi1 lo2 (= (inc hi1) lo2))
              (and hi2 lo1 (= (inc hi2) lo1)))
        ;; Contiguous — merge into one range
        (range-dom (if (and lo1 lo2) (min lo1 lo2) nil)
                   (if (and hi1 hi2) (max hi1 hi2) nil))
        ;; Gap — materialize to finite set union if both bounded
        (if (and lo1 hi1 lo2 hi2)
          (finite (clojure.set/union
                   (set (clojure.core/range lo1 (inc hi1)))
                   (set (clojure.core/range lo2 (inc hi2)))))
          ;; Unbounded — approximate with widened range
          (range-dom (if (and lo1 lo2) (min lo1 lo2) nil)
                     (if (and hi1 hi2) (max hi1 hi2) nil)))))

    ;; range ∪ finite (or finite ∪ range)
    (and (range? d1) (materializable? d2))
    (if (and (:lo d1) (:hi d1))
      (finite (clojure.set/union (members d1) (members d2)))
      any)

    (and (materializable? d1) (range? d2))
    (if (and (:lo d2) (:hi d2))
      (finite (clojure.set/union (members d1) (members d2)))
      any)

    ;; Both finite — set union
    (and (members d1) (members d2))
    (finite (clojure.set/union (members d1) (members d2)))

    :else any))

(defn- overlap-unite
  "Unite two overlapping domains. Kind-specific or → any."
  [d1 d2]
  (cond
    ;; range ∪ range (overlapping → merge to encompassing range)
    (and (range? d1) (range? d2))
    (range-dom (cond
                 (and (:lo d1) (:lo d2)) (min (:lo d1) (:lo d2))
                 :else nil)
               (cond
                 (and (:hi d1) (:hi d2)) (max (:hi d1) (:hi d2))
                 :else nil))

    ;; vector-of ∪ vector-of
    (and (= :vector-of (:kind d1)) (= :vector-of (:kind d2)))
    (vector-of-dom (unite (:element d1) (:element d2)))

    ;; tuple ∪ tuple (same length)
    (and (= :tuple (:kind d1)) (= :tuple (:kind d2)))
    (tuple-dom (mapv unite (:elements d1) (:elements d2)))

    ;; map-of ∪ map-of
    (and (= :map-of (:kind d1)) (= :map-of (:kind d2)))
    (map-of-dom (unite (:key d1) (:key d2))
                (unite (:value d1) (:value d2)))

    ;; map-fields ∪ map-fields — keep only shared fields, unite their domains
    (and (= :map-fields (:kind d1)) (= :map-fields (:kind d2)))
    (let [f1 (:fields d1)
          f2 (:fields d2)
          shared-keys (clojure.set/intersection (set (keys f1)) (set (keys f2)))]
      (if (empty? shared-keys)
        any
        (map-fields-dom (into {} (map (fn [k]
                                        [k (unite (get f1 k) (get f2 k))])
                                      shared-keys)))))

    ;; Both finite — set union
    (and (members d1) (members d2))
    (finite (clojure.set/union (members d1) (members d2)))

    :else any))

(defn- subset-unite
  "Unite d1 (subset) with d2 (superset). Result is d2."
  [_d1 d2]
  d2)

(defn- superset-subtract
  "Subtract d2 (subset) from d1 (superset). Kind-specific."
  [d1 d2]
  (cond
    ;; range - range (d2 ⊂ d1): trim from edges
    (and (range? d1) (range? d2))
    (let [{lo1 :lo hi1 :hi} d1
          {lo2 :lo hi2 :hi} d2]
      ;; If d2 shares the same lo, trim from below
      (cond
        (and (= lo1 lo2) hi2) (range-dom (inc hi2) hi1)
        (and (= hi1 hi2) lo2) (range-dom lo1 (dec lo2))
        ;; d2 sits in the middle — can't represent precisely, keep d1
        :else d1))

    ;; range - single
    (and (range? d1) (singleton? d2))
    (let [{:keys [lo hi]} d1
          v (:value d2)]
      (cond
        (= v lo) (range-dom (inc lo) hi)
        (= v hi) (range-dom lo (dec hi))
        ;; middle — materialize to finite set and remove
        (and lo hi) (finite (disj (set (clojure.core/range lo (inc hi))) v))
        :else d1))

    ;; range - finite → approximate with d1
    (and (range? d1) (= :finite (:kind d2)))
    d1

    ;; finite - finite
    (and (members d1) (members d2))
    (finite (clojure.set/difference (members d1) (members d2)))

    ;; type - finite → type (can't enumerate remainder of an infinite set)
    (and (type? d1) (members d2))
    d1

    ;; type - range → type (can't represent "integer minus range" precisely)
    (and (type? d1) (range? d2))
    d1

    ;; type - type (superset means d1 ⊃ d2, e.g. number - integer)
    ;; We can't represent "non-integer numbers" precisely, return d1
    (and (type? d1) (type? d2))
    d1

    ;; Composite domains — can't precisely represent the remainder, approximate with d1
    :else d1))

(defn- overlap-subtract
  "Subtract two overlapping domains. Kind-specific."
  [d1 d2]
  (cond
    ;; range - range (overlap): trim bounds
    (and (range? d1) (range? d2))
    (let [{lo1 :lo hi1 :hi} d1
          {lo2 :lo hi2 :hi} d2]
      (cond
        ;; d2 covers from below: trim d1's bottom
        (and (or (nil? lo2) (and lo1 (<= lo2 lo1))) hi2)
        (range-dom (inc hi2) hi1)
        ;; d2 covers from above: trim d1's top
        (and (or (nil? hi2) (and hi1 (>= hi2 hi1))) lo2)
        (range-dom lo1 (dec lo2))
        ;; d2 sits in the middle — can't punch a hole, approximate
        :else d1))

    ;; range - finite → approximate
    (and (range? d1) (= :finite (:kind d2)))
    d1

    ;; finite - range → filter
    (and (= :finite (:kind d1)) (range? d2))
    (finite (set (remove #(contains-val? d2 %) (:values d1))))

    ;; finite - finite
    (and (members d1) (members d2))
    (finite (clojure.set/difference (members d1) (members d2)))

    ;; finite - type → filter out matching
    (and (members d1) (type? d2))
    (finite (set (remove (type-predicate (:type d2)) (members d1))))

    ;; type - finite → type (approximation)
    (and (type? d1) (members d2))
    d1

    ;; type - range → type (approximation)
    (and (type? d1) (range? d2))
    d1

    ;; Composite overlap subtract — can't precisely represent, approximate with d1
    :else d1))

(defn intersect
  "Narrowing — values that are in both d1 and d2."
  [d1 d2]
  (case (relate d1 d2)
    :equal    d1
    :subset   d1
    :superset d2
    :disjoint void
    :overlap  (overlap-intersect d1 d2)))

(defn unite
  "Widening — values that are in either d1 or d2."
  [d1 d2]
  (case (relate d1 d2)
    :equal    d1
    :subset   d2
    :superset d1
    :disjoint (disjoint-unite d1 d2)
    :overlap  (overlap-unite d1 d2)))

(defn subtract
  "Values in d1 that are not in d2."
  [d1 d2]
  (case (relate d1 d2)
    :equal    void
    :subset   void
    :superset (superset-subtract d1 d2)
    :disjoint d1
    :overlap  (overlap-subtract d1 d2)))

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
  (if (range? d)
    (:lo d)  ;; nil if unbounded below
    (first (comparable-members d))))

(defn domain-max
  "Largest value in domain, or nil."
  [d]
  (if (range? d)
    (:hi d)  ;; nil if unbounded above
    (last (comparable-members d))))

(defn domain-below
  "Values in d that are strictly less than v."
  [d v]
  (cond
    (range? d)
    (let [{:keys [lo hi]} d
          new-hi (dec (long v))]
      (range-dom lo (if hi (min hi new-hi) new-hi)))

    :else
    (if-let [ms (members d)]
      (finite (set (filter #(and (number? %) (< % v)) ms)))
      d)))

(defn domain-above
  "Values in d that are strictly greater than v."
  [d v]
  (cond
    (range? d)
    (let [{:keys [lo hi]} d
          new-lo (inc (long v))]
      (range-dom (if lo (max lo new-lo) new-lo) hi))

    :else
    (if-let [ms (members d)]
      (finite (set (filter #(and (number? %) (> % v)) ms)))
      d)))

(defn domain-at-most
  "Values in d that are <= v."
  [d v]
  (cond
    (range? d)
    (let [{:keys [lo hi]} d
          new-hi (long v)]
      (range-dom lo (if hi (min hi new-hi) new-hi)))

    :else
    (if-let [ms (members d)]
      (finite (set (filter #(and (number? %) (<= % v)) ms)))
      d)))

(defn domain-at-least
  "Values in d that are >= v."
  [d v]
  (cond
    (range? d)
    (let [{:keys [lo hi]} d
          new-lo (long v)]
      (range-dom (if lo (max lo new-lo) new-lo) hi))

    :else
    (if-let [ms (members d)]
      (finite (set (filter #(and (number? %) (>= % v)) ms)))
      d)))

(defn domain-filter
  "Filter domain by predicate."
  [d pred]
  (if-let [ms (members d)]
    (finite (set (filter pred ms)))
    d))

;; ════════════════════════════════════════════════════════════════
;; Step — lazy enumeration primitive
;; ════════════════════════════════════════════════════════════════
;;
;; (step d) → [value remainder-domain] or nil
;;
;; A domain that can produce a value returns [v d'] where v is one
;; element and d' is the domain with v removed. When exhausted,
;; returns nil. The `kind-step` dispatch table parallels `kind-relate`.

(defn- single-step
  "Step a :single domain. Returns [value void]."
  [{:keys [value]}]
  [value void])

(defn- finite-step
  "Step a :finite domain. Picks smallest value (deterministic order)."
  [{:keys [values]}]
  (let [sorted (sort values)
        v (first sorted)]
    [v (finite (disj values v))]))

(defn- range-step
  "Step a :range domain. Walks from lo upward (or hi downward)."
  [{:keys [lo hi]}]
  (cond
    ;; Bounded: walk from lo to hi
    (and lo hi)
    [lo (range-dom (inc lo) hi)]

    ;; Half-infinite upward: lo, lo+1, lo+2, ...
    (and lo (nil? hi))
    [lo (range-dom (inc lo) nil)]

    ;; Half-infinite downward: hi, hi-1, hi-2, ...
    (and (nil? lo) hi)
    [hi (range-dom nil (dec hi))]

    ;; Fully unbounded: convert to spiral
    :else
    (let [spiral (spiral-dom)]
      ;; step the spiral (which starts at 0)
      [0 (spiral-dom 1 :pos)])))

(defn- spiral-step
  "Step a :spiral domain. Emits 0, 1, -1, 2, -2, 3, -3, ..."
  [{:keys [n sign]}]
  (cond
    ;; Starting point: n=0 → emit 0
    (zero? n)
    [0 (spiral-dom 1 :pos)]

    ;; Positive phase: emit n, then switch to negative
    (= sign :pos)
    [n (spiral-dom n :neg)]

    ;; Negative phase: emit -n, then advance to n+1 positive
    (= sign :neg)
    [(- n) (spiral-dom (inc n) :pos)]))

(defn- boolean-type-step
  "Step a :type :boolean domain → [true, (single false)]."
  [_d]
  [true (single false)])

(def ^:private kind-step
  "Dispatch table: :kind → step function.
   Each returns [value remainder-domain] or nil."
  {:single  single-step
   :finite  finite-step
   :range   range-step
   :spiral  spiral-step})

(defn step
  "Produce one value from a domain, returning [value remainder-domain] or nil.
   The remainder domain represents all values not yet produced."
  [d]
  (cond
    (void? d) nil
    (any? d) nil

    ;; Special case: boolean type
    (and (type? d) (= :boolean (:type d)))
    (boolean-type-step d)

    :else
    (when-let [step-fn (get kind-step (:kind d))]
      (step-fn d))))

(defn steppable?
  "Can this domain produce values via `step`?
   Returns true for single, finite, range, spiral, and boolean type.
   Returns false for void, any, non-boolean types, and composite domains."
  [d]
  (and (not (void? d))
       (not (any? d))
       (or (and (type? d) (= :boolean (:type d)))
           (#{:single :finite :range :spiral} (:kind d)))))

;; ════════════════════════════════════════════════════════════════
;; Domain splitting (for binary search)
;; ════════════════════════════════════════════════════════════════

(defn split
  "Bisect domain d into [left right] where left ∪ right = d, both non-empty.
   Returns nil if d is void or singleton (can't split further).
   Used by the search engine for binary tree splitting."
  [d]
  (case (:kind d)
    :void nil
    :single nil  ;; already ground

    :finite
    (let [;; Sort with type-safe comparator: group by type name first,
          ;; then by natural order within each type. This prevents
          ;; ClassCastException when finite domains contain mixed types
          ;; (e.g., #{1 "Alice"} from (one-of 1 "Alice")).
          sorted (sort (fn [a b]
                         (let [ta (type a) tb (type b)]
                           (if (= ta tb)
                             (compare a b)
                             (compare (.getName ^Class ta) (.getName ^Class tb)))))
                       (:values d))
          n      (count sorted)
          mid    (quot n 2)
          left   (set (take mid sorted))
          right  (set (drop mid sorted))]
      [(finite left) (finite right)])

    :range
    (let [{:keys [lo hi]} d]
      (cond
        ;; Bounded: split at midpoint
        ;; Use lo + quot(hi-lo, 2) to avoid negative overflow with quot
        (and lo hi)
        (let [mid (+ lo (quot (- hi lo) 2))]
          [(range-dom lo mid) (range-dom (inc mid) hi)])

        ;; lo-bounded only: pick a split point
        lo
        [(range-dom lo (+ lo 100)) (range-dom (+ lo 101) nil)]

        ;; hi-bounded only: pick a split point
        hi
        [(range-dom (- hi 100) hi) (range-dom nil (dec (- hi 100)))]

        ;; Fully unbounded: split around zero
        :else
        [(range-dom nil 0) (range-dom 1 nil)]))

    ;; Spiral: treat like unbounded integers
    :spiral [(range-dom nil 0) (range-dom 1 nil)]

    ;; Boolean type: split into the two values
    :type (when (= :boolean (:type d))
            [(single true) (single false)])

    ;; Anything else (composite, any, etc.) — not splittable here
    nil))

;; ════════════════════════════════════════════════════════════════
;; Arithmetic on domains
;; ════════════════════════════════════════════════════════════════

;; ── Range-aware arithmetic helpers ──────────────────────────────

(defn- safe-add [a b] (when (and a b) (+ a b)))
(defn- safe-sub [a b] (when (and a b) (- a b)))

(defn- range-bounds
  "Extract [lo hi] from a domain for arithmetic. Returns [lo hi] where
   nil means unbounded. Works for range, single, and finite."
  [d]
  (case (:kind d)
    :range [(:lo d) (:hi d)]
    :single [(:value d) (:value d)]
    :finite (let [ms (:values d)]
              [(apply min ms) (apply max ms)])
    [nil nil]))

(defn domain-add
  "All pairwise sums of values from d1 and d2."
  [d1 d2]
  (cond
    ;; At least one is a range — use bounds arithmetic
    (or (range? d1) (range? d2))
    (let [[lo1 hi1] (range-bounds d1)
          [lo2 hi2] (range-bounds d2)]
      (range-dom (safe-add lo1 lo2) (safe-add hi1 hi2)))

    (and (members d1) (members d2))
    (finite (set (for [a (members d1) b (members d2)] (+ a b))))

    :else any))

(defn domain-sub
  "All pairwise differences."
  [d1 d2]
  (cond
    (or (range? d1) (range? d2))
    (let [[lo1 hi1] (range-bounds d1)
          [lo2 hi2] (range-bounds d2)]
      ;; a-b: min = lo1-hi2, max = hi1-lo2
      (range-dom (safe-sub lo1 hi2) (safe-sub hi1 lo2)))

    (and (members d1) (members d2))
    (finite (set (for [a (members d1) b (members d2)] (- a b))))

    :else any))

(defn domain-mul
  "All pairwise products."
  [d1 d2]
  (cond
    (or (range? d1) (range? d2))
    (let [[lo1 hi1] (range-bounds d1)
          [lo2 hi2] (range-bounds d2)]
      (if (and lo1 hi1 lo2 hi2)
        ;; All bounds known — take min/max of all corner products
        (let [corners [(* lo1 lo2) (* lo1 hi2) (* hi1 lo2) (* hi1 hi2)]]
          (range-dom (apply min corners) (apply max corners)))
        ;; Some bound unknown — result is unbounded
        (range-dom nil nil)))

    (and (members d1) (members d2))
    (finite (set (for [a (members d1) b (members d2)] (* a b))))

    :else any))

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
