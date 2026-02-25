# Design: Lazy Enumeration & Infinite Domains

## The Idea

mufl's domains are sets — sometimes finite, sometimes infinite. Today the solver only enumerates finite domains (`members` returns a set, we iterate it). Infinite domains (`type`, `vector-of`, `tuple`, `map-of`) are opaque to the solver — it can propagate constraints through them but can't produce solutions from them.

But infinite domains *are* iterable. The key insight: **a domain can step** — produce one value and the remainder domain.

## The `step` primitive

```clojure
(step d) → [value remainder-domain] or nil
```

A domain that can produce a value returns `[v d']` where `v` is one element and `d'` is the domain with `v` removed (the "rest"). When exhausted, returns `nil`.

```clojure
(step (single 42))
;=> [42 {:kind :void}]

(step (finite #{1 2 3}))
;=> [1 {:kind :finite, :values #{2 3}}]

(step {:kind :range, :lo 1, :hi 5})
;=> [1 {:kind :range, :lo 2, :hi 5}]

(step {:kind :range, :lo 5, :hi 5})
;=> [5 {:kind :void}]

(step {:kind :range, :lo 1, :hi nil})
;=> [1 {:kind :range, :lo 2, :hi nil}]   ;; never exhausts

(step void)
;=> nil

(step integer-dom)
;=> [0 ???]   ;; what's "integer minus 0"?
```

### Why step works

**Domains stay pure data.** `step` is a function in the dispatch table (`kind-step`), parallel to `kind-relate`. The domain map itself contains no functions. The *result* of stepping is a value + another plain data domain.

**It unifies finite and infinite.** Every domain kind that can produce values implements `step`. The solver doesn't need to know whether a domain is finite or infinite — it just steps.

**It *is* the search tree.** Each `step` call is a branch point. The value is the choice made. The remainder domain is the unexplored subtree. Backtracking = stepping the remainder.

### Dispatch table

```clojure
(def kind-step
  {:single   single-step
   :finite   finite-step
   :range    range-step
   :type     type-step
   :vector-of vector-of-step    ;; deferred
   :tuple     tuple-step        ;; deferred
   :map-of    map-of-step})     ;; deferred
```

### Stepping strategies per kind

**`:single`** — trivial:
```clojure
(defn- single-step [{:keys [value]}]
  [value void])
```

**`:finite`** — pick smallest (or any deterministic order), return rest:
```clojure
(defn- finite-step [{:keys [values]}]
  (let [sorted (sort values)
        v (first sorted)]
    [v (finite (disj values v))]))
```

**`:range`** — walk from lo to hi:
```clojure
(defn- range-step [{:keys [lo hi]}]
  (cond
    ;; Bounded
    (and lo hi)
    (if (> lo hi) nil
        [lo (range-dom (inc lo) hi)])   ;; normalizes to void if lo+1 > hi

    ;; Half-infinite upward: 1, 2, 3, ...
    (and lo (nil? hi))
    [lo (range-dom (inc lo) nil)]

    ;; Half-infinite downward: -1, -2, -3, ...
    (and (nil? lo) hi)
    [hi (range-dom nil (dec hi))]

    ;; Fully infinite: 0, 1, -1, 2, -2, 3, -3, ...
    ;; → needs a spiral strategy (see below)
    :else ...))
```

**`:type`** — delegates to a kind-specific enumeration:
```clojure
(defn- type-step [{:keys [type] :as d}]
  (case type
    :boolean [true (single false)]   ;; then false, then void
    :integer (step (range-dom nil nil))  ;; delegate to infinite range spiral
    :keyword ...
    :string  ...
    :number  (step (range-dom nil nil))))  ;; start with integers
```

### The spiral for unbounded integers

An unbounded range `{:lo nil, :hi nil}` needs to produce all integers fairly: `0, 1, -1, 2, -2, 3, -3, ...`

This requires a domain representation that tracks the spiral state. Options:

**Option A**: A special `:spiral` kind that tracks position:
```clojure
{:kind :spiral, :n 0, :sign :pos}
;; step → [0, {:kind :spiral, :n 1, :sign :pos}]
;; step → [1, {:kind :spiral, :n 1, :sign :neg}]
;; step → [-1, {:kind :spiral, :n 2, :sign :pos}]
;; ...
```

**Option B**: Represent as two half-infinite ranges interleaved. The step function for an unbounded range returns `[0, <interleave-state>]` where the interleave state alternates between stepping `[1..∞]` and `[-1..-∞]`.

**Option C**: Convert to an explicit `{:kind :range, :lo 0, :hi nil}` on first step (producing 0), then interleave positive and negative. This could be a `{:kind :interleave, :streams [range-up range-down]}` domain.

Option A is simplest for now — a spiral is a concrete enumeration order for integers, and it's internal to the implementation. The user never sees it; it's what `step` on an unbounded range produces.

**But note**: the spiral is only needed when the solver reaches an unconstrained integer. If constraint propagation narrows `integer` to `{:kind :range, :lo 1, :hi 100}` first, the spiral is never used. In practice, useful programs will have bounds.

### `steppable?`

The solver's new predicate for "can I enumerate this?":

```clojure
(defn steppable? [d]
  (and (not (void? d))
       (not (any? d))
       (boolean (get kind-step (:kind d)))))
```

This replaces `finite?` in the solver. `void` can't step (no values). `any` can't step (too unconstrained — what would it even produce?). Everything else that has a `kind-step` entry can step.

## Two concerns, cleanly separated

### Bind time: set algebra (unchanged)

`relate`, `intersect`, `unite`, `subtract`, `domain-below`, `domain-above`, narrowing functions — all work on domains as **sets**. This is constraint propagation. It narrows domains.

Ranges participate here via bounds arithmetic:
- `intersect([1..10], [5..15])` → `[5..10]`
- `domain-below([1..100], 50)` → `[1..49]`
- `narrow-lt` on ranges: adjust `:lo`/`:hi` in O(1)
- `narrow-plus` on ranges: `[a.lo+b.lo .. a.hi+b.hi]` — bounds propagation

The `finite?` / `members` guards in existing narrowers stay as-is. Ranges get their own path. The two representations coexist — they're both valid domains, they both participate in `relate`.

### Solve time: stepping (new)

The solver picks a non-ground variable, calls `step` on its domain, tries the value (assign + propagate), recurses. On backtrack, steps the remainder domain.

```
solve(env):
  var = pick-variable(env)        ;; most constrained
  [v, rest-dom] = step(domain-of(var))
  env' = try-assign(env, var, v)  ;; propagate
  if env' ok:
    results from solve(env')      ;; recurse with v assigned
  then:
    results from solve(env with var's domain = rest-dom)  ;; backtrack, try next
```

The solver doesn't care if the domain is finite, range, or type. It just steps.

## The `:range` domain kind

```clojure
{:kind :range, :lo 1, :hi 100}     ;; 1..100 (bounded)
{:kind :range, :lo 1, :hi nil}     ;; 1..∞   (half-infinite up)
{:kind :range, :lo nil, :hi -1}    ;; -∞..-1 (half-infinite down)
{:kind :range, :lo nil, :hi nil}   ;; all integers (unbounded)
```

### Constructor with normalization

```clojure
(defn range-dom [lo hi]
  (cond
    ;; Empty
    (and lo hi (> lo hi)) void

    ;; Single value
    (and lo hi (= lo hi)) (single lo)

    ;; Normal range
    :else {:kind :range, :lo lo, :hi hi}))
```

No materialization. No threshold. `(between 1 1000000)` → `{:kind :range, :lo 1, :hi 1000000}`. O(1).

### `contains-val?`

```clojure
(defn- range-contains? [{:keys [lo hi]} v]
  (and (integer? v)
       (or (nil? lo) (>= v lo))
       (or (nil? hi) (<= v hi))))
```

### `relate` for ranges

Range vs range:
```
[1..10] vs [1..10]   → :equal
[1..5]  vs [1..10]   → :subset
[1..10] vs [1..5]    → :superset
[1..10] vs [20..30]  → :disjoint
[1..10] vs [5..15]   → :overlap
[1..∞]  vs [-∞..∞]   → :subset
[-∞..∞] vs [-∞..∞]   → :equal
```

Range vs type:
```
[1..10]  vs integer  → :subset   (always, ranges are integers)
[-∞..∞]  vs integer  → :equal    (unbounded range = all integers)
[1..10]  vs number   → :subset   (integers ⊂ numbers)
[1..10]  vs string   → :disjoint
```

Range vs finite:
```
[1..10]  vs #{3 5 7}     → :superset (if all members in range)
[1..10]  vs #{3 5 15}    → :overlap
[1..10]  vs #{20 30}     → :disjoint
```

Range vs single:
```
[1..10]  vs (single 5)   → :superset
[1..10]  vs (single 15)  → :disjoint
```

### `intersect` / `unite` / `subtract` for ranges

**Intersect** (two ranges):
```clojure
(defn- range-intersect [r1 r2]
  (let [lo (if (and (:lo r1) (:lo r2)) (max (:lo r1) (:lo r2))
               (or (:lo r1) (:lo r2)))
        hi (if (and (:hi r1) (:hi r2)) (min (:hi r1) (:hi r2))
               (or (:hi r1) (:hi r2)))]
    (range-dom lo hi)))
```

**Intersect** (range ∩ finite): filter the finite set by the range bounds.

**Unite** (two ranges): if contiguous/overlapping, merge to one range. If gap, approximate (return the wider range, losing precision). Or return a union type — but defer that complexity.

**Subtract**: `[1..10] - [5..10]` → `[1..4]`. `[1..10] - [3..7]` → approximate (return `[1..10]`, imprecise but safe — the solver filters during enumeration).

### Range-aware narrowing

The big payoff. Each narrower gets a range branch:

**`narrow-lt`** (a < b):
```clojure
;; range path:
;; a.hi = min(a.hi, b.hi - 1)  if b.hi known
;; b.lo = max(b.lo, a.lo + 1)  if a.lo known
```

**`narrow-plus`** (a + b = c):
```clojure
;; c ∈ [a.lo+b.lo .. a.hi+b.hi]
;; a ∈ [c.lo-b.hi .. c.hi-b.lo]
;; b ∈ [c.lo-a.hi .. c.hi-a.lo]
;; (nil bounds propagate: nil + 5 = nil, nil - 5 = nil)
```

**`narrow-times`** (a * b = c):
Over-approximate: `c ∈ [min(a.lo*b.lo, a.lo*b.hi, a.hi*b.lo, a.hi*b.hi) .. max(...)]`. Imprecise but sound — the solver compensates.

### Range and `members` / `finite?`

- `members` returns `nil` for ranges (they're not "finite" in the old sense)
- `finite?` returns `false` for ranges
- `size` returns `(- hi lo -1)` for bounded ranges, `nil` for unbounded
- This means existing narrowers that guard on `finite?` skip ranges — correct, because ranges get their own path
- The solver stops using `finite?` as the gate; it uses `steppable?` instead

## `query1` — one solution at a time

```clojure
(query1 expr) → [value search-state] or nil
(query1 search-state) → [value search-state] or nil
```

### Search state

The search state captures the solver's position in the search tree. It's a stack of **choice points**:

```clojure
{:kind :search-state
 :scope-path [ws-name]
 :choice-points [{:env env
                  :var-path path
                  :remaining-domain domain}  ;; the rest after step
                 ...]}
```

Each choice point says: "we chose a value for this variable; the `remaining-domain` is what we haven't tried yet." To get the next solution, pop the deepest choice point, step its remaining domain, and continue solving from there.

This is just the explicit representation of what backtracking search does implicitly via the call stack. Making it explicit = making it serializable, pausable, resumable.

### Implementation sketch

```clojure
(defn solve-step
  "Produce one solution from the search state.
   Returns [solved-env remaining-state] or nil."
  [env scope-path choice-points]
  ;; Find next variable to label
  (let [non-ground (collect-steppable env scope-path)]
    (cond
      ;; All ground — this is a solution
      (empty? non-ground)
      [env choice-points]

      ;; Pick most constrained variable, step its domain
      :else
      (let [var-path (pick-variable env non-ground)
            domain (domain-of env var-path)
            step-result (step domain)]
        (when step-result
          (let [[v rest-dom] step-result
                ;; Push choice point for backtracking
                cp {:env env :var-path var-path :remaining rest-dom}
                env' (try-assign env var-path v)]
            (if env'
              ;; Assignment succeeded — recurse deeper
              (or (solve-step env' scope-path (conj choice-points cp))
                  ;; Failed deeper — backtrack within this choice
                  (backtrack scope-path (conj choice-points cp)))
              ;; Assignment failed (contradiction) — try next value
              (let [env-with-rest (assoc-domain env var-path rest-dom)]
                (solve-step env-with-rest scope-path choice-points)))))))))

(defn backtrack
  "Pop the most recent choice point, step its remainder, continue."
  [scope-path choice-points]
  (when (seq choice-points)
    (let [cp (peek choice-points)
          rest-cps (pop choice-points)
          step-result (step (:remaining cp))]
      (if step-result
        (let [[v rest-dom'] step-result
              cp' (assoc cp :remaining rest-dom')
              env' (try-assign (:env cp) (:var-path cp) v)]
          (if env'
            (solve-step env' scope-path (conj rest-cps cp'))
            ;; This value failed — continue backtracking on this cp
            (backtrack scope-path (conj rest-cps (assoc cp :remaining rest-dom')))))
        ;; This choice point exhausted — backtrack further
        (backtrack scope-path rest-cps)))))
```

### `query1` implementation

```clojure
(defrecord SearchState [scope-path choice-points])

(defn query1* [expr-or-state]
  (if (instance? SearchState expr-or-state)
    ;; Continue from saved state: backtrack to get next solution
    (let [{:keys [scope-path choice-points]} expr-or-state]
      (when-let [[env cps] (backtrack scope-path choice-points)]
        [(extract-value env scope-path)
         (->SearchState scope-path cps)]))
    ;; Fresh query: setup env, solve for first solution
    (let [env (setup-and-bind expr-or-state)
          scope-path (:scope-path env)]
      (when-let [[env cps] (solve-step env scope-path [])]
        [(extract-value env scope-path)
         (->SearchState scope-path cps)]))))
```

### `query-lazy` from `query1`

```clojure
(defn query-lazy* [expr]
  (when-let [[v state] (query1* expr)]
    (lazy-seq
     (cons v (when state
               (query-lazy-continue state))))))

(defn- query-lazy-continue [state]
  (when-let [[v state'] (query1* state)]
    (lazy-seq
     (cons v (when state'
               (query-lazy-continue state'))))))
```

### `query` from `query-lazy` (backward compatible)

Existing `query` could internally use `query-lazy` + `vec`:

```clojure
(defn query* [expr]
  (vec (query-lazy* expr)))
```

But only if the result is guaranteed finite. For safety, `query` should check that all domains are bounded before enumerating, or set a hard limit.

## The `:range` as a building block

### `between` produces ranges

```clojure
;; before: (between 1 100) → finite set of 100 elements
;; after:  (between 1 100) → {:kind :range, :lo 1, :hi 100}
```

### Collection sizes

Composite domains gain a `:length` field:

```clojure
{:kind :vector-of, :element integer-dom, :length (range-dom 0 nil)}
```

`(= (count v) 3)` narrows `:length` to `(single 3)`. The enumerator then produces only 3-element vectors.

This is deferred — scalar ranges first — but the architecture supports it.

### Type narrowing

`(> x 0)` on `integer-dom` → `{:kind :range, :lo 1, :hi nil}`. The type narrows to a half-infinite range. This requires `intersect(type-integer, range[1..∞])` → `range[1..∞]` and the relational constraint narrowers to produce ranges from types.

## Implementation phases

### Phase 1: `:range` domain kind + algebra

Add to `domain.clj`:
- `range-dom` constructor with normalization
- `contains-val?` for ranges
- `range-relate` + register in `kind-relate`
- `overlap-intersect`, `overlap-unite`, `overlap-subtract` for range cases
- `domain-below`, `domain-above`, `domain-at-most`, `domain-at-least` for ranges (bounds adjustment)

Change `int-range` / `between` to produce ranges.

Add range-aware paths to narrowers in `bind.clj`:
- `narrow-lt`, `narrow-gt`, `narrow-lte`, `narrow-gte` — bounds adjustment
- `narrow-plus`, `narrow-minus` — interval arithmetic
- `narrow-times`, `narrow-mod`, `narrow-quot` — over-approximate bounds
- `narrow-eq` — intersect (if both ranges, range ∩ range)

Tests: all existing tests pass (bounded ranges that are small auto-normalize or the narrowers handle them). New tests for range algebra and range narrowing.

### Phase 2: `step` primitive

Add to `domain.clj`:
- `kind-step` dispatch table
- `step` function
- Implementations for `single`, `finite`, `range`
- Spiral implementation for unbounded ranges
- `steppable?` predicate

Tests: step produces correct values and remainders, exhaustion returns nil.

### Phase 3: `solve-step` + `query1`

Add `solve-step` and `backtrack` to `solve.clj`.
Add `query1*` and `query1` macro to `core.clj`.

The existing `solve` stays as-is (working, tested). `solve-step` is a new code path that uses `step` instead of `members`.

Tests: `query1` produces same first results as `query`. Repeated `query1` calls enumerate all solutions. Works on ranges and infinite domains.

### Phase 4: `query-lazy` + `query` migration

Add `query-lazy*` and `query-lazy` macro.
Optionally rewrite `query` in terms of `query-lazy`.

Tests: `(vec (query-lazy ...))` = `(query ...)` for all finite cases.

### Phase 5: Range-aware constraint propagation

Upgrade narrowers to handle range domains (not just finite).
`(> x 0)` on integer-dom → range.
`(+ x y)` where x is a range and y is finite → range-based narrowing.

This is where infinite domains start producing useful results.

## Open questions

1. **Unbounded integer stepping order**: `0, 1, -1, 2, -2, ...` is fair but might not match user expectations. Should positive integers come before negative? Should there be a way to control enumeration order?

2. **`:type` stepping**: How do you step `:string`? Length-based (`"", "a", "b", ...`) is systematic but the space is huge. `:keyword` similarly. Maybe these are only steppable after narrowing to a finite set? Or we provide a default enumeration but warn it's infinite.

3. **Composite stepping (deferred)**: When we get there, the dovetailing strategy matters. Length-first for vectors? Cantor diagonal for tuples? This is a real design decision but it can wait.

4. **`query` safety**: Should `query` refuse to run if any domain is infinite? Or should it have a configurable max-results limit? E.g., `(query {:limit 1000} expr)`.

5. **Interaction with forks**: The current fork expansion in `solve` uses `mapcat`. The new `solve-step` needs to handle forks too — expanding branches and interleaving their solution streams. This adds complexity to the choice-point stack.
