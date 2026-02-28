# mufl — The Domain Algebra

This document explains how mufl's domain algebra works internally: the data representations, the `relate` primitive, and how the full algebra derives from it. If you know Clojure but not mufl internals, start here.

For the user-facing language, see `doc/introduction.md`. For the tree architecture and bind mechanics, see `doc/design.md`.

The implementation lives in `src/mufl/domain.clj` (~590 lines). The tests in `test/mufl/domain_test.clj` serve as executable examples for everything described here.

---

## What is a domain?

A domain is the set of possible values a name can take. In a normal language, a variable holds one value. In mufl, a variable holds a domain — it might be a single concrete value, a finite set of candidates, an open type like "all integers," or the universal set "anything."

Every domain is a **plain Clojure map** with a `:kind` key and associated data. No functions in the map, so `=` works for equality, and domains are printable, serializable, and comparable by value.

### The built-in kinds

**`void`** — the empty set. No value is possible. This represents a contradiction.

```clojure
{:kind :void}
```

**`any`** — the universal set. We know nothing about what the value might be.

```clojure
{:kind :any}
```

**`single`** — exactly one value. This *is* a concrete value — there's no separate "value" concept in mufl, just a domain that happens to contain one element.

```clojure
(single 42)       ;=> {:kind :single, :value 42}
(single "hello")  ;=> {:kind :single, :value "hello"}
(single :ok)      ;=> {:kind :single, :value :ok}
```

**`finite`** — a finite enumeration. The set must have at least two elements — smaller sets normalize automatically:

```clojure
(finite #{1 2 3})  ;=> {:kind :finite, :values #{1 2 3}}
(finite #{42})     ;=> {:kind :single, :value 42}      ;; one element → single
(finite #{})       ;=> {:kind :void}                    ;; empty → void
```

This normalization is important: you never have to check for degenerate finite sets. A `{:kind :finite}` domain always has 2+ elements.

**`type`** — an open (infinite) type domain. Represents all values of a given type.

```clojure
(type-dom :integer)  ;=> {:kind :type, :type :integer}
(type-dom :string)   ;=> {:kind :type, :type :string}
(type-dom :number)   ;=> {:kind :type, :type :number}
(type-dom :keyword)  ;=> {:kind :type, :type :keyword}
(type-dom :boolean)  ;=> {:kind :type, :type :boolean}
```

Pre-built constants are available: `integer-dom`, `string-dom`, `number-dom`, `keyword-dom`, `boolean-dom`.

The type hierarchy is minimal: `:integer` is a subset of `:number`. All other type pairs are disjoint.

### Membership

`contains-val?` tests whether a specific value belongs to a domain:

```clojure
(contains-val? (single 42) 42)          ;=> true
(contains-val? (single 42) 43)          ;=> false
(contains-val? (finite #{1 2 3}) 2)     ;=> true
(contains-val? integer-dom 42)          ;=> true
(contains-val? integer-dom "hello")     ;=> false
(contains-val? any 42)                  ;=> true
(contains-val? void 42)                 ;=> false
```

For finite domains, `members` returns the set of values, and `size` returns the count:

```clojure
(members (finite #{1 2 3}))  ;=> #{1 2 3}
(members (single 42))        ;=> #{42}
(members void)               ;=> #{}
(members integer-dom)        ;=> nil    ;; open domains are not enumerable
(size (finite #{1 2 3}))     ;=> 3
```

---

## The `relate` primitive

Here's the core insight behind the algebra. Instead of writing separate `intersect`, `unite`, and `subtract` logic for every pair of domain kinds, we have **one** primitive that tells us the set-theoretic relationship between two domains. Everything else derives from it.

`(relate d1 d2)` returns one of five relations:

| Relation    | Meaning                              | Set notation   |
|-------------|--------------------------------------|----------------|
| `:equal`    | Same set of values                   | d1 = d2        |
| `:subset`   | d1 is strictly contained in d2       | d1 ⊂ d2        |
| `:superset` | d1 strictly contains d2              | d1 ⊃ d2        |
| `:disjoint` | No values in common                  | d1 ∩ d2 = ∅    |
| `:overlap`  | Partial overlap, neither contains the other | d1 ∩ d2 ≠ ∅, d1 ⊄ d2, d2 ⊄ d1 |

### Examples

```clojure
;; A single integer is a subset of all integers
(relate (single 42) integer-dom)                ;=> :subset

;; Integers and strings have nothing in common
(relate integer-dom string-dom)                 ;=> :disjoint

;; Two overlapping finite sets
(relate (finite #{1 2 3}) (finite #{2 3 4}))    ;=> :overlap

;; Every integer is a number
(relate integer-dom number-dom)                 ;=> :subset

;; A finite set of integers is contained in the integer type
(relate (finite #{1 2 3}) integer-dom)          ;=> :subset

;; Mixed-type finite set partially overlaps with integer type
(relate (finite #{1 2 "x"}) integer-dom)        ;=> :overlap

;; Same domain is equal to itself
(relate integer-dom integer-dom)                ;=> :equal
```

### Symmetry

`relate` is consistent under flipping: if `(relate a b)` returns `:subset`, then `(relate b a)` returns `:superset`. If it returns `:overlap`, the reverse also returns `:overlap`. This property is tested exhaustively in the test suite across all domain kinds.

```clojure
(relate (single 42) integer-dom)   ;=> :subset
(relate integer-dom (single 42))   ;=> :superset
```

### How `relate` dispatches

The top-level `relate` function handles the universal cases first, then delegates to kind-specific functions via a dispatch table:

```clojure
(defn relate [d1 d2]
  (cond
    (= d1 d2)   :equal       ;; structural equality — fast path
    (void? d1)   :subset      ;; void ⊂ everything
    (void? d2)   :superset    ;; everything ⊃ void
    (any? d1)    :superset    ;; any ⊃ everything (except any, caught above)
    (any? d2)    :subset      ;; everything ⊂ any
    :else
    ;; Double dispatch: try d1's handler, then flip d2's
    ...))
```

The `void` and `any` cases are handled universally because they relate the same way to every other domain — `void` is a subset of everything, `any` is a superset of everything.

For other kinds, `relate` uses a **dispatch table** — a map from `:kind` to a relate function:

```clojure
(def kind-relate
  {:single    single-relate
   :finite    finite-relate
   :type      type-relate
   :vector-of vector-of-relate
   :tuple     tuple-relate
   :map-of    map-of-relate})
```

The dispatch is **double**: first try d1's handler (which may return `nil` if it doesn't know how to compare with d2's kind), then try d2's handler and flip the result. If neither knows, default to `:disjoint`.

```clojure
(let [r1-fn (get kind-relate (:kind d1))
      r1    (when r1-fn (r1-fn d1 d2))]
  (or r1
      (let [r2-fn (get kind-relate (:kind d2))
            r2    (when r2-fn (r2-fn d2 d1))]
        (or (flip r2) :disjoint))))
```

This means kind-specific functions only need to handle the cases they understand. For example, `single-relate` knows how to compare a single value against any domain (via `contains-val?`), but `type-relate` doesn't handle `type` vs `single` — it returns `nil`, and the dispatcher flips to `single-relate` instead.

### Why this matters

The dispatch table keeps behavior separate from data. Domain values are pure maps — no polymorphic methods, no protocols. This means:

- **`=` works** — you can compare domains with Clojure's `=`
- **Printable** — domains print as readable maps
- **Serializable** — no functions embedded in domain values
- **Extensible** — adding a new kind means adding one entry to the dispatch table

---

## Deriving algebra from `relate`

The three algebra operations — `intersect`, `unite`, `subtract` — are thin case-dispatch wrappers over `relate`. Most of the logic is the same regardless of what kinds are involved:

### Intersect (narrowing)

`(intersect d1 d2)` returns the values that are in **both** domains.

```clojure
(defn intersect [d1 d2]
  (case (relate d1 d2)
    :equal    d1              ;; same set → either one
    :subset   d1              ;; d1 is smaller → d1
    :superset d2              ;; d2 is smaller → d2
    :disjoint void            ;; nothing in common → empty
    :overlap  (overlap-intersect d1 d2)))  ;; kind-specific
```

Four out of five cases need no kind-specific logic at all. Only `:overlap` requires computing the actual intersection, because neither domain fully contains the other.

```clojure
(intersect (finite #{1 2 3}) integer-dom)
;; relate returns :subset → result is (finite #{1 2 3})

(intersect integer-dom string-dom)
;; relate returns :disjoint → result is void

(intersect (finite #{1 2 3}) (finite #{2 3 4}))
;; relate returns :overlap → overlap-intersect computes #{2 3}
;=> {:kind :finite, :values #{2 3}}
```

### Unite (widening)

`(unite d1 d2)` returns all values that are in **either** domain.

```clojure
(defn unite [d1 d2]
  (case (relate d1 d2)
    :equal    d1              ;; same set → either one
    :subset   d2              ;; d2 is bigger → d2
    :superset d1              ;; d1 is bigger → d1
    :disjoint (disjoint-unite d1 d2)   ;; kind-specific
    :overlap  (overlap-unite d1 d2)))  ;; kind-specific
```

```clojure
(unite (finite #{1 2 3}) (finite #{2 3 4}))
;=> {:kind :finite, :values #{1 2 3 4}}

(unite void (finite #{1 2}))
;; relate returns :subset (void ⊂ anything) → result is (finite #{1 2})

(unite any (finite #{1 2}))
;; relate returns :superset (any ⊃ anything) → result is any
```

### Subtract (difference)

`(subtract d1 d2)` returns values in d1 that are **not** in d2.

```clojure
(defn subtract [d1 d2]
  (case (relate d1 d2)
    :equal    void            ;; remove everything → empty
    :subset   void            ;; d1 ⊂ d2 → nothing left
    :superset (superset-subtract d1 d2)  ;; kind-specific
    :disjoint d1              ;; no overlap → nothing removed
    :overlap  (overlap-subtract d1 d2))) ;; kind-specific
```

```clojure
(subtract (finite #{1 2 3}) (finite #{2 3}))
;=> {:kind :single, :value 1}   ;; normalized from #{1}

(subtract (finite #{1 2}) (finite #{1 2 3}))
;; relate returns :subset → result is void
```

### The overlap helpers

The kind-specific helpers (`overlap-intersect`, `overlap-unite`, etc.) handle the cases where `relate` returns `:overlap` or where a precise computation is needed. They dispatch on the kinds of the two domains:

- **Two finite domains** → set operations (`clojure.set/intersection`, `clojure.set/union`, `clojure.set/difference`)
- **Type vs finite** → filter the finite set by the type predicate
- **Two composite domains** → recurse into structure (more on this below)
- **Fallback** → conservative approximation (e.g., `any` for unknown unions)

The key insight is that these helpers are small and focused. The `relate`-based dispatch handles the majority of cases generically, and only the overlap/disjoint-unite/superset-subtract paths need kind-specific logic.

---

## Composite domains

The flat kinds (`void`, `any`, `single`, `finite`, `type`) describe scalar values. Composite domain kinds describe **collections with structural constraints**.

### `vector-of` — homogeneous vectors

A `vector-of` domain describes vectors where every element belongs to the same domain:

```clojure
(vector-of-dom integer-dom)
;=> {:kind :vector-of, :element {:kind :type, :type :integer}}

(vector-of-dom string-dom)
;=> {:kind :vector-of, :element {:kind :type, :type :string}}
```

Membership checks recurse into every element:

```clojure
(contains-val? (vector-of-dom integer-dom) [1 2 3])     ;=> true
(contains-val? (vector-of-dom integer-dom) [])           ;=> true
(contains-val? (vector-of-dom integer-dom) [1 "x"])      ;=> false
(contains-val? (vector-of-dom integer-dom) "hello")      ;=> false
```

### `tuple` — heterogeneous vectors with fixed length

A `tuple` domain describes vectors with an exact number of elements, each matching a specific domain:

```clojure
(tuple-dom [integer-dom string-dom])
;=> {:kind :tuple, :elements [{:kind :type, :type :integer}
;                              {:kind :type, :type :string}]}
```

Length must match exactly:

```clojure
(contains-val? (tuple-dom [integer-dom string-dom]) [1 "hi"])     ;=> true
(contains-val? (tuple-dom [integer-dom string-dom]) ["hi" 1])     ;=> false
(contains-val? (tuple-dom [integer-dom string-dom]) [1])          ;=> false
(contains-val? (tuple-dom [integer-dom string-dom]) [1 "hi" 3])  ;=> false
```

### `map-of` — homogeneous maps

A `map-of` domain describes maps where all keys belong to one domain and all values to another:

```clojure
(map-of-dom keyword-dom integer-dom)
;=> {:kind :map-of, :key {:kind :type, :type :keyword}
;                   :value {:kind :type, :type :integer}}
```

```clojure
(contains-val? (map-of-dom keyword-dom integer-dom) {:a 1 :b 2})  ;=> true
(contains-val? (map-of-dom keyword-dom integer-dom) {})            ;=> true
(contains-val? (map-of-dom keyword-dom integer-dom) {:a "x"})      ;=> false
(contains-val? (map-of-dom keyword-dom integer-dom) {"a" 1})       ;=> false
```

### Nesting

Composite domains nest freely. A vector of vectors of integers — a matrix domain:

```clojure
(def matrix-dom (vector-of-dom (vector-of-dom integer-dom)))

(contains-val? matrix-dom [[1 2] [3 4]])  ;=> true
(contains-val? matrix-dom [])             ;=> true
(contains-val? matrix-dom [[]])           ;=> true
(contains-val? matrix-dom [[1 "x"]])      ;=> false
```

### Pure data, all the way down

Like flat domains, composite domains are plain maps. Equality works:

```clojure
(= (vector-of-dom integer-dom) (vector-of-dom integer-dom))          ;=> true
(= (tuple-dom [integer-dom string-dom]) (tuple-dom [integer-dom string-dom]))  ;=> true
```

Composite domains are non-enumerable — `members` returns `nil`, `finite?` returns `false`. This makes sense: the set of all integer vectors is infinite.

---

## Relating composite domains

Composite domains participate fully in the `relate` primitive. The key principle: **structural recursion**.

### `vector-of` is covariant

The relation between two `vector-of` domains equals the relation between their element domains:

```clojure
(relate (vector-of-dom integer-dom) (vector-of-dom number-dom))
;=> :subset    ;; because integer ⊂ number

(relate (vector-of-dom number-dom) (vector-of-dom integer-dom))
;=> :superset  ;; because number ⊃ integer

(relate (vector-of-dom integer-dom) (vector-of-dom string-dom))
;=> :disjoint  ;; because integer ∩ string = ∅

(relate (vector-of-dom integer-dom) (vector-of-dom integer-dom))
;=> :equal
```

### `tuple` uses product relation

Each position contributes a relation, and the results are combined:

```clojure
(relate (tuple-dom [integer-dom string-dom])
        (tuple-dom [number-dom  string-dom]))
;=> :subset    ;; [integer ⊂ number, string = string] → subset

(relate (tuple-dom [number-dom  integer-dom])
        (tuple-dom [integer-dom number-dom]))
;=> :overlap   ;; [number ⊃ integer, integer ⊂ number] → mixed
```

The combination rules (implemented in `combine-relations`):
- Any `:disjoint` position → the whole tuple is `:disjoint`
- All `:equal` → `:equal`
- All `:equal` or `:subset` → `:subset`
- All `:equal` or `:superset` → `:superset`
- Otherwise → `:overlap`

Tuples of different lengths are always `:disjoint`:

```clojure
(relate (tuple-dom [integer-dom])
        (tuple-dom [integer-dom integer-dom]))
;=> :disjoint
```

### `map-of` uses product of key and value

Like `tuple`, but with exactly two positions — key and value:

```clojure
(relate (map-of-dom keyword-dom integer-dom)
        (map-of-dom keyword-dom number-dom))
;=> :subset    ;; keys equal, values subset

(relate (map-of-dom keyword-dom integer-dom)
        (map-of-dom string-dom  string-dom))
;=> :disjoint  ;; keys disjoint
```

### Cross-kind composites are disjoint

Different composite kinds never overlap:

```clojure
(relate (vector-of-dom integer-dom)
        (tuple-dom [integer-dom integer-dom]))
;=> :disjoint

(relate (vector-of-dom integer-dom)
        (map-of-dom keyword-dom integer-dom))
;=> :disjoint

(relate (tuple-dom [integer-dom])
        (map-of-dom keyword-dom integer-dom))
;=> :disjoint
```

### Composites vs flat domains are disjoint

A vector-of-integers is not an integer, and not a string:

```clojure
(relate (vector-of-dom integer-dom) string-dom)   ;=> :disjoint
(relate (vector-of-dom integer-dom) integer-dom)   ;=> :disjoint
(relate (vector-of-dom integer-dom) (single 42))   ;=> :disjoint
```

### Composites vs `void` and `any`

The universal rules apply — `void` is a subset of everything, `any` is a superset:

```clojure
(relate (vector-of-dom integer-dom) any)   ;=> :subset
(relate (vector-of-dom integer-dom) void)  ;=> :superset
(relate void (vector-of-dom integer-dom))  ;=> :subset
(relate any (vector-of-dom integer-dom))   ;=> :superset
```

### Composite algebra follows naturally

Since composites have `relate`, the full algebra — `intersect`, `unite`, `subtract` — works automatically for the non-overlap cases. For the overlap case, the helpers recurse into structure:

```clojure
;; Intersecting two vector-of domains → vector-of the intersection
(intersect (vector-of-dom integer-dom) (vector-of-dom number-dom))
;=> (vector-of-dom integer-dom)   ;; because integer ⊂ number

(intersect (vector-of-dom integer-dom) (vector-of-dom string-dom))
;=> void   ;; disjoint

;; Uniting two vector-of domains → vector-of the union
(unite (vector-of-dom integer-dom) (vector-of-dom number-dom))
;=> (vector-of-dom number-dom)   ;; because number ⊃ integer

;; Tuple intersection narrows each position independently
(intersect (tuple-dom [number-dom string-dom])
           (tuple-dom [integer-dom string-dom]))
;=> (tuple-dom [integer-dom string-dom])

;; Map-of intersection narrows both key and value domains
(intersect (map-of-dom keyword-dom number-dom)
           (map-of-dom keyword-dom integer-dom))
;=> (map-of-dom keyword-dom integer-dom)
```

Subtract on composites is approximate for superset/overlap cases — the precise remainder often can't be represented as a single domain. In those cases, the result conservatively returns the original:

```clojure
;; Superset subtract: number vectors minus integer vectors
;; Can't precisely represent "vectors of non-integer numbers"
(subtract (vector-of-dom number-dom) (vector-of-dom integer-dom))
;=> (vector-of-dom number-dom)   ;; approximation
```

---

## How composite domains bridge into the tree

When the mufl runtime encounters a composite type constructor — whether from `(narrow v (vector-of integer))`, a `def` schema like `(def IntVec (vector-of integer))`, or `=` unification between a variable and a composite domain — it builds a composite domain and applies it structurally.

The mechanism is `apply-composite-constraint` in `bind.clj`. It walks the collection node's children and intersects each element's domain with the appropriate part of the composite domain:

- For `vector-of`: every child gets intersected with the element domain
- For `tuple`: each child at position *i* gets intersected with `(nth elements i)`
- For `map-of`: each key child gets intersected with the key domain, each value child with the value domain

This is the **same mechanism** regardless of how the composite constraint was introduced. The user writes `(narrow v (vector-of integer))` or `(narrow v IntVec)` or `(= v some-typed-vector)`, and the same structural walk applies the constraint.

**Callable domains** extend this further: any bound domain value can be called directly as a function — `(intvec v)` is equivalent to `(narrow v intvec)`. This works in both expression and pattern position (see [Introduction — Callable domains](introduction.md#callable-domains) for syntax). The same `apply-domain-constraint` mechanism in `schema.clj` handles both the `narrow` form and the callable domain dispatch.

---

## Ordered and arithmetic operations

Beyond the set-theoretic algebra, the domain module provides operations for constraint propagation on ordered and arithmetic domains.

### Ordered operations

These filter finite domains by comparison to a bound:

```clojure
(domain-below (finite #{1 2 3 4 5}) 3)     ;=> (finite #{1 2})
(domain-above (finite #{1 2 3 4 5}) 3)     ;=> (finite #{4 5})
(domain-at-most (finite #{1 2 3 4 5}) 3)   ;=> (finite #{1 2 3})
(domain-at-least (finite #{1 2 3 4 5}) 3)  ;=> (finite #{3 4 5})
(domain-min (finite #{1 2 3}))              ;=> 1
(domain-max (finite #{1 2 3}))              ;=> 3
```

These power the propagation for `<`, `>`, `<=`, `>=` constraints. When `(< x y)` is applied, `x`'s domain is filtered to values below `y`'s maximum, and `y`'s domain is filtered to values above `x`'s minimum.

For open domains (types like `:integer`), these operations return the domain unchanged — they can't enumerate an infinite set.

### Arithmetic on domains

Pairwise operations on finite domains compute all possible results:

```clojure
(domain-add (finite #{1 2}) (finite #{2 3}))  ;=> (finite #{3 4 5})
(domain-sub (finite #{1 2}) (finite #{1 2 3}))  ;=> (finite #{-2 -1 0 1})
(domain-mul (finite #{1 2}) (finite #{2 3}))  ;=> (finite #{2 3 4 6})
```

These power `+`, `-`, `*`, `mod`, and `quot` constraint propagation. When `(= s (+ x y))` is constrained and any of the three domains narrows, the arithmetic domain is recomputed and the others are narrowed accordingly.

For open domains, arithmetic returns `any` — the system can't enumerate infinite pairwise combinations.

---

## Summary

The domain algebra is built on a small set of design choices:

1. **Domains are pure data maps** — a `:kind` key and associated fields. No functions, no protocols. `=` works.

2. **`relate` is the single primitive** — every pair of domains yields one of five relations. This replaces per-operation, per-kind-pair logic with a uniform foundation.

3. **Algebra derives from `relate`** — `intersect`, `unite`, and `subtract` are case dispatches. Only the `:overlap` case (and a few others) need kind-specific helpers.

4. **A dispatch table routes behavior** — `kind-relate` maps each `:kind` to its relate function. Double dispatch (try d1, flip d2) means each handler only needs to know about its own kind.

5. **Composite domains are first-class** — `vector-of`, `tuple`, and `map-of` participate in the full algebra via structural recursion (covariance, product relations).

6. **One mechanism for structural constraints** — whether a composite domain comes from a binary form, a `defdomain` schema, or unification, the same tree-walking apply-constraint path handles it.

For the implementation, see `src/mufl/domain.clj`. For executable examples, see `test/mufl/domain_test.clj`.
