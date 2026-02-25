# Lazy Enumeration

> *Prerequisites: familiarity with `query`, `one-of`, `between`, `and`, and constraints — see [Introduction](introduction.md).*

`query` returns *all* solutions. That's perfect for small domains, but what happens when the search space gets large?

```clojure
(query (let [x (between 1 1000000)] x))
;; → a vector of 1,000,000 values
```

Two such variables? A trillion combinations. You don't want to materialize that.

mufl provides two alternatives: **`query-lazy`** returns a lazy sequence — compute only what you `take` — and **`query1`** hands you one solution at a time with a continuation to get the next.

---

## query-lazy — solutions on demand

`query-lazy` has the same syntax as `query`, but returns a lazy sequence instead of a vector. Solutions are produced only as you consume them.

```clojure
(take 5 (query-lazy (let [x (between 1 100)] x)))
;=> (1 2 3 4 5)
```

Only 5 values were computed, not 100.

### With constraints

Constraints still narrow domains *before* enumeration — lazy search doesn't mean generate-and-test.

```clojure
(query-lazy (let [x (between 1 10)]
              (and (> x 5) x)))
;=> (6 7 8 9 10)
```

`(> x 5)` narrows `x` to 6..10 at bind time. `query-lazy` then lazily walks those 5 values. It never considers 1 through 5.

```clojure
(take 3 (query-lazy (let [x (between 1 10)]
                      (and (> x 3)
                           (= 0 (mod x 2))
                           x))))
;=> (4 6 8)
```

Multiple constraints compose: `x > 3` narrows to 4..10, then the even constraint filters during enumeration.

### Multi-variable queries

```clojure
(take 4 (query-lazy (let [x (one-of 1 2)
                          y (one-of 10 20)]
                      [x y])))
;=> ([1 10] [1 20] [2 10] [2 20])
```

### Equivalence with query

For finite domains, `query-lazy` produces the same solutions as `query` (as a lazy sequence instead of a vector):

```clojure
(= (set (query       (let [x (one-of 1 2 3)] x)))
   (set (query-lazy  (let [x (one-of 1 2 3)] x))))
;=> true
```

---

## query1 — one solution at a time

`query1` returns a pair `[value search-state]` or `nil` if no solution exists. The search state captures the solver's position — pass it back to `query1*` to get the next solution.

```clojure
(let [[v state] (query1 (let [x (one-of 1 2 3)] x))]
  v)
;=> 1
```

To get the next solution, feed the state back:

```clojure
(let [[v1 state] (query1 (let [x (one-of 1 2 3)] x))
      [v2 state'] (query1* state)]
  [v1 v2])
;=> [1 2]
```

When there are no more solutions, `query1*` returns `nil`:

```clojure
(query1 (let [x (one-of 1 2 3)]
          (and (> x 10) x)))
;=> nil
```

This is exactly what `query-lazy` does internally — it unfolds `query1*` into a lazy sequence. Use `query1` when you want explicit control over stepping; use `query-lazy` when you just want a lazy seq.

---

## Three styles, one query

Here's the same problem — even numbers from 1 to 10 — expressed three ways:

```clojure
;; All at once
(query (let [x (between 1 10)]
         (and (= 0 (mod x 2)) x)))
;=> [2 4 6 8 10]

;; Lazy sequence
(query-lazy (let [x (between 1 10)]
              (and (= 0 (mod x 2)) x)))
;=> (2 4 6 8 10)

;; One at a time
(let [[v _] (query1 (let [x (between 1 10)]
                      (and (= 0 (mod x 2)) x)))]
  v)
;=> 2
```

**Use `query`** when you need all solutions and the domain is small.
**Use `query-lazy`** when you want solutions on demand, or need only the first few from a large space.
**Use `query1`** when you want fine-grained control — inspect one solution, decide whether to continue.

---

## Constraint propagation + lazy search

What makes this different from a simple lazy range? **Constraints narrow before enumeration.**

```clojure
(query-lazy (let [x (between 1 100)]
              (and (> x 95) x)))
;=> (96 97 98 99 100)
```

This doesn't try values 1 through 95 and reject them. The constraint `(> x 95)` narrows `x`'s domain from 1..100 to 96..100 *at bind time*. The lazy enumerator then walks exactly 5 values.

This is key: constraint propagation and lazy search compose. Propagation eliminates impossible values upfront; laziness avoids materializing the survivors until you need them.

### Multi-variable with constraints

```clojure
(take 5 (query-lazy (let [x (between 1 10)
                          y (between 1 10)]
                      (and (= x 5) [x y]))))
;=> ([5 1] [5 2] [5 3] [5 4] [5 5])
```

The equality `(= x 5)` grounds `x` immediately. Only `y` needs enumeration — and it's lazy.

---

## How it works: domains as iterators

Behind the scenes, lazy enumeration is powered by a simple idea: **domains can step**.

Every enumerable domain implements `step`, which returns `[value, remainder-domain]` — one value extracted, plus a new domain representing everything that's left.

```
step({1..5})  → [1, {2..5}]
step({2..5})  → [2, {3..5}]
step({5..5})  → [5, void]
step(void)    → nil
```

A finite set works the same way:

```
step(#{3 1 2})  → [1, #{2 3}]
step(#{2 3})    → [2, #{3}]
step(#{3})      → [3, void]
```

There's no separate iterator or stream abstraction. The domain *is* the state. `step` extracts a value and returns a smaller domain. When the domain is exhausted, `step` returns `nil`.

This is what lets `query-lazy` work without special runtime machinery — the solver just steps domains, one value at a time, building solutions lazily.

---

## Summary

| Form | Returns | Use when |
|---|---|---|
| `query` | Vector of all solutions | Small, finite domains |
| `query-lazy` | Lazy seq of solutions | Large domains, or you only need the first N |
| `query1` | `[value state]` or `nil` | You want explicit control over iteration |
| `query+` | Vector of full environments | Inspecting all variable bindings |

The mental model hasn't changed — you still describe constraints and let the system find valid answers. The difference is *how many* answers you ask for, and *when* they're computed.

---

For the full constraint language, see [Introduction](introduction.md). For internal design details, see [Design: Lazy Enumeration](design-lazy-enumeration.md).
