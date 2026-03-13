# mufl

A constraint-logic language embedded in Clojure.

## What is it

Variables hold **domains** — sets of possible values — instead of single values. Constraints **narrow** those domains by removing impossibilities. `query` enumerates every solution that satisfies all constraints. Think miniKanren / CLP(FD), but with Clojure syntax and bidirectional functions.

## Quick taste

Literals pass through. `one-of` introduces uncertainty. Constraints narrow domains — no booleans, no generate-and-test.

```clojure
(query (let [x (one-of 1 2 3 4 5)]
         (and (> x 2) x)))
;=> [3 4 5]
```

Multiple variables, constraints propagate across all of them:

```clojure
(query (let [x (one-of 1 2 3 4 5)
             y (one-of 1 2 3 4 5)]
         (and (< x y)
              (= (+ x y) 6)
              [x y])))
;=> [[2 4] [1 5]]
```

Functions are **bidirectional** — define once, use to construct *and* destructure:

```clojure
(query (do (defn point [x y] {:x (integer x) :y (integer y)})

           ;; construct
           (point 1 2)))
;=> [{:x 1 :y 2}]

(query (do (defn point [x y] {:x (integer x) :y (integer y)})

           ;; destructure + constrain
           (let [(point a b) {:x (one-of 1 2 3 4 5) :y (one-of 1 2 3 4 5)}]
             (< a b)
             (= (+ a b) 6)
             [a b])))
;=> [[2 4] [1 5]]
```

Declare what a Pythagorean triple *is*; the engine finds them:

```clojure
(query (let [a (between 1 15)
             b (between 1 15)
             c (between 1 15)]
         (and (< a b)
              (<= b c)
              (= (+ (* a a) (* b b)) (* c c))
              [a b c])))
;=> [[3 4 5] [6 8 10] [5 12 13] [9 12 15]]
```

## Key features

- **Bidirectional functions** — `fn`/`defn` work as both constructors and destructors; body constraints apply in both directions
- **Constraint propagation** — arithmetic, relational, and structural constraints propagate backward through expressions
- **Structural pattern matching** — map/vector destructuring, dot-rest syntax, `as`, `ks`, `or` patterns
- **Collection operations** — `map`, `filter`, `reduce`, `sort`, `sort-by`, `distinct`, `every`, `some` — all constraint-aware
- **Type schemas** — `narrow`, `vector-of`, `map-of`, `tuple`; compose with `and`; callable as domain functions
- **Multiple query modes** — `query` (eager), `query-lazy` (lazy), `query+` (full bindings), `query1` (step-through)
- **Multi-branch `fn`** — literal/type/arity dispatch with pattern matching; clean recursion without `if`

## Getting started

Add as a local dependency in `deps.edn`:

```clojure
{:deps {mufl/mufl {:local/root "../mufl"}}}
```

Then:

```clojure
(require '[mufl.core :refer [query]])

(query (let [x (between 1 10)]
         (and (even x) (> x 5) x)))
;=> [6 8 10]
```

See [`src/mufl/showcase.clj`](src/mufl/showcase.clj) for a full guided tour — evaluate the `(comment ...)` blocks in a REPL.

## Running tests

```bash
# Run JVM tests
clj -M:test

# Run CLJS tests (Node.js)
npx shadow-cljs compile test
```

## ClojureScript

mufl works on both JVM Clojure and ClojureScript (Node.js). All source and test files are `.cljc` with reader conditionals for the few platform-specific bits. The CLJS build uses shadow-cljs with a `:node-test` target.

## Status

Active development. 602 tests passing on JVM, 593 on CLJS.
