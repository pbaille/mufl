# mufl — An Introduction

mufl is a small language where values can be *uncertain* and constraints *narrow* them. If you know Scheme or Clojure, most of the syntax will feel familiar — `let`, `fn`, vectors, maps, keywords — but underneath, something different is happening.

In a normal language, a variable holds *one* value. In mufl, a variable holds a **domain** — a set of possible values. You write constraints, and the system automatically eliminates impossibilities. When you ask for results, you get back every combination that satisfies all the constraints.

The entry point is `query`. It returns a vector of all solutions. It accepts multiple body forms (implicit `do`), so you can define constraints and domains before your main expression without wrapping in `do`.

---

## Values

Literals work as you'd expect. A literal is a domain containing exactly one value. `query` always returns a vector of all solutions — a literal has exactly one, so you get a single-element vector.

```clojure
(query 42)        ;=> [42]
(query "hello")   ;=> ["hello"]
(query :ok)       ;=> [:ok]
(query true)      ;=> [true]
```

## Uncertain values

`one-of` creates a variable that could be any of the given values. Values can be of any type — integers, strings, keywords, mixed:

```clojure
(query (let [x (one-of 1 2 3)] x))
;=> [1 2 3]

(query (let [x (one-of "red" "green" "blue")] x))
;=> ["red" "green" "blue"]

(query (let [x (one-of 1 "hello" :ok)] x))
;=> [1 "hello" :ok]
```

`between` creates an integer range from lo to hi (inclusive).

```clojure
(query (let [x (between 1 5)] x))
;=> [1 2 3 4 5]
```

Nothing interesting yet — we're just enumerating. The power comes when we add constraints.

## Constraints

In most languages, `(> x 2)` is a question — it returns a boolean. The value `x` is untouched; you get `true` or `false`, then decide what to do with it.

mufl has no booleans. `(> x 2)` removes every value ≤ 2 from `x`'s domain and **returns `x`** — the narrowed value itself, not a truth value. The constraint is the computation.

```clojure
(query (let [x (one-of 1 2 3 4 5)]
         (> x 2)))
;=> [3 4 5]
```

All relational operators — `=`, `!=`, `<`, `>`, `<=`, `>=` — work this way: narrow the domain, return the value. No boolean intermediary, no separate test-then-act step.

The same principle extends to every narrowing operation in mufl. Numeric constraints — `even`, `odd`, `pos`, `neg`, `zero` — filter by property. Type constraints — `integer`, `string`, `number`, `keyword`, `boolean` — filter by type:

```clojure
(query (let [x (between 1 10)]
         (and (even x) x)))
;=> [2 4 6 8 10]

(query (let [x (one-of 1 "hello" :ok)]
         (integer x)))
;=> [1]
```

`and` sequences constraints — each expression narrows further, and the last is the return value:

```clojure
(query (let [x (one-of 1 2 3 4 5)]
         (and (> x 1)
              (< x 5)
              (!= x 3)
              x)))
;=> [2 4]
```

Since `(!= x 3)` already returns `x`, the trailing `x` is technically redundant here. But making the return value explicit in `and` is often clearer — the key point is that constraints *are* value-returning expressions, not boolean tests.

When multiple variables are involved, constraints propagate across all of them. The system finds every valid combination:

```clojure
(query (let [x (one-of 1 2 3 4 5)
             y (one-of 1 2 3 4 5)]
         (and (< x y)
              (= (+ x y) 6)
              [x y])))
;=> [[2 4] [1 5]]
```

`(= (+ x y) 6)` narrows *both* `x` and `y`. `(< x y)` eliminates symmetric pairs. The solver finds all combinations that satisfy every constraint simultaneously.

## Control flow

Since there are no booleans, control flow doesn't test truth values — it explores which constraints are *satisfiable* and combines the results.

`or` tries each branch and unions the resulting domains:

```clojure
(query (let [x (between 1 10)]
         (and (or (< x 3) (> x 7)) x)))
;=> [1 2 8 9 10]
```

`not` flips a relational constraint — `(not (< x 3))` becomes `(>= x 3)`:

```clojure
(query (let [x (one-of 1 2 3 4 5)]
         (and (not (< x 3)) x)))
;=> [3 4 5]
```

Constraints compose naturally through `and` and `or`:

```clojure
;; Even AND positive
(query (let [x (one-of -4 -2 0 1 2 3 4)]
         (and (even x) (pos x) x)))
;=> [2 4]

;; Even OR negative
(query (let [x (one-of -3 -1 1 2 3 4)]
         (and (or (even x) (neg x)) x)))
;=> [-3 -1 2 4]
```

### `if` and `cond`

In a conventional language, `if` evaluates a boolean and takes one branch. In mufl, `if` tries the condition as a narrowing operation:

- If only one branch is satisfiable → takes it immediately.
- If both branches are satisfiable → explores both, returns the union of their solutions.

```clojure
(query (let [x (between 1 10)]
         (if (< x 5) x 10)))
;=> [1 2 3 4 10]
```

For values 1–4, `(< x 5)` is satisfiable — the then-branch yields `x`. For values 5–10, the negation `(>= x 5)` holds — the else-branch yields `10`. The result is the union of both.

`cond` chains multiple branches, each tried as a constraint:

```clojure
(query (let [x (between 1 10)]
         (cond
           (< x 4) x
           (> x 7) x
           :else 0)))
;=> [1 2 3 8 9 10 0]
```

When the condition is decidable — ground values, no uncertainty — exactly one branch is taken eagerly. This is what makes recursion work: a recursive call with ground arguments resolves `if` at bind time, the base case terminates, and the recursive case continues. Without eager resolution, both branches would be explored and recursion would never bottom out.

## Functions

`fn` creates a **bidirectional** computation. A function works in expression position (call it to construct a value) *and* in pattern position (use it to destructure a value). This isn't a special mode you opt into — it's what functions are in mufl.

### Basic usage

Functions are closures. Constraints propagate through function calls:

```clojure
(query (let [double (fn [x] (+ x x))
             n (between 1 5)]
         (and (= (double n) 6) n)))
;=> [3]
```

The constraint `(= (double n) 6)` propagates backward through the function body: `x + x = 6` implies `x = 3`.

### Bidirectionality

The body of a function is an invertible template. In expression position it constructs; in pattern position it destructures.

```clojure
;; Define a function — body is the template
(query (let [point (fn [x y] {:x (integer x) :y (integer y)})]
         ;; Expression position → constructs a value
         (point 1 2)))
;=> [{:x 1 :y 2}]

;; Pattern position → destructures a value
(query (let [point (fn [x y] {:x (integer x) :y (integer y)})
             (point a b) {:x 1 :y 2}]
         [a b]))
;=> [[1 2]]
```

The destructor inverts the body: `{:x (integer x) :y (integer y)}` becomes a destructuring pattern that extracts `:x` and `:y` with `integer` type guards. Parameters with type wrappers constrain the bound values in both directions.

Destructuring narrows values — only domain members that fit the pattern survive:

```clojure
(query (let [point (fn [x y] {:x (integer x) :y (integer y)})
             (point a b) {:x (one-of 1 "hello" 2) :y 10}]
         [a b]))
;=> [[1 10] [2 10]]
```

`(point a b) {:x (one-of 1 "hello" 2) :y 10}` means "destructure this map as a point" — the `integer` guard eliminates `"hello"` from `a`'s domain.

### Body-level constraints

Each branch has a single expression. Use `and` to combine constraints with the value template — the intermediate expressions narrow, and the last expression is the return value. These constraints apply in both directions:

```clojure
(query (let [small-point (fn [x y]
                           (and (>= x 1) (<= x 3)
                                {:x (integer x) :y (integer y)}))
             (small-point a b) {:x (one-of 0 1 2 3 4) :y 10}]
         [a b]))
;=> [[1 10] [2 10] [3 10]]
```

The constraints `(>= x 1) (<= x 3)` in the `and` eliminate 0 and 4 from `a`'s domain during destructuring.

### Parameter type wrappers

Wrapping a parameter in a type constrains it at call time *and* during destructuring:

```clojure
(query (let [point (fn [(integer x) (integer y)] {:x x :y y})]
         ;; At call time: args must be integers
         (point 1 2)))
;=> [{:x 1 :y 2}]

;; During destructuring: extracted values are narrowed to integers
(query (let [point (fn [(integer x) (integer y)] {:x x :y y})
             (point a b) {:x (one-of 1 "hello" 2) :y 10}]
         [a b]))
;=> [[1 10] [2 10]]
```

### `defn` — naming functions

`defn` is sugar for `(def name (fn ...))`. It names a function, nothing more:

```clojure
(defn point [x y] {:x (integer x) :y (integer y)})
;; ≡ (def point (fn [x y] {:x (integer x) :y (integer y)}))
```

Since `fn` is inherently bidirectional, `defn` inherits both call and destructure:

```clojure
(query (do (defn point [x y] {:x (integer x) :y (integer y)})
           (point 1 2)))
;=> [{:x 1 :y 2}]

(query (do (defn point [x y] {:x (integer x) :y (integer y)})
           (let [(point a b) {:x 1 :y 2}]
             [a b])))
;=> [[1 2]]
```

Destructors compose with `as` to bind the whole value alongside extracted parts:

```clojure
(query (do (defn point [x y] {:x (integer x) :y (integer y)})
           (let [(as p (point x y)) {:x (one-of 1 2 3) :y 10}]
             (and (> x 1) [p x y]))))
;=> [[{:x 2 :y 10} 2 10] [{:x 3 :y 10} 3 10]]
```

### Named constraint functions

Since function bodies are constraint expressions, `defn` naturally creates named constraints:

```clojure
(query (defn positive [x] (> x 0))
       (defn ordered [a b] (< a b))
       (let [x (one-of -1 0 1 2 3)
             y (one-of -1 0 1 2 3)]
         (positive x)
         (positive y)
         (ordered x y)
         [x y]))
;=> [[1 2] [1 3] [2 3]]
```

`positive` and `ordered` are ordinary functions — their bodies are constraints that narrow the arguments. They work in expression position to filter domains.

### Multi-branch functions

`fn` supports multiple branches — each vector starts a new clause, tried in order. The first satisfiable branch wins:

```clojure
(query (let [f (fn [0] :zero
                   [1] :one
                   [n] :other)]
         (f 0)))
;=> [:zero]
```

Params support the full pattern language — literals, type wrappers, destructuring:

```clojure
(query (let [describe (fn [(integer x)] :int
                          [(string x)]  :str)]
         [(describe 42) (describe "hi")]))
;=> [[:int :str]]
```

Branches can have different arities:

```clojure
(query (let [f (fn [x]   (* x 2)
                   [x y] (+ x y))]
         [(f 5) (f 3 4)]))
;=> [[10 7]]
```

An optional trailing expression (not preceded by a vector) is the default — used when no branch matches:

```clojure
(query (let [f (fn [0] :zero
                   42)]
         (f 5)))
;=> [42]
```

## Recursion

Multi-branch makes recursion natural — the base case is a pattern, no `if` needed:

```clojure
(query (let [fact (fn [0] 1
                      [n] (* n (fact (- n 1))))]
         (fact 5)))
;=> [120]

(query (let [fib (fn [0] 0
                      [1] 1
                      [n] (+ (fib (- n 1)) (fib (- n 2))))]
         (fib 10)))
;=> [55]
```

Single-branch with `if` still works when you prefer it:

```clojure
(query (let [base 2
             pow (fn [n] (if (= n 0) 1 (* base (pow (- n 1)))))]
         (pow 5)))
;=> [32]
```

## Collections

Vectors and maps are first-class. Their elements are individual domain nodes, so constraints can target any element.

```clojure
;; Vector access
(query (let [v [10 20 30]] (nth v 1)))
;=> [20]

;; Map access
(query (let [m {:x 1 :y 2}] (get m :x)))
;=> [1]

;; Keyword-as-function sugar
(query (let [m {:x 1 :y 2}] (:x m)))
;=> [1]
```

Constraints propagate through collection access:

```clojure
(query (let [m {:x (one-of 1 2 3) :y 10}]
         (and (> (:x m) 1) m)))
;=> [{:x 2 :y 10} {:x 3 :y 10}]
```

Nested collections work too:

```clojure
(query (let [m {:a {:b 42}}]
         (:b (:a m))))
;=> [42]
```

`count` returns the number of elements in a vector or map:

```clojure
(query (let [v [10 20 30]] (count v)))
;=> [3]

(query (let [m {:x 1 :y 2}] (count m)))
;=> [2]
```

It works on rest-pattern results too:

```clojure
(query (let [[a b . xs] [1 2 3 4 5]] (count xs)))
;=> [3]
```

`drop` removes the first n elements from a vector:

```clojure
(query (let [v [1 2 3 4 5]] (drop 2 v)))
;=> [[3 4 5]]
```

`dissoc` removes keys from a map. It supports multiple keys:

```clojure
(query (let [m {:x 1 :y 2 :z 3}] (dissoc m :x)))
;=> [{:y 2 :z 3}]

(query (let [m {:x 1 :y 2 :z 3}] (dissoc m :x :y)))
;=> [{:z 3}]
```

## Destructuring

Let bindings support map and vector destructuring, with full constraint propagation. Destructuring propagates domain information — each bound symbol gets the tightest domain the system can infer from the seed value.

```clojure
;; Map destructuring
(query (let [{:x x :y y} {:x (one-of 1 2 3) :y (one-of 1 2 3)}]
         (and (< x y) [x y])))
;=> [[1 2] [1 3] [2 3]]

;; Vector destructuring
(query (let [[a b c] [10 20 30]] (+ a b)))
;=> [30]
```

### Capturing the whole value

Use the `as` operator to bind the whole value while also destructuring:

```clojure
(query (let [(as v [a b]) [(one-of 1 2 3) 10]]
         (and (> a 1) v)))
;=> [[2 10] [3 10]]

(query (let [(as m {:x x}) {:x 1 :y 2}] [m x]))
;=> [[{:x 1 :y 2} 1]]
```

### Destructuring operators

Destructuring operators work exclusively in pattern position. When `let` encounters a list pattern `(op args...)`, the operator destructures the value — extracting sub-values and propagating domain information to each bound symbol.

```clojure
;; ks — keyword-symbol map destructuring
;; Extracts keys named after the symbols
(query (let [(ks x y) {:x 1 :y 2}] (+ x y)))
;=> [3]

;; as — bind whole + inner destructure
(query (let [(as m (ks x y)) {:x 1 :y 2}] [m x]))
;=> [[{:x 1 :y 2} 1]]

;; or — pattern fallback (tries patterns in order)
(query (let [(or (ks x) [x]) {:x 42}] x))
;=> [42]
```

Operators compose naturally with literal patterns and each other:

```clojure
;; Nested destructuring: ks inside vector pattern
(query (let [[(ks x) b] [{:x 1} 20]] x))
;=> [1]

;; as with ks inside
(query (let [(as whole (ks x y)) {:x 1 :y 2}] whole))
;=> [{:x 1 :y 2}]
```

### Dot syntax (rest patterns)

The `.` (dot) symbol in vector and map patterns captures the remaining elements, inspired by immucode's convention.

**Vector rest** — `[a b . xs]` binds positional elements before the dot and collects the rest:

```clojure
;; xs gets everything from index 2 onward
(query (let [[a b . xs] [1 2 3 4 5]] xs))
;=> [[3 4 5]]

;; Works with a single positional element
(query (let [[a . xs] [10 20 30]] [a xs]))
;=> [[10 [20 30]]]

;; Bare dot — ignore rest (just bind positionals)
(query (let [[a b .] [1 2 3 4]] [a b]))
;=> [[1 2]]

;; Combined with as operator
(query (let [(as v [a b . xs]) [1 2 3 4 5]] [xs v]))
;=> [[[3 4 5] [1 2 3 4 5]]]

;; Nested destructuring on the rest
(query (let [[a b . [c d]] [1 2 3 4]] [c d]))
;=> [[3 4]]
```

**Head/middle/tail** — when two or more elements follow the dot, the first captures the middle slice and the rest are positional from the end:

```clojure
;; a = first, mid = middle elements, c = last
(query (let [[a . mid c] [1 2 3 4 5]] [a mid c]))
;=> [[1 [2 3 4] 5]]

;; Two head positionals, two tail positionals
(query (let [[a b . mid x y] [1 2 3 4 5 6 7]] [a b mid x y]))
;=> [[1 2 [3 4 5] 6 7]]

;; Empty middle when vector is exactly head + tail
(query (let [[a . mid c] [1 2]] [a mid c]))
;=> [[1 [] 2]]

;; Nested patterns work in tail position too
(query (let [[a . mid {:x x}] [1 2 3 {:x 4}]] [a mid x]))
;=> [[1 [2 3] 4]]
```

The general form is `[h₁ h₂ ... . rest t₁ t₂ ...]` where `h` elements bind from the start, `t` elements bind from the end, and `rest` captures everything in between. One element after the dot is always a simple rest binding (backward compatible); two or more triggers head/middle/tail decomposition.

**General map patterns** — `{:key pattern}` where values are arbitrary patterns, not just symbols:

```clojure
;; Values are binding symbols
(query (let [{:x a :y b} {:x 1 :y 2}] [a b]))
;=> [[1 2]]

;; Values can be nested patterns
(query (let [{:x a :y [b c]} {:x 1 :y [2 3]}] [a b c]))
;=> [[1 2 3]]
```

**Map rest** — `{:key val . rest}` captures remaining keys after the named ones:

```clojure
;; rest gets everything except :x
(query (let [{:x a . rest} {:x 1 :y 2 :z 3}] rest))
;=> [{:y 2 :z 3}]

;; Nested destructuring on the rest via ks
(query (let [{:x a . (ks y z)} {:x 1 :y 2 :z 3}] [a y z]))
;=> [[1 2 3]]

;; Combined with as operator
(query (let [(as m {:x a . rest}) {:x 1 :y 2 :z 3}] [rest m]))
;=> [[{:y 2 :z 3} {:x 1 :y 2 :z 3}]]
```

## Domains (def and narrow)

`def` binds a name to a value — it's just an alias. `narrow` constrains a value against a domain expression — scalar, composite, or structural.

```clojure
(query (def person {:name string :age (between 0 150)})
       (let [p {:name "Alice" :age (one-of 10 25 200)}]
         (narrow p person)
         (:age p)))
;=> [10 25]
```

`person` is just a map value `{:name string :age (between 0 150)}`. `(narrow p person)` constrains each field of `p` against the corresponding domain: `:name` must be a string, `:age` must be in 0..150. The value 200 is eliminated.

`narrow` also works with scalar domains directly:

```clojure
(query (let [x (one-of 1 "hello" :foo)]
         (narrow x integer)
         x))
;=> [1]
```

Domain schemas compose with `and`:

```clojure
(query (def person {:name string :age (between 0 150)})
       (def employee (and person {:company string}))
       (let [e {:name "Alice" :age (one-of 30 200) :company "Acme"}]
         (narrow e employee)
         (:age e)))
;=> [30]
```

Vector literals define tuple schemas — no need for the `tuple` wrapper:

```clojure
(query (def point [integer integer])
       (let [p [(one-of 3 "x") (one-of 4 "y")]]
         (narrow p point)
         p))
;=> [[3 4]]
```

named schemas and constraints combine:

```clojure
(query (def person {:name string :age (between 0 150)})
       (let [p {:name "Alice" :age (one-of 10 25 30)}]
         (narrow p person)
         (>= (:age p) 18)
         p))
;=> [{:name "Alice" :age 25} {:name "Alice" :age 30}]
```

## Type constructors: vector-of, tuple, map-of

Type constructors constrain entire collections at once. They walk the collection at bind time — consistent with how `map`/`filter`/`reduce` work.

### vector-of

`(vector-of type)` produces a composite domain where every element must match the type. Use with `narrow` to constrain a vector:

```clojure
(query (let [v [(one-of 1 "a") (one-of 2 "b") (one-of 3 "c")]]
         (narrow v (vector-of integer))
         v))
;=> [[1 2 3]]
```

Works with named domains too:

```clojure
(query (def named {:name string})
       (let [people [{:name (one-of "Alice" 42)} {:name (one-of "Bob" 99)}]]
         (narrow people (vector-of named))
         people))
;=> [[{:name "Alice"} {:name "Bob"}]]
```

In `def` schemas:

```clojure
(query (def int-vec (vector-of integer))
       (let [v [(one-of 1 "x") (one-of 2 "y")]]
         (narrow v int-vec)
         v))
;=> [[1 2]]
```

### tuple

`(tuple [type1 type2 ...])` produces a composite domain with per-position types. Use with `narrow`:

```clojure
(query (let [v [(one-of 42 "x") (one-of "hello" 99) (one-of true 0)]]
         (narrow v (tuple [integer string boolean]))
         v))
;=> [[42 "hello" true]]
```

In `def` schemas:

```clojure
(query (def point (tuple [integer integer]))
       (let [p [(one-of 3 "x") (one-of 4 "y")]]
         (narrow p point)
         p))
;=> [[3 4]]
```

### map-of

`(map-of key-type val-type)` produces a composite domain constraining all keys and values. Use with `narrow`:

```clojure
(query (let [m {:a (one-of 1 "x") :b (one-of 2 "y")}]
         (narrow m (map-of keyword integer))
         m))
;=> [{:a 1 :b 2}]
```

In `def` schemas:

```clojure
(query (def scores (map-of keyword integer))
       (let [s {:x (one-of 10 "a") :y (one-of 20 "b")}]
         (narrow s scores)
         s))
;=> [{:x 10 :y 20}]
```

### Nesting type constructors

Type constructors compose naturally. A vector of named domains, or a domain containing a `vector-of` field:

```clojure
(query (def student (and {:name string}
                         {:scores (vector-of integer)}))
       (let [s {:name "Alice" :scores [(one-of 90 "x") (one-of 80 "y")]}]
         (narrow s student)
         [(get s :name) (get s :scores)]))
;=> [["Alice" [90 80]]]
```

## Callable domains

Any bound domain value — type constructors, `def`-named schemas, scalar domains — can be **called as a function**. Calling a domain is equivalent to `narrow`: `(intvec v)` means `(narrow v intvec)`.

### Expression position

In expression position, calling a domain constrains the argument and returns the environment (like `narrow`):

```clojure
(query (let [intvec (vector-of integer)
             v [(one-of 1 "a") (one-of 2 "b")]]
         (intvec v) v))
;=> [[1 2]]
```

Works with all domain kinds — type constructors, struct schemas, scalar domains:

```clojure
;; Struct domain
(query (def person {:name string :age integer})
       (let [p {:name (one-of "Alice" 42) :age (one-of 30 "old")}]
         (person p) p))
;=> [{:name "Alice" :age 30}]

;; Tuple domain
(query (let [pair (tuple [integer string])
             v [(one-of 42 "x") (one-of "hello" 99)]]
         (pair v) v))
;=> [[42 "hello"]]

;; map-of domain
(query (let [scores (map-of keyword integer)
             m {:a (one-of 1 "x") :b (one-of 2 "y")}]
         (scores m) m))
;=> [{:a 1 :b 2}]
```

### Pattern position

Callable domains work in **pattern position** too — `(let [(intvec x) v] x)` destructures `v` and constrains it to match the domain:

```clojure
(query (let [intvec (vector-of integer)
             v [(one-of 1 "a") (one-of 2 "b")]]
         (let [(intvec x) v] x)))
;=> [[1 2]]
```

This composes with all pattern features — inner destructuring, `as`, nested patterns:

```clojure
;; Inner destructuring: extract elements after domain constraint
(query (let [intvec (vector-of integer)
             v [(one-of 1 "a") (one-of 2 "b")]]
         (let [(intvec [a b]) v] [a b])))
;=> [[1 2]]

;; Struct domain in pattern with map destructuring
(query (def person {:name string :age integer})
       (let [p {:name (one-of "Alice" 42) :age (one-of 30 "old")}]
         (let [(person {:name n :age a}) p] [n a])))
;=> [["Alice" 30]]

;; Scalar domain as pattern guard
(query (def small (between 1 10))
       (let [v (one-of 5 20 "x")]
         (let [(small x) v] x)))
;=> [5]
```

Callable domains generalize the built-in type guard syntax. Where `(integer x)` filters to integers, `(my-domain x)` filters to any named domain — giving you the same concise pattern for user-defined types.

## Higher-order functions

`map`, `filter`, and `reduce` operate on vectors. They unroll at bind time since collection sizes are known.

```clojure
;; Map
(query (let [v [1 2 3]
             doubled (map (fn [x] (* x 2)) v)]
         doubled))
;=> [[2 4 6]]

;; Filter
(query (let [v [1 2 3 4 5]
             evens (filter even v)]
         evens))
;=> [[2 4]]

;; Reduce
(query (let [v [1 2 3 4 5]
             total (reduce + 0 v)]
         total))
;=> [15]
```

What makes HOFs interesting in mufl is how they compose with non-ground values and constraints:

```clojure
;; Map over uncertain elements — each result inherits the constraint
(query (let [v [(one-of 1 2 3) (one-of 4 5 6)]
             doubled (map (fn [x] (* x 2)) v)]
         (and (> (nth doubled 0) 3)
              doubled)))
;=> [[4 8] [4 10] [4 12] [6 8] [6 10] [6 12]]
```

Filter with a custom constraint function:

```clojure
(query (do (defn big [x] (> x 3))
           (let [v [1 2 3 4 5]
                 bigs (filter big v)]
             bigs)))
;=> [[4 5]]
```

Reduce computing a value that feeds into a constraint:

```clojure
;; Find vectors where the sum exceeds 10
(query (let [v [(one-of 1 5) (one-of 2 6) (one-of 3 7)]
             total (reduce + 0 v)]
         (and (> total 10)
              [v total])))
;=> [[[5 2 7] 14] [[5 6 3] 14] [[1 6 7] 14] [[5 6 7] 18]]
```

Chaining HOFs together:

```clojure
;; Sum of doubled values
(query (let [v [1 2 3]
             doubled (map (fn [x] (* x 2)) v)
             total (reduce + 0 doubled)]
         total))
;=> [12]
```

## Arithmetic

Arithmetic expressions create *derived* domains. If the inputs aren't ground, the result is the set of all possible outputs.

```clojure
(query (let [x (one-of 1 2 3)
             s (+ x 10)]
         s))
;=> [11 12 13]
```

Constraints on derived values propagate backward to the inputs:

```clojure
(query (let [x (one-of 1 2 3 4 5)
             y (one-of 1 2 3 4 5)
             s (+ x y)]
         (and (< x y)
              (= s 6)
              [x y s])))
;=> [[2 4 6] [1 5 6]]
```

`mod` and `quot` work the same way:

```clojure
;; Multiples of 3 up to 20
(query (let [x (between 1 20)]
         (and (= (mod x 3) 0) x)))
;=> [3 6 9 12 15 18]
```

`abs` computes absolute value. It propagates bidirectionally — constraining the result narrows both positive and negative possibilities of the input:

```clojure
(query (abs -5))
;=> [5]

(query (let [x (one-of -3 -2 -1 0 1 2 3)]
         (abs x)))
;=> [0 1 2 3]

;; Constraining (abs x) narrows x to both signs
(query (let [x (one-of -3 -2 -1 0 1 2 3)]
         (and (= (abs x) 2) x)))
;=> [-2 2]
```

`min` and `max` return the smaller or larger of two values:

```clojure
(query (min 3 2))
;=> [2]

(query (max 3 5))
;=> [5]
```

## Distinct

`distinct` says all variables in a vector must take different values.

```clojure
(query (let [a (one-of 1 2 3)
             b (one-of 1 2 3)
             c (one-of 1 2 3)]
         (and (distinct [a b c])
              [a b c])))
;=> [[3 2 1] [2 3 1] [3 1 2] [1 3 2] [2 1 3] [1 2 3]]
```

All 6 permutations — no duplicates within any solution.

## A classic: Pythagorean triples

Putting it all together — find all Pythagorean triples where a, b, c ≤ 15:

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

No loops, no generate-and-test. You declare what a Pythagorean triple *is*, and the constraint engine finds them all.

## Inspecting solutions (query+)

`query` returns the evaluated result expression for each solution. Sometimes you want to see *all* the variable bindings — not just the expression you returned. `query+` does exactly this: it returns a vector of maps, one per solution, containing every user-defined binding.

> **Lazy queries:** When you don't need *all* solutions — or the domain is too large to enumerate eagerly — see [Lazy Enumeration](lazy-enumeration.md) for `query-lazy` (lazy seq of solutions) and `query1` (one solution at a time with a continuation).

```clojure
(query+ (let [x (one-of 1 2 3)
              y (one-of 1 2 3)]
          (and (< x y) [x y])))
;=> [{x 1, y 2} {x 1, y 3} {x 2, y 3}]
```

Compare with `query`, which only returns the result expression:

```clojure
(query (let [x (one-of 1 2 3)
             y (one-of 1 2 3)]
         (and (< x y) [x y])))
;=> [[1 2] [1 3] [2 3]]
```

`query+` is useful for inspection — you can see how every variable was bound in each solution, not just the ones you chose to return. Function definitions (`fn`, `defn`) are excluded — only value bindings appear.

```clojure
(query+ (defn positive [x] (> x 0))
        (let [n (one-of -2 -1 0 1 2 3)]
          (positive n)
          n))
;=> [{n 1} {n 2} {n 3}]
```

Like `query`, `query+` accepts multiple body forms (implicit `do`).

## Summary

| Concept | mufl |
|---|---|
| Variable | A domain (set of possible values) |
| Constraint | Narrows domains — never adds values, only removes |
| `and` | Apply all constraints; return last expression |
| `or` | Union of domains across branches |
| `if` / `cond` | Branching — eager when decidable, deferred when not |
| `fn` | Bidirectional function — constructor in expression position, destructor in pattern position |
| `defn` | Sugar for `(def name (fn ...))` — names a function |
| `def` | Named value binding — gives a name to any value |
| `narrow` | Constrain a value against a domain — scalar, composite, or structural |
| Callable domains | `(my-domain x)` ≡ `(narrow x my-domain)` — works in both expression and pattern position |
| `vector-of` | Constrain all vector elements to a type |
| `tuple` | Per-position type constraints on a vector |
| `map-of` | Constrain all map keys and values |
| `ks` | Keyword-symbol map destructuring (pattern only) |
| `as` | Bind whole value + inner destructuring (pattern only) |
| `abs` | Absolute value — propagates bidirectionally |
| `min` / `max` | Two-argument min/max on values |
| `count` | Number of elements in a vector or map |
| `drop` | Drop first n elements from a vector |
| `dissoc` | Remove keys from a map |
| Predicates | `even`, `odd`, `pos`, `neg`, `zero` |
| Type constraints | `string`, `integer`, `number`, `keyword`, `boolean` (also usable as type domains) |
| `query` | Bind + solve → all solutions (implicit `do` for multiple body forms) |
| `query+` | Like `query`, but returns full environments as maps |
| `query-lazy` | Lazy seq of solutions — compute only what you `take` ([details](lazy-enumeration.md)) |
| `query1` | One solution + continuation — step through solutions manually ([details](lazy-enumeration.md)) |

The mental model: you're not computing a result step by step — you're *describing* the space of valid answers, and the system narrows it down for you.

---

For a deeper look at how domains, constraint propagation, and the internal algebra work under the hood, see [Domain Algebra](domain-algebra.md).
