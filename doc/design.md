# mufl — Unified Environment Tree Language

## Vision

mufl fuses FL's constraint-logic semantics with immucode's immutable environment tree architecture. The result: a language where **one tree** is the single source of truth for names, domains, constraints, and forms.

## Core Principle: Everything Is a Domain

A **domain** is the set of possible values a name can take.

- `(single 1)` — exactly one value. This **is** the value `1`. No separate `:value` field needed.
- `(union [1 2 3])` — could be 1, 2, or 3. Not yet determined.
- `integer` — some integer. Open domain.
- `any` — we know nothing yet.
- `void` — empty domain. Contradiction.

The question "do we have a concrete value?" is just `(singleton? domain)` — a query, not a structural distinction. There is no `:value` / `:type` duality. There is only `:domain`.

## Core Principle: Constraints Are Tree Nodes

A constraint like `(< x y)` is an expression. Expressions live in the tree. Therefore constraints live in the tree — as child nodes of the scope where they were written.

A constraint node references the domain-nodes it constrains (via paths). Domain-nodes track which constraints reference them (reverse index). This is the dependency graph that drives propagation.

## Core Principle: Bind Propagates Eagerly

Every `bind` operation leaves the tree at **fixpoint** — all domains are maximally narrowed given the current constraints. There is no deferred work. What you see in the tree after `bind` is the truth.

Propagation at bind time is **deterministic** — it narrows domains but does not fork/search. When multiple assignments are possible (non-singleton domains remain), the tree faithfully represents that uncertainty. Search (forking, enumeration) is a separate phase invoked explicitly.

## Architecture

```
User expression
      │
      ▼
  ┌────────┐     looks up forms/ops   ┌─────────────────────┐
  │  bind  │ ◄────────────────────────►│  Environment Tree   │
  │        │     in the tree           │                     │
  └────┬───┘                           │  Domains + Links    │
       │                               │  Constraints        │
       │ returns new tree              │  Forms (:bind fns)  │
       │ at fixpoint                   │  Reverse index      │
       ▼                               └─────────────────────┘
  Tree with narrowed domains
       │
       │ (when concrete answers needed)
       ▼
  ┌────────────┐
  │  search    │  grounded? + split DFS
  │            │  binary bisection on domains
  └────────────┘
       │
       ▼
    results
```

## Node Structure

Every node in the tree can carry any combination of these fields:

```clojure
{;; === Tree navigation (from immucode.tree) ===
 ::tree/node    {child-name → child-node, ...}
 ::tree/parent  <parent-node>
 ::tree/name    <this-node-name>

 ;; === The content — one field, not two ===
 :domain        <domain>          ;; (single 1), (union [1 2 3]), integer, any, void, ...

 ;; === Navigation ===
 :link          [path to target]  ;; this node IS that node (union-find)

 ;; === Constraint info (when this node IS a constraint) ===
 :constraint    <op>              ;; :<, :=, :!=, :alldiff, etc.
 :refs          [<path> ...]      ;; domain-nodes this constraint references

 ;; === Reverse index (when this node HAS a domain) ===
 :watched-by    #{<path> ...}     ;; constraint-nodes that reference this domain

 ;; === Form semantics (what (this-name ...) means in head position) ===
 :bind          (fn [env args])   ;; how to bind sub-expressions of this form

 ;; === Destructuring (what (this-name ...) means in pattern position) ===
 :destruct      (fn [env args value])

 ;; === Flags ===
 :local         true              ;; locally scoped (not from base env)
 :primitive     true              ;; marks base-env nodes
 }
```

### Key simplifications vs. old design

- **No `:value` field** — `{:domain (single 1)}` IS the value 1
- **No `:alias` field** — a logic variable is a node with a non-singleton domain
- **No `:logic?` flag** — a node is "logic" if its domain is non-singleton
- **No `:type` separate from `:domain`** — the domain IS the type
- **No separate constraint accumulator** — constraints live in the tree
- **`:bind` not `:analyze`** — we're growing the tree, not producing a separate result map

## The `bind` Function

The heart of mufl. Takes `[env expr]`, returns a new tree.

```clojure
(bind env expr) → env'
```

The returned tree has been:
1. **Enriched** — new nodes created for the expression
2. **Constrained** — constraint nodes added
3. **Propagated** — all domains narrowed to fixpoint

### Dispatch

`bind` dispatches on the shape of `expr`:

- **literal** (number, string, boolean, nil) → node with `{:domain (single <value>)}`
- **symbol** → `tree/find` upward. Creates `:link` to found node.
- **seq (list)** → look up head symbol in tree, call its `:bind` function
- **vector** → delegate to vector form's `:bind`
- **map** → delegate to map form's `:bind`

```clojure
(defn bind
  ([env expr]
   (cond
     (number? expr)  (assoc env :domain (single expr))
     (string? expr)  (assoc env :domain (single expr))
     (boolean? expr) (assoc env :domain (single expr))
     (nil? expr)     (assoc env :domain (single nil))
     (keyword? expr) (assoc env :domain (single expr))

     (symbol? expr)
     (if-let [found (tree/find env [expr])]
       (assoc env :link (tree/position found))
       (bind env (list 'external expr)))

     (seq? expr)
     (let [[head & args] expr
           node (tree/find env [head])]
       (if-let [f (:bind node)]
         (f env args)
         (throw (ex-info (str "No :bind for: " head) {:head head}))))

     (vector? expr) (bind env (cons 'vector expr))
     (map? expr)    (bind env (cons 'hash-map expr))

     :else (throw (ex-info "Cannot bind" {:expr expr}))))

  ([env sym expr]
   ;; Named binding: create child node, bind expr there
   (let [path [sym]]
     (-> (tree/ensure-path env path)
         (tree/upd path #(bind % expr)))))

  ([env sym expr & more]
   ;; Sequential bindings with optional return
   (let [[pairs return] (pairs&return (list* sym expr more))
         bound (reduce (fn [e [s v]] (bind e s v)) env pairs)]
     (if return (bind bound return) bound))))
```

### The Propagation Cycle

When a constraint node is added to the tree, propagation runs:

```
1. Constraint C is created, referencing domain-nodes [A, B, ...]
2. Each referenced node gets C added to its :watched-by set
3. C's narrowing function runs → may narrow domains of A, B, ...
4. For each node whose domain changed:
   a. If domain became void → contradiction (error or branch death)
   b. For each constraint C' in that node's :watched-by (except C):
      re-run C's narrowing
5. Repeat until no domain changes (fixpoint)
```

This is **arc consistency** embedded in the tree.

```clojure
(defn propagate
  "Run all pending constraints to fixpoint.
   Returns updated env, or nil on contradiction."
  [env pending]
  (if (empty? pending)
    env
    (let [[c & rest] pending
          c-node (tree/at env c)
          result (narrow c-node env)]
      (cond
        ;; Contradiction
        (nil? result)
        nil

        ;; No change
        (= (:env result) env)
        (recur env rest)

        ;; Progress — enqueue watchers of changed nodes
        :else
        (let [new-pending (->> (:changed result)
                               (mapcat #(:watched-by (tree/at (:env result) %)))
                               (remove (set rest))
                               (into rest))]
          (recur (:env result) new-pending))))))
```

Each constraint type (`:< `:= `:!= `:alldiff` etc.) has a `narrow` function that examines the current domains of its referenced nodes and returns either:
- `nil` — contradiction (empty domain)
- `{:env env :changed []}` — no narrowing possible
- `{:env env' :changed [path1 path2 ...]}` — domains narrowed, these nodes changed

### Constraint narrowing examples

**`(<)` constraint** referencing nodes at paths `a` and `b`:
```clojure
;; a < b
;; a's domain can't contain anything >= max(b's domain)
;; b's domain can't contain anything <= min(a's domain)
(defn narrow-lt [env a-path b-path]
  (let [a-dom (domain-of env a-path)
        b-dom (domain-of env b-path)
        a-max (domain-max b-dom)  ;; a must be < max of b
        b-min (domain-min a-dom)  ;; b must be > min of a
        a-new (domain-below a-dom a-max)   ;; remove values >= a-max
        b-new (domain-above b-dom b-min)]  ;; remove values <= b-min
    (apply-narrowings env [[a-path a-new] [b-path b-new]])))
```

**`(=)` constraint** (unification):
```clojure
;; a = b → their domains must intersect
(defn narrow-eq [env a-path b-path]
  (let [a-dom (domain-of env a-path)
        b-dom (domain-of env b-path)
        shared (domain-intersect a-dom b-dom)]
    (if (domain-void? shared)
      nil  ;; contradiction
      (apply-narrowings env [[a-path shared] [b-path shared]]))))
```

## Domain Algebra

The domain system extends immucode's type system with operations needed for constraint propagation:

```clojure
;; Construction
(single v)              ;; exactly one value
(union [v1 v2 ...])     ;; finite enumeration
(between lo hi)         ;; integer range (for FD-style)
integer, number, string, keyword, ...  ;; open primitive domains
any                     ;; everything
void                    ;; nothing (contradiction)

;; Singleton check — "is this a value?"
(singleton? (single 1))      ;; → true, value is 1
(singleton? (union [1 2 3])) ;; → false
(singleton? integer)         ;; → false

;; Algebra
(intersect d1 d2)   ;; narrowing — what's in both
(unite d1 d2)       ;; widening — what's in either
(complement d)      ;; negation
(subtract d1 d2)    ;; d1 minus d2

;; For ordered domains (constraint propagation)
(domain-min d)      ;; smallest value
(domain-max d)      ;; largest value
(domain-below d v)  ;; values in d that are < v
(domain-above d v)  ;; values in d that are > v

;; Comparison (from immucode.types)
(compare d1 d2) → :equal | :bigger | :smaller | :distinct | :overlap
```

## Forms as Tree Nodes

Each form becomes a node in the base environment with a `:bind` function.

### `one-of`

```clojure
{:bind
 (fn [env args]
   (assoc env :domain (union args)))}
```

The simplest form. No constraints needed — it just sets the domain.

### `let`

```clojure
{:bind
 (fn [env [bindings & body]]
   (let [pairs (partition 2 bindings)
         env' (reduce (fn [e [sym expr]]
                        (bind e sym expr))
                      env pairs)]
     ;; bind body in the extended env
     (bind env' (if (= 1 (count body))
                  (first body)
                  (cons 'do body)))))}
```

### `and`

In the constraint world, `and` means "all of these must hold." Each sub-expression is either:
- A constraint (narrows domains, returns nothing)
- A value expression (the last one becomes the return)

```clojure
{:bind
 (fn [env args]
   (reduce (fn [e arg] (bind e arg))
           env args))}
```

The propagation happens inside each `bind` call. By the time `and` returns, all constraints have been applied and all domains are at fixpoint.

### `<`, `=`, `>`, etc. (relational constraints)

```clojure
;; (<) node
{:bind
 (fn [env [a-expr b-expr]]
   (let [a (resolve-to-node env a-expr)
         b (resolve-to-node env b-expr)]
     (if (and (singleton? (:domain a)) (singleton? (:domain b)))
       ;; Both concrete — just check
       (if (< (singleton-value a) (singleton-value b))
         (assoc env :domain (single true))   ;; or: return a (FL style)
         (assoc env :domain void))           ;; contradiction
       ;; At least one non-singleton — create constraint, propagate
       (-> env
           (add-constraint :< [(path-of a) (path-of b)])
           (propagate-from-new-constraint)))))}
```

### `+`, `-`, `*` (arithmetic)

Arithmetic on domains creates a **derived node** whose domain is computed from its operands:

```clojure
;; (+) node
{:bind
 (fn [env [a-expr b-expr]]
   (let [a (resolve-to-node env a-expr)
         b (resolve-to-node env b-expr)]
     (if (and (singleton? (:domain a)) (singleton? (:domain b)))
       ;; Both concrete — compute
       (assoc env :domain (single (+ (singleton-value a) (singleton-value b))))
       ;; At least one non-singleton — derived domain + constraint
       (let [result-domain (domain-add (:domain a) (:domain b))]
         (-> env
             (assoc :domain result-domain)
             (add-constraint :+ [(path-of a) (path-of b) (path-of env)])
             (propagate-from-new-constraint))))))}
```

The `:+` constraint has inverse reasoning: if the result is narrowed, the operands can be narrowed too (and vice versa).

## Name Resolution

Name resolution uses `tree/find` — walk upward through ancestors looking for the symbol. This is exactly what immucode's `bubfind` does and what FL's `scope-get` does.

```clojure
(defn resolve-name [env sym]
  (tree/find env [sym]))
```

The found node's `:domain`, `:link`, or `:bind` field tells us what kind of thing it is.

## Example Walkthrough

```clojure
(let [x (one-of 1 2 3 4 5)
      y (one-of 1 2 3 4 5)]
  (and (< x y)
       (= (+ x y) 6)
       [x y]))
```

### Step 1: bind `x`

`(one-of 1 2 3 4 5)` → node at `[:x]` with `{:domain (union [1 2 3 4 5])}`

Tree state:
```
[scope]
  └─ x  {:domain (union [1 2 3 4 5])}
```

### Step 2: bind `y`

Same. Tree state:
```
[scope]
  ├─ x  {:domain (union [1 2 3 4 5])}
  └─ y  {:domain (union [1 2 3 4 5])}
```

### Step 3: bind `(< x y)`

Creates constraint node, then propagates:
- `x < y` → x can't be 5 (nothing bigger), y can't be 1 (nothing smaller)
- x narrows to `{1 2 3 4}`, y narrows to `{2 3 4 5}`
- No other constraints yet, so fixpoint reached.

```
[scope]
  ├─ x   {:domain (union [1 2 3 4]), :watched-by #{c0}}
  ├─ y   {:domain (union [2 3 4 5]), :watched-by #{c0}}
  └─ c0  {:constraint :<, :refs [x y]}
```

### Step 4: bind `(= (+ x y) 6)`

First, `(+ x y)` creates a derived node with domain `{3 4 5 6 7 8 9}` (all pairwise sums where x<y).

Then `(= ... 6)` unifies the sum with `(single 6)`:
- Sum domain narrows to `{6}`
- This triggers the `+` constraint: if `x + y = 6`, with `x ∈ {1,2,3,4}` and `y ∈ {2,3,4,5}`:
  - For each x value, y must be `6 - x`. So valid pairs: (1,5), (2,4), (3,3), (4,2)
  - But we also need `x < y`: (1,5), (2,4)
  - x narrows to `{1, 2}`, y narrows to `{4, 5}`
- This might trigger c0 again, but c0 checks `x < y` and `{1,2}` < `{4,5}` is always true, so no further narrowing.

```
[scope]
  ├─ x    {:domain (union [1 2]), :watched-by #{c0 c2}}
  ├─ y    {:domain (union [4 5]), :watched-by #{c0 c2}}
  ├─ c0   {:constraint :<, :refs [x y]}
  ├─ sum  {:domain (single 6), :watched-by #{c1}}
  ├─ c1   {:constraint :=, :refs [sum six]}
  ├─ c2   {:constraint :+, :refs [x y sum]}
  └─ six  {:domain (single 6)}
```

### Step 5: bind `[x y]`

The return expression. Creates a vector node whose children link to x and y.

### Step 6: search

At this point, the tree is at fixpoint but x and y still have non-singleton domains. To get concrete answers, `search` explores via DFS with binary domain bisection:

1. Check if tree is grounded (all value nodes have singleton domains) → no
2. Pick variable with smallest domain (fail-first heuristic) → x has domain {1 2}
3. Call `dom/split` to bisect x's domain → {1} and {2}
4. Fork left: set x={1}, propagate → y must be 5 → validate-ground confirms → yield [1 5]
5. Fork right: set x={2}, propagate → y must be 4 → validate-ground confirms → yield [2 4]

Result: `([1 5] [2 4])`

## Search Phase

Search is the operation that enumerates concrete solutions from a propagated tree via depth-first exploration with binary domain bisection:

1. **Base case: grounded?** — Check if all value-bearing nodes at the scope path have singleton domains. If yes, run `validate-ground` (re-runs all constraints to catch stale propagation), then yield the solution.
2. **Recursive case: split** — Pick variable with smallest domain (fail-first heuristic). Call `dom/split` to bisect the domain into two halves. For each half:
   a. Narrow the variable's domain to the half
   b. Propagate to fixpoint
   c. If contradiction → prune this branch
   d. Otherwise → recurse

This is simpler than traditional CSP search: no explicit choice-points or backtracking state. Each recursion is a pure function call over an immutable tree. The DFS queue manages the exploration frontier.

The key point: **search is separate from bind**. Bind is deterministic propagation. Search is nondeterministic exploration. Different search strategies (DFS, BFS, best-first) differ only in queue management — they all use the same `grounded?` and `split` primitives.

## What About `if`?

`if` is where immucode's type narrowing and FL's conditional constraints converge beautifully:

```clojure
(if (< x 3) then else)
```

In the then-branch: x's domain is intersected with `{... values < 3 ...}`.
In the else-branch: x's domain is intersected with the complement — `{... values >= 3 ...}`.

This is exactly what immucode already does with type predicates (`(if (number? x) ...)`), but generalized to arbitrary domain constraints. The `if` node:

1. Evaluates the test expression
2. Creates a **branch scope** for `then` where the test's constraint is applied
3. Creates a **branch scope** for `else` where the test's constraint is negated
4. Each branch propagates independently

If one branch's domain becomes `void`, that branch is impossible — potentially detectable at bind time.

## Destructuring

Destructuring in patterns uses `:destruct` functions on tree nodes. When a symbol appears in head position of a pattern, `tree/find` looks it up and calls `:destruct`.

The `:destruct` function receives the pattern arguments and the value being destructured, and returns tree modifications: new bindings and new constraints.

Example: `(ks a b)` in pattern position destructures a map:

```clojure
{:destruct
 (fn [env args value]
   ;; (ks a b) against value → bind a to (:a value), b to (:b value)
   (reduce (fn [e sym]
             (bind e sym (list :keyword-get value (keyword sym))))
           env args))}
```

## Domains as First-Class Nodes

A domain definition creates a node carrying both `:bind` and `:destruct`:

```clojure
(defdomain Person {:name string :age integer})
```

Creates node at `[:Person]`:

```clojure
{:domain {:name string, :age integer}

 ;; As constraint: (Person p) narrows p's keys
 :bind
 (fn [env [target-expr]]
   (let [target (resolve-to-node env target-expr)]
     ;; intersect each key's domain
     ...))

 ;; As destructuring: (let [(Person {:name n}) p] ...)
 :destruct
 (fn [env args value]
   ;; destructure + domain constraints
   ...)}
```

## Tree Operations

Reuses immucode.tree's core ideas:

```clojure
(cd tree [path])          ;; navigate to child
(parent tree)             ;; go up
(root tree)               ;; go to root
(find tree [path])        ;; walk upward (lexical scoping)
(put tree [path] k v)     ;; set a field
(ensure-path tree [path]) ;; create nodes along path
(position tree)           ;; current path from root
(at tree [path])          ;; navigate from root to path
(children tree)           ;; child nodes
```

## Implementation Roadmap

### Phase 1: Foundation
1. **`mufl.domain`** — Domain algebra: single, union, intersect, complement, void, singleton?, compare, min/max, above/below
2. **`mufl.tree`** — Tree navigation: cd, parent, root, find, put, ensure-path, position, at
3. **`mufl.bind`** — Core bind + propagation engine
4. **`mufl.env`** — Base environment: one-of, let, and, =, <, >, +, -
5. **`mufl.search`** — Search strategies: grounded?, split, validate-ground, DFS/BFS
6. **`mufl.show`** — Tree → mufl code serialization (extract-value, extract-bindings, print-solution)
7. **`mufl.core`** — `query` entry point (bind + search)
8. **Tests** — End-to-end: `(query (let [x (one-of 1 2 3)] (and (> x 1) x)))` → `(2 3)`

### Phase 2: Full Form Coverage
9. All relational constraints: !=, <=, >=, distinct
10. All arithmetic: *, /, mod
11. or, if, when, cond
12. Collections: vector, hash-map, keyword access
13. Destructuring: ks, as, or, &, quote, list
14. HOFs: map, reduce
15. defc (function definitions as tree nodes)

### Phase 3: Domains
16. defdomain macro
17. Domain-as-constraint
18. Domain-as-destructuring
19. Domain composition

### Phase 4: Optimization & Types
20. maximize / minimize
21. Type transitions for builtins
22. Cross-scope type propagation
