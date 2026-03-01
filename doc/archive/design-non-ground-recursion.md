> **📦 ARCHIVED** — Moved to `doc/archive/` on 2026-02-28. Recursion with ground arguments now works
> via multi-branch `fn` with pattern matching (tested in `recursion_test.clj`). The approaches described
> here (search-time unfolding, bind-time enumeration, annotation-based opt-in) were not implemented.
> Non-ground recursive args remain a limitation.

# Design: Non-Ground Recursion

## Current State

Recursive functions in mufl work when arguments are **ground** (singleton domain) at each call site. This is because `if` eagerly resolves which branch to take when the test is decidable:

```clojure
;; Works: n is always ground at each call
(query (let [fact (fn [n]
                    (if (= n 0) 1 (* n (fact (- n 1)))))]
         (fact 5)))
;=> [120]
```

The mechanism:
1. `(fact 5)` → `n` is `{5}`, test `(= n 0)` → only else-branch satisfiable → eager
2. `(fact 4)` → `n` is `{4}`, same reasoning → eager
3. ... down to `(fact 0)` → `n` is `{0}`, test `(= n 0)` → only then-branch → returns 1
4. Each level multiplies and returns a singleton

**Non-ground args fail** because `if` can't determine which branch to take:

```clojure
;; Fails: n is non-ground, if stores a :fork, recursion can't unfold
(query (let [fact (fn [n]
                    (if (= n 0) 1 (* n (fact (- n 1)))))]
         (let [x (between 1 5)]
           (fact x))))
;; → recursion depth exceeded (or stuck fork)
```

When `n` is `{1, 2, 3, 4, 5}`:
- Test `(= n 0)`: both branches satisfiable (then if n=0, else if n≠0)
- `if` stores a `:fork` node — deferred to search time
- But the recursive call `(fact (- n 1))` in the else branch tries to bind **now** (at bind time)
- `(- n 1)` produces `{0, 1, 2, 3, 4}` — still non-ground
- Next recursive call sees `{-1, 0, 1, 2, 3}` — still non-ground
- Infinite regression → depth limit hit

## The Core Problem

The fundamental issue is that **bind time and search time have different capabilities**:

| | Bind time | Search time |
|---|---|---|
| Recursion | Unfolds eagerly (needs ground args) | Can enumerate fork branches |
| Non-ground domains | Propagates constraints | Enumerates values |
| Fork nodes | Created when if/cond is undecidable | Expanded into concrete branches |

Recursion needs ground args → ground args come from enumeration → enumeration happens at search time → but recursion happens at bind time. There's a phase mismatch.

## Approach 1: Search-time Recursive Unfolding

**Idea**: When search encounters a fork node that contains a recursive call with a non-ground arg, enumerate the arg's domain and unfold the recursion for each value.

**How it would work**:
1. During bind, detect that a recursive call has non-ground args
2. Instead of trying to unfold, store a "deferred recursion" marker: `{:deferred-call {:fn fact :args [n] :arg-paths [...]}}`
3. During search, when labeling reaches a deferred recursion:
   - The arg variable has been ground by labeling
   - Re-evaluate the recursive call with the now-ground arg
   - This produces a concrete result that can be linked

**Challenges**:
- **Detecting deferred recursion**: Need to distinguish "this fork is from a recursive if" vs "this fork is from user's if on unrelated vars"
- **Re-evaluation context**: The recursive call needs the full environment at the point where it was deferred. This means storing a snapshot or a thunk.
- **Nested scopes**: Recursive calls create `call1234` gensym scopes. The search engine currently only labels workspace-scope variables, not variables inside call scopes.
- **Composability**: What if the recursion result feeds into another constraint? The constraint graph would need to be rebuilt after each deferred call resolves.

**Complexity**: High. Requires fundamental changes to the bind/search boundary.

## Approach 2: Bind-time Enumeration (Unroll per value)

**Idea**: When a recursive function is called with a finite non-ground arg, enumerate the arg domain at bind time and create a fork with one branch per value.

**How it would work**:
```clojure
;; User writes:
(fact x)  ;; x ∈ {1, 2, 3, 4, 5}

;; System expands to (conceptually):
(cond
  (= x 1) (fact 1)   ;; ground → unfolds recursively
  (= x 2) (fact 2)   ;; ground → unfolds recursively
  (= x 3) (fact 3)   ;; ground → unfolds recursively
  (= x 4) (fact 4)
  (= x 5) (fact 5))
```

Each branch has a ground arg, so recursion unfolds normally. The cond creates a fork that search handles.

**Implementation**:
1. In the `:mufl-fn` handler in `bind`, after evaluating arg expressions:
   - Check if any arg has a non-singleton finite domain
   - If so, and the function is recursive (references itself in body), expand into a fork
2. The fork has one branch per domain value of the non-ground arg
3. Each branch constrains the arg to a single value + calls the function

**Advantages**:
- No changes to search engine — uses existing fork mechanism
- Recursion still unfolds at bind time with ground args
- The fork branches are pruned by search like any other fork

**Challenges**:
- **Domain size explosion**: `(fact (between 1 100))` creates 100 branches, each with its own recursion tree. This could be O(n × depth) in tree nodes.
- **Multiple non-ground args**: `(f x y)` where both `x ∈ {1..5}` and `y ∈ {1..5}` → 25 branches. Cartesian product.
- **Non-recursive functions**: This expansion is unnecessary for non-recursive functions — they handle non-ground args fine through constraint propagation.
- **Detecting recursion**: How to know if a function is recursive? Check if `head` appears in `body` (simple syntactic check). But mutual recursion is harder.
- **Infinite domains**: `(fact x)` where x has domain `integer` can't enumerate. Need to restrict to finite domains only.

**Complexity**: Medium. Contained within the fn-call handler.

## Approach 3: Annotation-based Opt-in

**Idea**: Let the user explicitly mark functions as enumerable on non-ground args.

```clojure
;; Option A: annotation on the function
(let [fact (fn ^:enum [n]
              (if (= n 0) 1 (* n (fact (- n 1)))))]
  (let [x (between 1 5)]
    (fact x)))

;; Option B: wrapper form
(let [x (between 1 5)]
  (enumerate-call fact x))

;; Option C: on the variable
(let [x ^:enumerate (between 1 5)]
  (fact x))
```

**Advantages**:
- User controls when enumeration happens
- No accidental performance traps
- Clear semantics: "I know this should be expanded per value"

**Disadvantages**:
- More syntax to learn
- User must understand when it's needed

## Approach 4: Lazy/Tabled Recursion

**Idea**: Memoize recursive function results. When `(fact x)` is called with non-ground x, compute `fact` for each value in x's domain lazily, caching results.

Similar to tabled resolution in Prolog (XSB, SWI tabling). The function is evaluated once per distinct input value, and results are cached.

**This is elegant but complex**: requires a memoization layer that interacts with the tree structure and constraint propagation. Probably overkill for the current system.

## Recommendation

**Approach 2 (bind-time enumeration) with Approach 3 (opt-in annotation)** seems like the sweet spot:

1. **Don't auto-enumerate** — it's too easy to accidentally create huge expansion trees
2. **Add `^:enum` metadata** on the function definition to opt in
3. When a `^:enum` function is called with non-ground finite-domain args:
   - Expand into a cond fork at bind time
   - Each branch grounds the args and calls normally
4. **Guard rail**: if the Cartesian product of non-ground arg domains exceeds a threshold (e.g., 1000), throw an error suggesting the user narrow domains first

**Implementation sketch** (changes to `bind.clj`, fn-call handler):

```clojure
;; In the :mufl-fn branch of bind:
(if (and (:enum (meta (:mufl-fn node)))
         (some non-ground-finite-arg? evaluated-args))
  ;; Expand into cond branches
  (let [non-ground-arg-idx (first-non-ground-finite-arg-index evaluated-args)
        arg-domain (domain-of evaluated-args non-ground-arg-idx)
        branches (for [v (dom/members arg-domain)]
                   ;; Constraint: arg = v, then call fn
                   [(list '= (nth args non-ground-arg-idx) v)
                    (cons head (assoc (vec args) non-ground-arg-idx v))])]
    (bind env (cons 'cond (apply concat branches))))
  ;; Normal call path
  ...)
```

## Open Questions for Pierre

1. **Auto vs opt-in?** Should the system detect recursive calls with non-ground args and auto-enumerate, or require an explicit annotation?
2. **Domain size limit?** What's a reasonable max for enumeration (100? 1000?)?
3. **Syntax preference?** `^:enum` metadata on fn, a wrapper form, or something else?
4. **Priority?** Is this blocking real use cases, or more of a nice-to-have?
