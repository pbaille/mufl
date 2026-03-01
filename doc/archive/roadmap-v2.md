> **📦 ARCHIVED** — Moved to `doc/archive/` on 2026-02-28. Status at archival:
> - Phase 2a quick wins: ✅ ALL DONE (stale link domain, arity check, derived flag, DRY wrappers)
> - Phase 2b completion: ✅ DONE (collections, destructuring, HOFs, def/defn, callable domains)
> - Phase 2.5 hardening: ⬜ Scoped constraints not done; recursion works for ground args
> - Phase 3 domains: Partially done (vector-of, tuple, map-of, narrow, callable domains work)
> - Codebase grew from 128 tests / 208 assertions to 491 tests / 1440 assertions
> - File structure changed: bind.clj split into bind.clj, narrow.clj, pattern.clj, schema.clj
> - env.clj split into 8 `register-*` groups

# mufl Roadmap v2 — Post-Audit Assessment

**Date:** 2026-02-25  
**Context:** After Phases 1 & 2 audit (128 tests, 208 assertions, 5 bugs fixed, 11 architectural issues identified)

---

## 1. Architectural Issue Assessment

### Load-Bearing vs. Surface-Level Classification

Each issue is assessed for **compounding risk** — would building Phase 3+ on top make this harder to fix?

---

#### ISSUE 1: Constraint nodes live at root level

**Assessment: 🔶 Fix before Phase 3 — load-bearing**

Currently every constraint creates a `:c1234` gensym node at root, alongside user primitives like `+`, `let`, `fn`. With the current ~10 constraints in a query this is manageable. But Phase 3 (defdomain, domain composition) and Phase 2 completion (collections, destructuring, HOFs) will multiply constraint count by 5-10×. A `defdomain Person {:name string :age integer}` that generates structural constraints per field, plus collection access generating per-element constraints, will produce an unreadable root.

More critically, **scope-based constraint GC becomes impossible**. When a function call creates temporary constraints, they persist at root forever. This doesn't cause incorrect results today, but it means:
- Memory grows monotonically with expression complexity
- Debugging becomes nearly impossible (hundreds of `:c*` and `:expr*` nodes at root)
- Any future optimization pass (dead constraint elimination, scope-local propagation) requires rewriting where constraints live

**Compounding risk:** HIGH. Every new form that creates constraints adds to the problem. Moving constraints later means migrating all `add-constraint` call sites + updating all `watched-by` paths + changing `propagate`'s constraint lookup.

**Recommendation:** FIX NOW. Move constraints into scope-local `::constraints` subtrees. Each workspace/call scope owns its constraints. Propagation walks the scope chain upward. Cost: ~1 day, touches `add-constraint`, `propagate`, `collect-constraints`, and the `watched-by` path format.

---

#### ISSUE 2: Stale `:domain` on link nodes

**Assessment: 🟢 Fix now — quick win**

When `y` is bound to `x`, `y` gets a snapshot of `x`'s domain that immediately goes stale. The `domain-of` function correctly follows links, so this never causes wrong results. But it's a conceptual trap: anyone reading tree state (including future code that inspects nodes) might read the stale domain.

**Compounding risk:** LOW but insidious. More link-creating forms (destructuring, HOFs) = more stale snapshots. It's a 5-minute fix.

**Recommendation:** FIX NOW. Remove `:domain` from link nodes in `bind`'s symbol case. Just store `:link`. Cost: change one line in `bind.clj`, update the stale-domain test expectation.

---

#### ISSUE 3: Three disjunction mechanisms (or / if / cond)

**Assessment: 🔶 Defer unification, but document the contract clearly**

`or` does bind-time domain union (fast, imprecise). `if`/`cond` do search-time fork expansion (slow, precise). This is actually a reasonable architecture: `or` is the fast-path approximation, `if`/`cond` is the precise path. The problem isn't the duality — it's that the relationship isn't explicit.

**Compounding risk:** MEDIUM. Phase 3's `defdomain` will need disjunction for type unions (e.g., `(defdomain Shape (or Circle Rect))`). If this creates forks, it's precise but potentially very slow. If it uses `or`-style union, it's fast but lossy. We need to decide which, and the current ad-hoc split makes that decision harder.

**Recommendation:** DEFER full unification, but:
1. Document explicitly that `or` = bound consistency (approximation) and `if`/`cond` = fork consistency (precise)
2. Add an `or-precise` or make `if`/`cond` available as expressions that can be used wherever `or` is used
3. When Phase 3 arrives, `defdomain` can choose which mechanism based on cardinality

---

#### ISSUE 4: `even?`/`odd?` are one-shot filters, not constraints

**Assessment: 🟢 Fine as-is — document**

Since narrowing is monotonic (domains only shrink), a one-shot filter is permanently valid. The only scenario where this matters is if a future mechanism widens domains — but that contradicts the core design principle.

**Compounding risk:** NONE under current semantics. If we add domain widening (which we shouldn't), this breaks. But domain widening would break many things.

**Recommendation:** KEEP. Document that predicate filters are one-shot by design, consistent with monotonic narrowing.

---

#### ISSUE 5: `min`/`max` aren't constraint-backed

**Assessment: 🟡 Fix when needed — not blocking**

Unlike `abs` which creates a narrowing constraint, `min`/`max` just compute the result domain. Constraining a `min`/`max` result can't propagate back to operands. This is a real limitation, but `min`/`max` are relatively rarely used as constraint targets.

**Compounding risk:** LOW. Adding constraint backing later is additive — it doesn't require changing existing code, just adding `narrow-min`/`narrow-max` functions and making the `:bind` create constraints like `abs` does.

**Recommendation:** FIX LATER. When we have a use case that needs `min`/`max` inverse propagation. The fix is mechanical: copy the `abs` pattern.

---

#### ISSUE 6: Recursive functions don't work

**Assessment: 🔴 Fundamental limitation — needs design thought before Phase 3**

The fork-at-search-time approach for `if` means recursive function bodies can't fully evaluate. A recursive function like `(fn [n] (if (= n 0) 1 (* n (self (- n 1)))))` can't work because:
1. `if` stores a fork, doesn't evaluate branches at bind time
2. The recursive call would need to create nested forks ad infinitum
3. There's no termination detection

This blocks a large class of useful programs. Phase 3's domain definitions with recursive structure (e.g., linked lists, trees) would need recursion.

**Compounding risk:** HIGH for expressiveness, LOW for correctness of existing code. The fix is non-trivial — it requires either:
- A special `recur` that unrolls to a bounded depth
- Tabled/memoized recursion (like Datalog's fixpoint)
- Detecting when domains become ground and evaluating eagerly

**Recommendation:** DESIGN NOW, implement in Phase 2.5. Don't build Phase 3's recursive domains without a recursion story.

---

#### ISSUE 7: fn arity isn't checked

**Assessment: 🟢 Fix now — quick win**

Extra args silently dropped, too few args give confusing "Unresolved symbol" error. This is a 10-minute fix in `bind`'s `:mufl-fn` call path.

**Recommendation:** FIX NOW. Add `(when (not= (count params) (count args)) (throw ...))` before binding. Cost: 3 lines.

---

#### ISSUE H-5 (from audit): Derived nodes should be marked with `:derived` flag

**Assessment: 🟢 Fix now — improves robustness**

`pick-split-variable` uses name-pattern matching (`derived-name?` checking for "expr"/"lit"/"call" prefixes) to skip intermediate nodes. A `:derived true` flag on the node itself is more robust.

**Compounding risk:** LOW, but the current approach breaks if naming conventions change or if user-defined names happen to start with "expr"/"lit"/"call".

**Recommendation:** FIX NOW. Add `:derived true` in `ensure-node`, check it in `pick-split-variable`. Cost: 5 lines.

---

### Summary Table

| Issue | Severity | Fix When | Effort | Blocks Phase 3? |
|-------|----------|----------|--------|-----------------|
| #1 Root-level constraints | 🔶 Load-bearing | **Phase 2.5** | 1 day | Yes — debugging, GC |
| #2 Stale link domain | 🟢 Quick win | **Now** | 15 min | No |
| #3 Three disjunction mechanisms | 🔶 Defer | Phase 3 | 2-3 days | Partially |
| #4 even?/odd? one-shot | 🟢 Fine | Never | 0 | No |
| #5 min/max not constraint-backed | 🟡 Later | When needed | 2 hours | No |
| #6 No recursion | 🔴 Fundamental | **Phase 2.5** | Design: 1 day, impl: 2-3 days | Yes |
| #7 No arity check | 🟢 Quick win | **Now** | 15 min | No |
| H-5 Derived node flag | 🟢 Quick win | **Now** | 30 min | No |

---

## 2. Continue vs. Refound Assessment

### What Phase 3 Actually Needs

Phase 3 is `defdomain` — defining composite domain types that act as both constraints and destructuring patterns. Let's trace what that requires:

```clojure
(defdomain Person {:name string :age (range 0 150)})

;; As constraint: (Person p) narrows p to have :name∈string, :age∈0..150
;; As destructuring: (let [(Person {:name n :age a}) p] ...)
;; As composition: (defdomain Employee (and Person {:company string}))
```

**What this needs from the tree:**
1. **Collection nodes** — maps/vectors as first-class domain-bearing tree nodes (Phase 2 incomplete)
2. **Key access** — `(:name p)` navigating into a map node and returning a sub-domain
3. **Structural constraints** — "this node must be a map with keys :name and :age"
4. **Constraint scoping** — domain-generated constraints should be scoped to the domain application site
5. **Recursive structure** — `(defdomain Tree {:val any :left (or nil Tree) :right (or nil Tree)})`

Items 1-3 are Phase 2 completion work. Item 4 is Issue #1 (root-level constraints). Item 5 is Issue #6 (no recursion).

### Verdict: **Hybrid — targeted hardening, then proceed**

The foundations are sound. The core bind-propagate-search pipeline is correct, well-tested, and conceptually clean. The tree-as-single-source-of-truth principle works. The domain algebra is solid.

**We should NOT refound.** The architecture doesn't have fundamental flaws — it has structural debt in two specific areas (constraint scoping, recursion) that would compound. A targeted hardening phase addresses these without losing momentum.

**We should NOT just continue.** Building collections, destructuring, and `defdomain` on top of root-level constraints would create a debugging nightmare and require expensive rework.

---

## 3. Revised Roadmap

### Phase 2a: Quick Wins (1 session)

Minimal fixes that improve code quality immediately:

1. **Remove stale `:domain` from link nodes** (Issue #2)
   - In `bind`'s symbol case: `(assoc env :link ...)` without copying `:domain`
   - Update test expectation in `link-stale-domain-is-snapshot`

2. **Add fn arity checking** (Issue #7)
   - Check `(count params) = (count args)` before binding in the `:mufl-fn` call path
   - Clear error message: "Expected N args, got M"

3. **Mark derived nodes with `:derived true`** (H-5)
   - Set `:derived true` in `ensure-node` for literal and expression nodes
   - Replace `derived-name?` in `pick-split-variable` with `(:derived child)`

4. **DRY up relational constraint wrappers** (QUALITY-5)
   - Replace 6 identical `defprim` calls with a `doseq`

### Phase 2.5: Structural Hardening (2-3 sessions)

Address the two load-bearing issues before building more on top:

#### 2.5a: Scoped Constraints

Move constraint nodes from root to the scope that created them.

**Design:**
- Each scope node can have a `::constraints` child that holds constraint nodes
- `add-constraint` places the constraint under the nearest enclosing scope (workspace or call scope)
- `watched-by` paths are updated to point into the scope's `::constraints` subtree
- `propagate` works the same — it follows `watched-by` paths regardless of where they point
- `collect-constraints` walks the scope chain instead of scanning root children

**Migration path:**
- `add-constraint` changes from `(tree/root env)` to `(tree/root env)` but places under scope
- All tests should still pass — behavior is identical, only node placement changes
- Debug output becomes dramatically cleaner

**Benefits unlocked:**
- Call-scope constraints can be garbage collected when the call returns
- Constraint locality makes debugging tractable
- Future optimization: scope-local propagation (don't re-propagate constraints in unrelated scopes)

#### 2.5b: Recursion Strategy

Design and implement bounded recursion for user-defined functions.

**Proposed approach: bounded unrolling with ground-check**

```clojure
;; When a recursive call is detected:
;; 1. Check if all args are ground (singleton domains)
;; 2. If yes: eagerly evaluate (no forking needed)
;; 3. If no: unroll up to N levels, then return domain approximation
```

This handles the common case (recursive functions called with ground values during search) while avoiding infinite unrolling for symbolic recursion.

**Implementation:**
- Detect recursion: track the call stack during fn application (push fn name, check before calling)
- Ground-arg fast path: if all args are singleton, evaluate eagerly (bind `if` directly instead of forking)
- Bounded unroll: configurable depth limit (default: 20), return `any` domain at limit
- Error on symbolic recursion exceeding depth (clear message: "Recursive function with non-ground args exceeds depth limit")

**This enables:**
- `factorial`, `fibonacci`, `length`, `sum` — any function that recurses on ground values
- Phase 3's recursive domains (bounded structural recursion)

### Phase 2b: Phase 2 Completion (2-3 sessions)

Complete the remaining Phase 2 items from the original roadmap:

1. **Collection access** — `(get m :key)`, `(:key m)`, `(nth v i)`
   - Map nodes: children are keyed by map keys, access returns child node domain
   - Vector nodes: children are keyed by index, access returns child node domain
   - Creates link from accessor result to the collection child

2. **Destructuring** — `(let [{:keys [a b]} m] ...)`
   - `:destruct` functions on map/vector forms
   - Pattern-position dispatch: when a form appears in `let` binding position, call `:destruct` instead of `:bind`
   - Start with `ks` (keyword destructure) and positional vector destructure

3. **`defc` — named constraint functions**
   ```clojure
   (defc positive [x] (> x 0))
   ;; usage: (positive n) ≡ (> n 0)
   ```
   - Essentially a macro: `defc` stores the body template, application expands and binds it
   - Simpler than full fn because no scope creation needed — constraints operate on caller's scope

4. **HOFs: `map`/`reduce`** (if collection access is done)
   - `map` applies a function to each element of a collection, creating a new collection node
   - `reduce` folds — requires recursion support (hence Phase 2.5b prerequisite)

### Phase 3: Domains (3-4 sessions)

With scoped constraints and recursion in place:

1. **`defdomain` macro**
   ```clojure
   (defdomain Person {:name string :age (range 0 150)})
   ```
   Creates a node with both `:bind` (constraint application) and `:destruct` (pattern matching).

2. **Domain-as-constraint**: `(Person p)` narrows `p`'s structural domain
   - Generates scoped constraints: `:name` must be string, `:age` must be in range
   - Uses collection access from Phase 2b

3. **Domain-as-destructuring**: `(let [(Person {:name n}) p] ...)`
   - Combines destructuring with domain constraint

4. **Domain composition**: `(defdomain Employee (and Person {:company string}))`
   - Intersection of structural constraints
   - Uses `and` semantics (all constraints must hold)

5. **Recursive domains** (if 2.5b recursion works):
   ```clojure
   (defdomain IntList (or nil {:head integer :tail IntList}))
   ```

### Phase 4: Optimization & Search Intelligence (future)

1. **maximize/minimize** — objective functions over constraint solutions
2. **Smarter `or`** — optionally precise (fork-based) when cardinality is small
3. **Cross-scope propagation** — constraints that span function boundaries
4. **Lazy domain evaluation** — don't enumerate large cartesian products eagerly
5. **Constraint learning** — nogood recording from failed branches

---

## 4. Immediate Actions

Before the next session, do these (30 minutes total):

### Action 1: Remove stale domain from links
```clojure
;; In bind.clj, symbol case of bind:
;; BEFORE:
(assoc env :link (tree/position found-target) :domain (:domain found-target))
;; AFTER:
(assoc env :link (tree/position found-target))
```

### Action 2: Add fn arity check
```clojure
;; In bind.clj, :mufl-fn application:
;; ADD before binding params:
(when (not= (count params) (count args))
  (throw (ex-info (str "Arity mismatch: " (count params) " params, " (count args) " args")
                  {:params params :args args})))
```

### Action 3: Mark derived nodes
```clojure
;; In bind.clj, ensure-node:
;; For literal case, add :derived true:
(let [anon (gensym "lit")
      env' (bind env anon expr)]
  ;; After binding, mark as derived
  [env' [anon]])
;; → need to set :derived on the node

;; In search.clj, pick-split-variable:
;; Replace (not (derived-name? child-name)) with (not (:derived child))
```

### Action 4: DRY relational wrappers
```clojure
;; In env.clj, replace 6 defprim calls with:
(reduce (fn [env [sym op]]
          (defprim env sym {:bind (fn [env args] (bind/bind-relational env op args))}))
        env
        [['< :<] ['> :>] ['<= :<=] ['>= :>=] ['= :=] ['!= :!=]])
```

---

## 5. Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|------------|
| Scoped constraints break propagation edge cases | Medium | High | Extensive test suite (128 tests) catches regressions |
| Recursion design is too complex | Medium | Medium | Start with ground-arg-only recursion, extend later |
| Phase 2b (collections) reveals new architectural issues | Medium | Medium | Collections are well-understood; the tree already supports map/vector nodes |
| `defdomain` turns out to need something we didn't anticipate | Low | High | Phase 2.5 + 2b together give us most of the infrastructure; domain composition is the wildcard |
| Over-engineering the hardening phase | Low | Medium | Timebox to 2-3 sessions; if it takes longer, reassess |

---

## 6. Confidence Assessment

**Foundation health: 8/10.** The core pipeline (bind → propagate → search) is correct and well-tested. The domain algebra is solid. The tree navigation works. The two load-bearing issues (constraint scoping, recursion) are real but bounded — they're not architectural rot, they're missing features.

**Path confidence: HIGH for hybrid approach.** Continuing without hardening would create a debugging nightmare in Phase 3. Refounding would be wasteful — the architecture is right, it just needs two targeted improvements. The hybrid path (quick wins → targeted hardening → complete Phase 2 → Phase 3) is the lowest-risk highest-progress option.

**Timeline estimate:**
- Phase 2a (quick wins): 1 session
- Phase 2.5 (hardening): 2-3 sessions
- Phase 2b (completion): 2-3 sessions
- Phase 3 (domains): 3-4 sessions
- **Total to Phase 3 complete: 8-11 sessions**
