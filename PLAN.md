# Plan: Implement `sort-by` operation

## Context

`sort` was implemented in commit `95ad9c9` with bidirectional constraint propagation via a `:sorted-perm` narrowing function. `sort-by f v` extends this to sort by a key function `f` — the result is a permutation of `v` ordered by `(f elem)`. The challenge is expressing that the same permutation applied to keys must also be applied to values, without an explicit `element` constraint (variable-index access).

## Approach

**Single combined constraint** (`:sorted-by-perm`) that tracks 4 groups of n paths: input-keys, output-keys, input-values, output-values. The key function `f` is applied at bind time (like `map` unrolls), creating constraint nodes that link keys back to values through `f`'s internals — backward propagation through `f` comes for free.

The new narrowing function reuses the same key-ordering logic as `sorted-perm` plus adds **key-value pair coupling**: `output-value[i] ⊆ ⋃{input-value[j].domain : input-key[j].domain ∩ output-key[i].domain ≠ ∅}`.

## Files to modify

- `src/mufl/env.clj` — add `defop 'sort-by` in the `register-collections` section (after `sort` at ~line 1450)
- `src/mufl/narrow.clj` — add `narrow-sorted-by-perm` function (before `narrowing-fns` at ~line 719), register `:sorted-by-perm` in dispatch map
- `test/mufl/collection_ops_test.clj` — add `sort-by` test section after the existing sort tests (~line 850)

## Reuse

| What | Where | How used |
|------|-------|----------|
| `sort` defop pattern | `env.clj:1393` | Template for resolve-vector → create-fresh-vars → add-constraints → return-vector |
| `map` f-binding pattern | `env.clj:751` | Bind f to gensym, apply as `(f-sym (nth vec-expr i))` |
| `narrow-sorted-perm` | `narrow.clj:595` | Key propagation logic (forward/backward union, ordering bounds, singleton counting) — replicate for key track |
| `apply-narrowings` | `narrow.clj:51` | Apply computed `[path domain]` pairs, detect contradictions |
| `add-constraint` | `bind.clj:77` | Register the `:sorted-by-perm` constraint with refs and watches |
| `dom/unite`, `dom/intersect`, `dom/void?`, `dom/singleton?` | `domain.clj` | Domain algebra for narrowing |
| `bind/set-domain` (aliased from `narrow/set-domain`) | `narrow.clj:35` | Set initial domains on fresh variables |

## Steps

### Step 1: Add `narrow-sorted-by-perm` to `narrow.clj` (~80 LOC)

Insert before the `narrowing-fns` map (line ~719).

The function takes `env` and `refs` (a flat vector of 4n paths):
```
refs layout: [ki0..ki(n-1)  ko0..ko(n-1)  vi0..vi(n-1)  vo0..vo(n-1)]
              input-keys     output-keys    input-values   output-values
```

Three phases:

- [ ] **Phase 0 — All-keys-ground fast path**: When all input-key domains are singletons, compute the argsort permutation directly. Set each output-key to the sorted key value. Set each output-value to the input-value at the permuted index. Use `apply-narrowings`.

- [ ] **Phase 1 — Key propagation** (replicate from `sorted-perm`):
  - Forward: `ko[i] ⊆ input-key-union`, bounded by global min/max and neighbor ordering
  - Backward: `ki[j] ⊆ output-key-union`  
  - Singleton counting on keys: if key value `c` appears `k` times in input-key singletons and exactly `k` output-key positions can hold `c`, force those positions

- [ ] **Phase 2 — Value coupling** (new logic):
  - Forward: `vo[i] ⊆ ⋃{vi[j].domain : ki[j].domain ∩ ko[i].domain ≠ ∅}`
  - Backward: `vi[j] ⊆ ⋃{vo[i].domain : ko[i].domain ∩ ki[j].domain ≠ ∅}`
  - When `ko[i]` is singleton with value `c` and only one input position `j` has `ki[j]` containing `c`: force `vo[i] = vi[j].domain` (tight link)

- [ ] Register `:sorted-by-perm` in the `narrowing-fns` map

### Step 2: Add `defop 'sort-by` to `env.clj` (~65 LOC)

Insert after the `sorted?` defop (~line 1470). Follow the combined patterns of `sort` (fresh-var creation, constraint registration) and `map` (f-binding, element application).

- [ ] **Bind function**: `f-sym = (gensym "sortbyfn__")`, bind `f-sym` to `f-expr` (like `map`)
- [ ] **Resolve vector**: `resolve-collection env vec-expr :vector "sort-by"`
- [ ] **Edge cases**: n=0 → return `[]`; n=1 → return `[(nth vec-expr 0)]`
- [ ] **Compute keys**: For each element i, bind `key-sym[i] = (f-sym (nth vec-expr i))`. This creates constraint nodes linking keys to values through `f`. Collect key paths and domains.
- [ ] **Ground fast path**: If all key domains are singletons, compute argsort, bind result vector directly as `[(nth vec-expr π[0]) (nth vec-expr π[1]) ...]`
- [ ] **Constraint path**:
  - Create n fresh integer output-key variables (`rk`), domain = key-union
  - Create n fresh integer output-value variables (`rv`), domain = value-union  
  - Add `<=` chain: `rk[0] <= rk[1] <= ... <= rk[n-1]`
  - Collect all 4n paths: `[key-paths... rk-paths... val-paths... rv-paths...]`
  - Call `add-constraint` with `:sorted-by-perm`
  - Bind and return `(vec rv-syms)` as the result vector

### Step 3: Add tests to `collection_ops_test.clj` (~80 LOC)

Insert after existing sort tests section.

- [ ] **sort-by-basic** — ground inputs with various key functions:
  - `(sort-by (fn [x] (- x)) [1 3 2])` → `[[3 2 1]]` (descending via negation)
  - `(sort-by abs [-3 1 -2])` → `[[1 -2 -3]]` (by absolute value)
  - `(sort-by (fn [x] (mod x 10)) [21 12 33])` → `[[21 12 33]]` (by last digit)
  - Single element, empty vector edge cases

- [ ] **sort-by-with-domains** — domain inputs:
  - `(sort-by (fn [x] (- x)) [(one-of 1 3) (one-of 2 4)])` — verify all results sorted descending
  - Disjoint key domains → clean separation

- [ ] **sort-by-backward-propagation** — constrain output, verify input narrows:
  - Sort by negation, constrain first element of result → must be the max of input

- [ ] **sort-by-identity-matches-sort** — semantic equivalence:
  - `(sort-by identity v)` produces same result set as `(sort v)` for domain inputs

- [ ] **sort-by-non-injective-key** — lossy key function:
  - `(sort-by (fn [x] (mod x 3)) ...)` — elements with same key can appear in any order

### Step 4: Run tests, fix issues

- [ ] Run `clj -M:test` — verify all 558 existing tests still pass
- [ ] Verify new sort-by tests pass
- [ ] Check for edge cases in propagation (keys grounding incrementally during solve)

## Verification

```bash
cd /Users/pierrebaille/Code/mufl && clj -M:test
```

Expected: 558 + ~8 new tests = ~566 tests, 0 failures, 0 errors.

Manual spot-check: the key propagation chain works end-to-end:
```
rk ordering (<=) → rk narrows → sorted-by-perm backward → k[j] narrows → f propagation → v[j] narrows
```
