(ns mufl.showcase.bidirectional
  "Bidirectional Functions — one definition, two directions.

   In most languages you write a constructor to build values AND a separate
   parser/validator to inspect them. In mufl, you write the definition once.
   The same `defn` body:

     - In **expression position** — constructs a value from its arguments
     - In **pattern position** (inside `let`) — destructs a value, recovering
       the original arguments

   Body constraints (via `and`), type guards (`integer`, `string`, ...), and
   structural templates (`{:x ... :y ...}`) all apply bidirectionally. The
   invariants you write for construction are the exact same ones that fire
   during extraction. One definition. Two directions.

   Domain used throughout: 2D integer coordinate points.
   Evaluate the `(comment ...)` blocks in a REPL top-to-bottom."
  (:require [mufl.core :as m]))

;;════════════════════════════════════════════════════════════════
;; 1. THE CORE IDEA
;;════════════════════════════════════════════════════════════════
;;
;; A single `defn` body acts as a structural template. mufl can run
;; this template forward (build a map from args) or backward (extract
;; args from a map). No separate constructor/destructor needed — the
;; template IS both.

(comment

  ;; ── Expression position: BUILD a point ──────────────────────────────────
  ;; Call the function normally. Arguments flow IN, a map comes OUT.

  (m/query
   (do
     (defn point [x y] {:x (integer x) :y (integer y)})
     (point 1 2)))
  ;=> [{:x 1 :y 2}]

  ;; ── Pattern position: EXTRACT from a point ──────────────────────────────
  ;; Bind the function call on the LEFT side of a let binding.
  ;; The map flows IN, arguments come OUT.

  (m/query
   (do
     (defn point [x y] {:x (integer x) :y (integer y)})
     (let [(point a b) {:x 1 :y 2}]
       [a b])))
  ;=> [[1 2]]

  ;; ── Side by side ────────────────────────────────────────────────────────
  ;; Same defn. In expression position it constructs; in pattern position
  ;; it destructs. The template `{:x (integer x) :y (integer y)}` inverts:
  ;; keys become field sources, `(integer ...)` wrappers become guards.

  (m/query
   (do
     (defn point [x y] {:x (integer x) :y (integer y)})
     (let [constructed  (point 3 4)          ; build   → {:x 3 :y 4}
           (point cx cy) {:x 10 :y 20}]      ; extract → cx=10, cy=20
       [constructed cx cy])))
  ;=> [[{:x 3 :y 4} 10 20]]
  )
;;════════════════════════════════════════════════════════════════
;; 2. TYPE GUARDS PROPAGATE BOTH WAYS
;;════════════════════════════════════════════════════════════════
;;
;; `(integer x)` in the body is a type guard. In expression position it
;; validates each input value. In pattern position it filters the extracted
;; domain — non-integers never emerge from the pattern.
;;
;; You write the guard once. It enforces in both directions automatically.
;;
;; Note: pass domain variables via `let` rather than inline `one-of`
;; for multi-value construction.

(comment

  ;; ── Expression: construction across a domain ────────────────────────────
  ;; Bind domain to `x` first, then call `point`. Three inputs → three maps.

  (m/query
   (do
     (defn point [x y] {:x (integer x) :y (integer y)})
     (let [x (one-of 1 2 3)]
       (point x 0))))
  ;=> [{:x 1 :y 0} {:x 2 :y 0} {:x 3 :y 0}]

  ;; ── Pattern: non-integers filtered during extraction ─────────────────────
  ;; The incoming map has a mixed :x field (integers and a string).
  ;; `(integer x)` in the body removes non-integers at extraction time.

  (m/query
   (do
     (defn point [x y] {:x (integer x) :y (integer y)})
     (let [(point a b) {:x (one-of 1 "hello" 2) :y 10}]
       [a b])))
  ;=> [[1 10] [2 10]]
  ;; "hello" filtered — the same guard that validates construction
  ;; also filters during extraction.
  )
;;════════════════════════════════════════════════════════════════
;; 3. BODY CONSTRAINTS ARE INVERTIBLE
;;════════════════════════════════════════════════════════════════
;;
;; An `and` body wraps both constraints AND the structural template.
;; The constraints are fully bidirectional:
;;
;;   - Expression position: out-of-range args → contradiction (blocked)
;;   - Pattern position:    extracted values outside the range are filtered
;;
;; The invariant lives in one place. Construction and inspection share it.

(comment

  ;; ── Define a bounded point ───────────────────────────────────────────────

  ;; bounded-point only allows coordinates in [0, 5]
  (m/query
   (do
     (defn bounded-point [x y]
       (and (>= x 0) (<= x 5)
            (>= y 0) (<= y 5)
            {:x (integer x) :y (integer y)}))
     (let [(bounded-point a b) {:x 3 :y 2}]
       [a b])))
  ;=> [[3 2]]

  ;; ── Expression: out-of-range arg → constraint fires, throws ─────────────
  ;; x = -1 fails (>= x 0) — the same constraint that filters in pattern position.

  #_(m/query
     (do
       (defn bounded-point [x y]
         (and (>= x 0) (<= x 5)
              (>= y 0) (<= y 5)
              {:x (integer x) :y (integer y)}))
       (bounded-point -1 2)))
  ;; Throws: Contradiction detected — (>= -1 0) fails

  ;; ── Pattern: out-of-range values filtered during extraction ──────────────
  ;; The `one-of` domain contains values outside [0, 5]. The body
  ;; constraints eliminate them at extraction time.

  (m/query
   (do
     (defn bounded-point [x y]
       (and (>= x 0) (<= x 5)
            (>= y 0) (<= y 5)
            {:x (integer x) :y (integer y)}))
     (let [(bounded-point a b) {:x (one-of 0 1 2 3 4) :y 2}]
       [a b])))
  ;=> [[0 2] [1 2] [2 2] [3 2] [4 2]]

  ;; ── Both axes filtered simultaneously ───────────────────────────────────
  ;; Out-of-range values on both x and y eliminated in one pass.

  (m/query
   (do
     (defn bounded-point [x y]
       (and (>= x 0) (<= x 5)
            (>= y 0) (<= y 5)
            {:x (integer x) :y (integer y)}))
     (let [(bounded-point a b) {:x (one-of -1 3 7) :y (one-of -1 5 6)}]
       [a b])))
  ;=> [[3 5]]
  ;; x: only 3 survives [0,5]  ;  y: only 5 survives [0,5]
  )
;;════════════════════════════════════════════════════════════════
;; 4. THE `as` PATTERN
;;════════════════════════════════════════════════════════════════
;;
;; `(as name pattern)` binds the whole destructured value to `name`
;; while simultaneously extracting its components. You get both the
;; whole and its parts — in one binding, one step.

(comment

  ;; ── Bind whole + extract components ─────────────────────────────────────

  (m/query
   (do
     (defn point [x y] {:x (integer x) :y (integer y)})
     (let [(as p (point x y)) {:x 5 :y 10}]
       [p x y])))
  ;=> [[{:x 5 :y 10} 5 10]]
  ;; p is the whole map; x and y are the extracted coordinates.

  ;; ── as + additional constraints ──────────────────────────────────────────
  ;; After binding, you can further constrain the extracted parts.
  ;; The whole `p` stays consistent — it reflects the narrowed values.

  (m/query
   (do
     (defn point [x y] {:x (integer x) :y (integer y)})
     (let [(as p (point x y)) {:x (one-of 1 2 3) :y 10}]
       (> x 1)
       [p x y])))
  ;=> [[{:x 2 :y 10} 2 10] [{:x 3 :y 10} 3 10]]
  ;; x=1 filtered; p tracks the narrowing: {:x 2...} and {:x 3...}.
  )
;;════════════════════════════════════════════════════════════════
;; 5. ROUND-TRIP IDENTITY
;;════════════════════════════════════════════════════════════════
;;
;; Construct a value with `point`, then immediately destructure with
;; `(point a b)`. You recover exactly the original arguments.
;;
;; This isn't a coincidence — it's a theorem. The constructor and
;; destructor are inverses by construction (pun intended).

(comment

  ;; ── Construct 2, 3 → destruct → recover 2, 3 ────────────────────────────

  (m/query
   (do
     (defn point [x y] {:x (integer x) :y (integer y)})
     (let [p           (point 2 3)   ; build  {:x 2 :y 3}
           (point a b) p]            ; invert {:x 2 :y 3} → 2, 3
       [a b])))
  ;=> [[2 3]]

  ;; ── Round-trip across an entire domain ──────────────────────────────────
  ;; Bind domain variables with `let`, construct, then destruct.
  ;; Every point in the domain round-trips perfectly.

  (m/query
   (do
     (defn point [x y] {:x (integer x) :y (integer y)})
     (let [x           (one-of 1 2 3)
           y           (one-of 4 5 6)
           p           (point x y)
           (point a b) p]
       [a b])))
  ;=> [[1 4] [2 4] [3 4] [1 5] [2 5] [3 5] [1 6] [2 6] [3 6]]
  ;; All 9 pairs recover their original coordinates.

  ;; ── Insight ──────────────────────────────────────────────────────────────
  ;; In a conventional language, a round-trip test requires you to implement
  ;; both encode() and decode() and test their composition.
  ;; In mufl, both directions fall out of the single `defn` body.
  )
;;════════════════════════════════════════════════════════════════
;; 6. BACKWARD ARITHMETIC
;;════════════════════════════════════════════════════════════════
;;
;; When a `fn` body is an arithmetic expression (not a map/vector template),
;; bidirectionality works via EQUALITY CONSTRAINTS, not pattern destructuring.
;;
;;   `(= (double n) 6)` propagates backward: x + x = 6 → x = 3
;;
;; This is different from `(let [(double a) 6] a)` — that attempts pattern
;; destructuring which only works for map/vector bodies. For arithmetic,
;; use equality.

(comment

  ;; ── double: x + x ────────────────────────────────────────────────────────
  ;; Constraint `(= (double n) 6)` narrows `n` from {1..5} to {3}.

  (m/query
   (let [double (fn [x] (+ x x))
         n      (between 1 5)]
     (and (= (double n) 6) n)))
  ;=> [3]

  ;; ── translate: x + offset ────────────────────────────────────────────────
  ;; What x, when shifted by 10, equals 13?

  (m/query
   (let [translate (fn [x] (+ x 10))
         n         (between 0 20)]
     (and (= (translate n) 13) n)))
  ;=> [3]

  ;; ── Why equality, not pattern ────────────────────────────────────────────
  ;; Pattern destructuring `(let [(f a) 6] a)` inverts the body TEMPLATE.
  ;; A map template `{:x a}` inverts cleanly. An arithmetic expression
  ;; `(+ x x)` has no structural inverse — use equality constraints instead.
  )
;;════════════════════════════════════════════════════════════════
;; 7. NAMED CONSTRAINT FUNCTIONS
;;════════════════════════════════════════════════════════════════
;;
;; `defn` creates reusable predicates that narrow domains when called.
;; They compose: apply multiple named constraints to the same map and
;; mufl propagates all of them simultaneously.
;;
;; Constrain individual fields with keyword access — not the whole map.
;; The function body should end with a constraint expression, not a field
;; accessor, so that it acts as a pure filter without leaking its return value.

(comment

  ;; ── on-diagonal: both coordinates must be equal ──────────────────────────

  (m/query
   (do
     (defn on-diagonal [p] (= (:x p) (:y p)))
     (let [p {:x (between 0 3) :y (between 0 3)}]
       (on-diagonal p)
       [(:x p) (:y p)])))
  ;=> [[0 0] [1 1] [2 2] [3 3]]
  ;; (= (:x p) (:y p)) equates the two independent fields.

  ;; ── above-diagonal: x must be strictly less than y ───────────────────────

  (m/query
   (do
     (defn above-diagonal [p] (< (:x p) (:y p)))
     (let [p {:x (between 1 3) :y (between 1 3)}]
       (above-diagonal p)
       [(:x p) (:y p)])))
  ;=> [[1 2] [1 3] [2 3]]

  ;; ── Composing named constraints ──────────────────────────────────────────
  ;; All constraints apply simultaneously — intersection of all predicates.
  ;; Keep constraint bodies ending with a relational expression, not a field,
  ;; so they act as pure filters.

  (m/query
   (do
     (defn above-diagonal [p] (< (:x p) (:y p)))
     (defn far-from-origin [p] (and (>= (:x p) 2) (>= (:y p) 2)))
     (let [p {:x (between 1 3) :y (between 1 3)}]
       (above-diagonal p)
       (far-from-origin p)
       [(:x p) (:y p)])))
  ;=> [[2 3]]
  ;; x < y AND x >= 2 AND y >= 2 AND x,y in {1,2,3} → only (2,3)
  )
;;════════════════════════════════════════════════════════════════
;; 8. MULTI-BRANCH DISPATCH
;;════════════════════════════════════════════════════════════════
;;
;; `fn` branches can use type-wrapper params like `(integer x)` and
;; `(string x)` to dispatch on the type of the argument.
;; Only the matching branch fires; the others are eliminated.
;;
;; Multi-branch fns work reliably in expression position.

(comment

  ;; ── Type dispatch: integer vs string ────────────────────────────────────

  (m/query
   (let [label (fn [(integer x)] :number
                 [(string x)]  :text)]
     [(label 42) (label "hello")]))
  ;=> [[:number :text]]

  ;; ── Dispatch on a domain ─────────────────────────────────────────────────
  ;; Passing an integer domain: the engine picks the matching branch once
  ;; and returns one result for the whole domain.

  (m/query
   (let [label (fn [(integer x)] :number
                 [(string x)]  :text)
         v     (one-of 1 2 3)]
     (label v)))
  ;=> [:number]

  ;; ── Combining dispatch with point construction ───────────────────────────
  ;; Use a domain bound with `let` and combine with construction.

  (m/query
   (do
     (defn point [x y] {:x (integer x) :y (integer y)})
     (let [label (fn [(integer x)] :number
                   [(string x)]  :text)
           xs    (one-of 1 2 3)]
       [(point xs xs) (label xs)])))
  ;=> [[{:x 1 :y 1} :number] [{:x 2 :y 2} :number] [{:x 3 :y 3} :number]]
  )
