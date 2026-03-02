(ns mufl.showcase.type-infer
  "Type Inference Engine — HM-style type inference as a mufl constraint program.

   Demonstrates: recursive defn, multi-branch cond dispatch, map destructuring,
   domain narrowing, and constraint propagation narrowing uncertain types.

   The core insight: type inference IS constraint propagation. A lambda parameter
   starts with an uncertain type; constraints from its uses in the body narrow it
   to the correct type. mufl makes this literal — we describe the inference rules,
   and the constraint engine does the work.

   Evaluate the (comment ...) blocks in a REPL top-to-bottom."
  (:require [mufl.core :refer [query query*]]))

;; ════════════════════════════════════════════════════════════════
;; 1. TYPE REPRESENTATION
;; ════════════════════════════════════════════════════════════════
;;
;; Types are Clojure maps with a :kind discriminator:
;;
;;   {:kind :int}                         — integer type
;;   {:kind :bool}                        — boolean type
;;   {:kind :arrow :from T :to T}         — function type T → T
;;
;; These are ground Clojure data passed into the mufl query as
;; concrete values. Uncertainty lives in the *search* over which
;; type an expression has.

(comment

  ;; The three types
  {:kind :int}
  {:kind :bool}
  {:kind :arrow :from {:kind :int} :to {:kind :bool}}

  ;; Arrow types compose
  {:kind :arrow
   :from {:kind :int}
   :to   {:kind :arrow :from {:kind :bool} :to {:kind :int}}})

;; ════════════════════════════════════════════════════════════════
;; 2. EXPRESSION LANGUAGE
;; ════════════════════════════════════════════════════════════════
;;
;; Expressions are ground Clojure maps. They form a simple typed
;; lambda calculus with integers, booleans, addition, lambda
;; abstraction, application, and conditionals.
;;
;;   {:kind :int-lit  :val 42}            — integer literal
;;   {:kind :bool-lit :val true}          — boolean literal
;;   {:kind :var      :name :x}           — variable reference
;;   {:kind :add      :left E :right E}   — addition (forces both to :int)
;;   {:kind :lam      :param :x :body E}  — lambda (param ∈ {:x :y :z})
;;   {:kind :app      :fn E   :arg E}     — function application
;;   {:kind :iff      :cond E :then E :else E}  — conditional

(comment

  ;; 42
  {:kind :int-lit :val 42}

  ;; true
  {:kind :bool-lit :val true}

  ;; x  (assuming x is in scope)
  {:kind :var :name :x}

  ;; x + 1
  {:kind :add
   :left  {:kind :var :name :x}
   :right {:kind :int-lit :val 1}}

  ;; λx. (x + 1)
  {:kind :lam
   :param :x
   :body  {:kind :add
           :left  {:kind :var :name :x}
           :right {:kind :int-lit :val 1}}}

  ;; (λx. x + 1) 42
  {:kind :app
   :fn  {:kind :lam
         :param :x
         :body  {:kind :add
                 :left  {:kind :var :name :x}
                 :right {:kind :int-lit :val 1}}}
   :arg {:kind :int-lit :val 42}}

  ;; if true then 1 else 2
  {:kind :iff
   :cond {:kind :bool-lit :val true}
   :then {:kind :int-lit :val 1}
   :else {:kind :int-lit :val 2}})

;; ════════════════════════════════════════════════════════════════
;; 3. TYPE ENVIRONMENT
;; ════════════════════════════════════════════════════════════════
;;
;; A type environment maps variable names (keywords) to their types.
;; It is a plain Clojure map — always ground.
;;
;;   {}                               — empty env
;;   {:x {:kind :int}}               — x has type int
;;   {:x {:kind :int} :y {:kind :bool}}
;;
;; Extending the environment inside mufl requires a workaround:
;; since map keys must be literal in mufl's `merge`, we dispatch
;; on the param name with `cond` and use a literal key per branch.
;; Variables are restricted to :x, :y, :z for this showcase.
;;
;; Lookup also requires literal keys in mufl's `get`, so variable
;; access uses the same cond-dispatch pattern.

(comment

  ;; Extending an environment with a new variable binding
  (query (let [env {:x {:kind :int}}]
           (merge env {:y {:kind :bool}})))
  ;=> [{:x {:kind :int}, :y {:kind :bool}}]

  ;; Looking up a variable — must use literal key
  (query (let [env {:x {:kind :int}}]
           (get env :x)))
  ;=> [{:kind :int}]

  ;; The cond-dispatch pattern for both merge and get:
  (query (let [env   {:x {:kind :int}}
               name  :y
               t     {:kind :bool}
               new-env (cond (= name :x) (merge env {:x t})
                             (= name :y) (merge env {:y t})
                             (= name :z) (merge env {:z t}))]
           new-env))
  ;=> [{:x {:kind :int}, :y {:kind :bool}}]
  )
;; ════════════════════════════════════════════════════════════════
;; 4. TYPE INFERENCE CORE
;; ════════════════════════════════════════════════════════════════
;;
;; `infer*` is a recursive mufl function. For each expression kind,
;; it returns the inferred type. The cond dispatches on (:kind expr),
;; which is always ground, so mufl resolves each branch eagerly.
;;
;; ── Key mufl patterns used ────────────────────────────────────
;;
;;   {:kind (one-of :int :bool)}   — uncertain type, narrowed by constraints
;;   (= (:kind t) :int)            — constraint: t's kind must be :int
;;   (let [r {:kind :int}] r)      — bind result to avoid scope leakage
;;   cond-dispatch on :name/:param — required because get/merge need literal keys
;;
;; ── Inference rules ───────────────────────────────────────────
;;
;;   INT-LIT:  ⊢ n : int                                    (trivial)
;;   BOOL-LIT: ⊢ b : bool                                   (trivial)
;;   VAR:      Γ ⊢ x : Γ(x)                                 (lookup)
;;   ADD:      Γ ⊢ e1 : int   Γ ⊢ e2 : int
;;             ─────────────────────────────
;;             Γ ⊢ e1 + e2 : int
;;
;;   LAM:      Γ, x:τ ⊢ e : σ    τ ∈ {int, bool}
;;             ────────────────────────────────────
;;             Γ ⊢ λx.e : τ → σ
;;             (constraint propagation narrows τ from the body)
;;
;;   APP:      Γ ⊢ e1 : τ → σ   Γ ⊢ e2 : τ
;;             ─────────────────────────────
;;             Γ ⊢ e1 e2 : σ
;;
;;   IFF:      Γ ⊢ ec : bool   Γ ⊢ et : τ   Γ ⊢ ee : τ
;;             ──────────────────────────────────────────
;;             Γ ⊢ if ec then et else ee : τ

(def ^:private infer-defn
  '(defn infer*
     [env expr]
     (let [k (:kind expr)]
       (cond
         ;; ── INT-LIT ─────────────────────────────────────────
         ;; An integer literal always has type :int.
         ;; We bind the result to avoid local scope leaking into the map.
         (= k :int-lit)
         (let [r {:kind :int}] r)

         ;; ── BOOL-LIT ────────────────────────────────────────
         ;; A boolean literal always has type :bool.
         (= k :bool-lit)
         (let [r {:kind :bool}] r)

         ;; ── VAR ─────────────────────────────────────────────
         ;; Look up the variable in the type environment.
         ;; mufl requires literal keys in `get`, so we dispatch on name.
         (= k :var)
         (let [name (:name expr)]
           (cond (= name :x) (get env :x)
                 (= name :y) (get env :y)
                 (= name :z) (get env :z)))

         ;; ── ADD ─────────────────────────────────────────────
         ;; Both operands must have kind :int; result is :int.
         ;; Comparing kinds (not full maps) is what mufl's = supports.
         (= k :add)
         (let [t-left  (infer* env (:left  expr))
               t-right (infer* env (:right expr))]
           (= (:kind t-left)  :int)
           (= (:kind t-right) :int)
           (let [r {:kind :int}] r))

         ;; ── LAM ─────────────────────────────────────────────
         ;; The parameter type starts uncertain: {:kind (one-of :int :bool)}.
         ;; Constraints from the body narrow it to the right type.
         ;; mufl requires literal keys in merge, so we dispatch on param name.
         (= k :lam)
         (let [param   (:param expr)
               t-param {:kind (one-of :int :bool)}
               new-env (cond (= param :x) (merge env {:x t-param})
                             (= param :y) (merge env {:y t-param})
                             (= param :z) (merge env {:z t-param}))
               t-body  (infer* new-env (:body expr))
               r       {:kind :arrow :from t-param :to t-body}]
           r)

         ;; ── APP ─────────────────────────────────────────────
         ;; The function must have kind :arrow.
         ;; The argument's kind must match the arrow's :from kind.
         ;; The result is the arrow's :to field.
         (= k :app)
         (let [t-fn  (infer* env (:fn  expr))
               t-arg (infer* env (:arg expr))]
           (= (:kind t-fn) :arrow)
           (= (:kind (:from t-fn)) (:kind t-arg))
           (:to t-fn))

         ;; ── IFF ─────────────────────────────────────────────
         ;; Condition must have kind :bool.
         ;; Both branches must have the same kind.
         ;; The result is the then-branch type.
         (= k :iff)
         (let [t-cond (infer* env (:cond expr))
               t-then (infer* env (:then expr))
               t-else (infer* env (:else expr))]
           (= (:kind t-cond) :bool)
           (= (:kind t-then) (:kind t-else))
           t-then)))))

;; ════════════════════════════════════════════════════════════════
;; 5. PUBLIC API
;; ════════════════════════════════════════════════════════════════
;;
;; `infer` is a Clojure-level function that runs a mufl query.
;; It builds the query form from `infer-defn` (the mufl function
;; definition) plus a concrete call `(infer* env expr)` with the
;; runtime values spliced in as ground Clojure data.
;;
;; Returns the inferred type map on success.
;; Throws clojure.lang.ExceptionInfo on type error (contradiction).
;;
;; `infer-all` returns all valid typings. Most expressions have
;; exactly one type. Lambdas with unused parameters have multiple —
;; one per candidate parameter type.

(defn infer
  "Type-infer `expr` in type environment `env`.
   Returns the inferred type map {:kind ...} on success.
   Throws clojure.lang.ExceptionInfo if no consistent typing exists."
  [env expr]
  (first (query* (list 'do infer-defn (list 'infer* env expr)))))

(defn infer-all
  "Return all valid typings for `expr` in type environment `env`.
   Most expressions have exactly one type. Lambdas over an unused
   parameter have multiple — one per candidate parameter type."
  [env expr]
  (query* (list 'do infer-defn (list 'infer* env expr))))

;; ════════════════════════════════════════════════════════════════
;; 6. BASIC EXAMPLES
;; ════════════════════════════════════════════════════════════════
;;
;; Simple cases: literals and variable lookup. Each expression has
;; a unique type determined without any search.

(comment

  ;; Integer literal → :int
  (infer {} {:kind :int-lit :val 42})
  ;=> {:kind :int}

  ;; Boolean literal → :bool
  (infer {} {:kind :bool-lit :val true})
  ;=> {:kind :bool}

  ;; Variable lookup in non-empty env
  (infer {:x {:kind :int}} {:kind :var :name :x})
  ;=> {:kind :int}

  (infer {:y {:kind :bool}} {:kind :var :name :y})
  ;=> {:kind :bool}
  )
;; ════════════════════════════════════════════════════════════════
;; 7. ADDITION — constraint propagation on sub-expressions
;; ════════════════════════════════════════════════════════════════
;;
;; Add requires both sides to have kind :int. If either side has
;; a different kind, the = constraint throws ExceptionInfo.

(comment

  ;; x + 1  where x : int  →  int
  (infer {:x {:kind :int}}
         {:kind :add
          :left  {:kind :var :name :x}
          :right {:kind :int-lit :val 1}})
  ;=> {:kind :int}

  ;; 3 + 4  →  int
  (infer {}
         {:kind :add
          :left  {:kind :int-lit :val 3}
          :right {:kind :int-lit :val 4}})
  ;=> {:kind :int}

  ;; true + 1  →  type error (bool kind ≠ int kind)
  ;; (throws clojure.lang.ExceptionInfo)
  ;; (infer {} {:kind :add :left {:kind :bool-lit :val true} :right {:kind :int-lit :val 1}})
  )
;; ════════════════════════════════════════════════════════════════
;; 8. LAMBDA — constraint propagation narrows parameter type
;; ════════════════════════════════════════════════════════════════
;;
;; This is the showcase's key demonstration. The lambda parameter
;; type starts uncertain: {:kind (one-of :int :bool)}. The body
;; expression applies constraints that narrow it. mufl's constraint
;; propagation finds the only consistent typing automatically.
;;
;; When the parameter is unused, the type is ambiguous — mufl
;; returns all valid typings (one per candidate).

(comment

  ;; λx. (x + 1)
  ;; x is used in add → (:kind t-x) must be :int → t-x narrowed to {:kind :int}
  ;; Constraint propagation: (= (:kind t-x) :int) narrows t-param
  (infer {} {:kind :lam
             :param :x
             :body  {:kind :add
                     :left  {:kind :var :name :x}
                     :right {:kind :int-lit :val 1}}})
  ;=> {:kind :arrow, :from {:kind :int}, :to {:kind :int}}

  ;; λx. true — x unused, both typings valid
  ;; mufl searches both branches of (one-of :int :bool) and finds two solutions
  (infer-all {} {:kind :lam
                 :param :x
                 :body  {:kind :bool-lit :val true}})
  ;=> [{:kind :arrow, :from {:kind :bool}, :to {:kind :bool}}
  ;;   {:kind :arrow, :from {:kind :int}, :to {:kind :bool}}]

  ;; λx. λy. (x + y) — nested lambdas, both params constrained to :int
  (infer {} {:kind :lam
             :param :x
             :body  {:kind :lam
                     :param :y
                     :body  {:kind :add
                             :left  {:kind :var :name :x}
                             :right {:kind :var :name :y}}}})
  ;=> {:kind :arrow, :from {:kind :int},
  ;;               :to {:kind :arrow, :from {:kind :int}, :to {:kind :int}}}
  )
;; ════════════════════════════════════════════════════════════════
;; 9. APPLICATION — checking function/argument type compatibility
;; ════════════════════════════════════════════════════════════════
;;
;; Application checks that the function is an arrow type and the
;; argument's kind matches the arrow's :from kind. The result is
;; the arrow's :to field.

(comment

  ;; (λx. x + 1) 42  →  int
  (infer {}
         {:kind :app
          :fn  {:kind :lam
                :param :x
                :body  {:kind :add
                        :left  {:kind :var :name :x}
                        :right {:kind :int-lit :val 1}}}
          :arg {:kind :int-lit :val 42}})
  ;=> {:kind :int}

  ;; Applying an env-bound function x : int → bool  →  (x 0) : bool
  (infer {:x {:kind :arrow :from {:kind :int} :to {:kind :bool}}}
         {:kind :app
          :fn  {:kind :var :name :x}
          :arg {:kind :int-lit :val 0}})
  ;=> {:kind :bool}
  )
;; ════════════════════════════════════════════════════════════════
;; 10. CONDITIONAL — branches must unify
;; ════════════════════════════════════════════════════════════════
;;
;; The condition must have kind :bool. The then and else branches
;; must have the same kind. The result is the then-branch type.

(comment

  ;; if true then 1 else 2  →  int
  (infer {} {:kind :iff
             :cond {:kind :bool-lit :val true}
             :then {:kind :int-lit :val 1}
             :else {:kind :int-lit :val 2}})
  ;=> {:kind :int}

  ;; if y then (x + 1) else 0  where y : bool, x : int  →  int
  ;; Note: showcase vars are :x :y :z only
  (infer {:y {:kind :bool} :x {:kind :int}}
         {:kind :iff
          :cond {:kind :var :name :y}
          :then {:kind :add
                 :left  {:kind :var :name :x}
                 :right {:kind :int-lit :val 1}}
          :else {:kind :int-lit :val 0}})
  ;=> {:kind :int}
  )
;; ════════════════════════════════════════════════════════════════
;; 11. TYPE ERRORS — constraint violations throw ExceptionInfo
;; ════════════════════════════════════════════════════════════════
;;
;; When no consistent typing exists, mufl's = constraint encounters
;; a contradiction and throws clojure.lang.ExceptionInfo.
;; In tests, use (is (thrown? clojure.lang.ExceptionInfo ...)).

(comment

  ;; Applying a non-function (42 : int) — (:kind t-fn) = :int ≠ :arrow
  ;; (throws clojure.lang.ExceptionInfo "Contradiction detected")
  ;; (infer {} {:kind :app :fn {:kind :int-lit :val 42} :arg {:kind :int-lit :val 1}})

  ;; Wrong argument type — (:kind (:from t-fn)) = :int ≠ (:kind t-arg) = :bool
  ;; (throws clojure.lang.ExceptionInfo "Contradiction detected")
  ;; (infer {} {:kind :app
  ;;            :fn  {:kind :lam :param :x
  ;;                  :body {:kind :add :left {:kind :var :name :x}
  ;;                                   :right {:kind :int-lit :val 1}}}
  ;;            :arg {:kind :bool-lit :val true}})

  ;; Conditional branches mismatch — (:kind t-then) = :int ≠ (:kind t-else) = :bool
  ;; (throws clojure.lang.ExceptionInfo "Contradiction detected")
  ;; (infer {} {:kind :iff :cond {:kind :bool-lit :val true}
  ;;                       :then {:kind :int-lit :val 1}
  ;;                       :else {:kind :bool-lit :val false}})
  )
