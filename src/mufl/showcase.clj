(ns mufl.showcase
  "A gradual showcase of mufl's primitives.
   
   mufl is a constraint-logic language embedded in Clojure.
   Variables hold *domains* — sets of possible values — and
   constraints *narrow* them. You describe what's valid;
   the system finds every answer that fits.
   
   Evaluate forms inside the (comment ...) blocks in a REPL.
   Each section introduces one concept, building on the last."
  (:require [mufl.core :refer [query query+ query1 query1* query-lazy]]))

;; ════════════════════════════════════════════════════════════════
;; 1. LITERALS AND `query`
;; ════════════════════════════════════════════════════════════════
;;
;; `query` evaluates a mufl expression and returns a vector of
;; all solutions. A literal has exactly one value in its domain,
;; so you get a single-element vector.

(comment

  (query 42)        ;=> [42]
  (query "hello")   ;=> ["hello"]
  (query :ok)       ;=> [:ok]
  (query true)      ;=> [true]

  ;; Nothing surprising yet. The interesting part: what happens
  ;; when a value could be *more than one thing*.
  )
;; ════════════════════════════════════════════════════════════════
;; 2. UNCERTAIN VALUES — `one-of`, `between`
;; ════════════════════════════════════════════════════════════════
;;
;; `one-of` creates a variable whose domain is the given values.
;; `between` creates an integer range (inclusive).
;; query enumerates all possibilities.

(comment

  (query (let [x (one-of 1 2 3)] x))
  ;=> [1 2 3]

  (query (let [x (one-of "red" "green" "blue")] x))
  ;=> ["red" "green" "blue"]

  ;; Domains can be mixed-type:
  (query (let [x (one-of 1 "hello" :ok)] x))
  ;=> [1 "hello" :ok]

  ;; Integer range:
  (query (let [x (between 1 5)] x))
  ;=> [1 2 3 4 5]

  ;; Still just enumeration. The power comes from constraints.
  )
;; ════════════════════════════════════════════════════════════════
;; 3. CONSTRAINTS — narrowing, not booleans
;; ════════════════════════════════════════════════════════════════
;;
;; In most languages, (> x 2) returns true or false.
;; In mufl, (> x 2) *removes* every value ≤ 2 from x's domain.
;; No booleans, no separate test-then-act step. Constraints are
;; side-effecting narrows — use `and` to sequence them and
;; return the value you care about.

(comment

  (query (let [x (one-of 1 2 3 4 5)]
           (and (> x 2) x)))
  ;=> [3 4 5]

  ;; All relational operators work this way: =, !=, <, >, <=, >=

  ;; Numeric predicates narrow by property:
  (query (let [x (between 1 10)]
           (and (even x) x)))
  ;=> [2 4 6 8 10]

  (query (let [x (between 1 10)]
           (and (odd x) x)))
  ;=> [1 3 5 7 9]

  ;; Type constraints narrow by type — and return the narrowed value:
  (query (let [x (one-of 1 "hello" :ok)]
           (integer x)))
  ;=> [1]

  (query (let [x (one-of 1 "hello" :ok)]
           (string x)))
  ;=> ["hello"]
  )
;; ════════════════════════════════════════════════════════════════
;; 4. `and` — sequencing constraints
;; ════════════════════════════════════════════════════════════════
;;
;; `and` applies each expression in order. Each one narrows
;; further. The last expression is the return value.

(comment

  (query (let [x (one-of 1 2 3 4 5)]
           (and (> x 1)
                (< x 5)
                (!= x 3)
                x)))
  ;=> [2 4]

  ;; Since constraints return the narrowed value, (!= x 3)
  ;; already returns x. The trailing x is technically redundant —
  ;; but making the return value explicit is often clearer.

  ;; Multiple variables: constraints propagate across all of them.
  (query (let [x (one-of 1 2 3 4 5)
               y (one-of 1 2 3 4 5)]
           (and (< x y)
                (= (+ x y) 6)
                [x y])))
  ;=> [[2 4] [1 5]]
  )
;; ════════════════════════════════════════════════════════════════
;; 5. `or`, `not` — alternatives and negation
;; ════════════════════════════════════════════════════════════════
;;
;; `or` tries each branch and unions the resulting domains.
;; `not` flips a relational constraint.

(comment

  ;; or: values that are < 3 OR > 7
  (query (let [x (between 1 10)]
           (and (or (< x 3) (> x 7)) x)))
  ;=> [1 2 8 9 10]

  ;; not: flip a constraint — (not (< x 3)) becomes (>= x 3)
  (query (let [x (one-of 1 2 3 4 5)]
           (and (not (< x 3)) x)))
  ;=> [3 4 5]

  ;; Compose naturally:
  ;; even AND positive
  (query (let [x (one-of -4 -2 0 1 2 3 4)]
           (and (even x) (pos x) x)))
  ;=> [2 4]

  ;; even OR negative
  (query (let [x (one-of -3 -1 1 2 3 4)]
           (and (or (even x) (neg x)) x)))
  ;=> [-3 -1 2 4]
  )
;; ════════════════════════════════════════════════════════════════
;; 6. `if`, `cond` — branching as exploration
;; ════════════════════════════════════════════════════════════════
;;
;; No booleans, so `if` doesn't test a truth value — it tries
;; the condition as a narrowing operation.
;;
;; If only one branch is satisfiable → takes it.
;; If both branches are satisfiable → explores both, unions results.
;;
;; When the condition is decidable (ground), exactly one branch
;; is taken eagerly. This is what makes recursion work.

(comment

  (query (let [x (between 1 10)]
           (if (< x 5) x 10)))
  ;=> [1 2 3 4 10]
  ;; Values 1–4 take the then-branch; 5–10 fall through to else
  ;; (all mapping to 10, deduplicated to one result).

  (query (let [x (between 1 10)]
           (cond
             (< x 4)  x
             (> x 7)  x
             :else    0)))
  ;=> [1 2 3 8 9 10 0]

  ;; Ground condition → eager resolution:
  (query (let [x 5]
           (if (= x 5) :yes :no)))
  ;=> [:yes]

  (query (let [x 5]
           (cond
             (< x 3)  :small
             (> x 7)  :big
             :else    :medium)))
  ;=> [:medium]
  )
;; ════════════════════════════════════════════════════════════════
;; 7. `let` with nullary type constructors
;; ════════════════════════════════════════════════════════════════
;;
;; (integer), (string), (free) etc. in `let` create fresh typed
;; variables. This is the idiomatic way to introduce logic
;; variables when you know their type.

(comment

  ;; x is a fresh integer — unbounded until constrained
  (query (let [x (integer) y (integer)]
           (and (>= x 0) (>= y 0)
                (= (+ x y) 3)
                [x y])))
  ;=> [[3 0] [2 1] [1 2] [0 3]]

  ;; Unary form constrains an existing value:
  (query (let [x (one-of 1 "hello" :foo)]
           (integer x)))
  ;=> [1]
  )
;; ════════════════════════════════════════════════════════════════
;; 8. `fresh` — logic variable introduction
;; ════════════════════════════════════════════════════════════════
;;
;; `fresh` introduces logic variables with optional type
;; declarations. Body forms are implicit `and`.

(comment

  ;; Typed declarations:
  (query (fresh [(integer x) (integer y)]
                (>= x 0) (>= y 0)
                (= (+ x y) 8)
                [x y]))
  ;=> [[8 0] [7 1] [6 2] [5 3] [4 4] [3 5] [2 6] [1 7] [0 8]]

  ;; Bare symbols get (free) — unconstrained until narrowed:
  (query (fresh [x y]
                (integer x) (integer y)
                (>= x 0) (>= y 0)
                (= (+ x y) 3)
                [x y]))
  ;=> [[3 0] [2 1] [1 2] [0 3]]

  ;; Mixed:
  (query (fresh [(integer x) y]
                (>= x 1) (<= x 3)
                (= y "hello")
                [x y]))
  ;=> [[1 "hello"] [2 "hello"] [3 "hello"]]
  )
;; ════════════════════════════════════════════════════════════════
;; 9. ARITHMETIC — derived domains
;; ════════════════════════════════════════════════════════════════
;;
;; Arithmetic on uncertain values creates derived domains.
;; Constraints on derived values propagate backward to inputs.

(comment

  ;; Derived sum:
  (query (let [x (one-of 1 2 3)
               s (+ x 10)]
           s))
  ;=> [11 12 13]

  ;; Backward propagation: constraining the sum narrows x and y
  (query (let [x (one-of 1 2 3 4 5)
               y (one-of 1 2 3 4 5)
               s (+ x y)]
           (and (< x y) (= s 6) [x y s])))
  ;=> [[2 4 6] [1 5 6]]

  ;; mod: find multiples of 3
  (query (let [x (between 1 20)]
           (and (= (mod x 3) 0) x)))
  ;=> [3 6 9 12 15 18]

  ;; abs: bidirectional — constraining (abs x) narrows both signs
  (query (abs -5))
  ;=> [5]

  (query (let [x (one-of -3 -2 -1 0 1 2 3)]
           (and (= (abs x) 2) x)))
  ;=> [-2 2]

  ;; min, max:
  (query (min 3 7))
  ;=> [3]

  (query (max 3 7))
  ;=> [7]
  )
;; ════════════════════════════════════════════════════════════════
;; 10. COLLECTIONS — vectors, maps, access
;; ════════════════════════════════════════════════════════════════
;;
;; Vectors and maps are first-class. Each element is its own
;; domain node — constraints target individual elements.

(comment

  ;; Vector access:
  (query (let [v [10 20 30]] (nth v 1)))
  ;=> [20]

  ;; Map access:
  (query (let [m {:x 1 :y 2}] (get m :x)))
  ;=> [1]

  ;; Keyword-as-function sugar:
  (query (let [m {:x 1 :y 2}] (:x m)))
  ;=> [1]

  ;; Constraints propagate through collection access:
  (query (let [m {:x (one-of 1 2 3) :y 10}]
           (and (> (:x m) 1) m)))
  ;=> [{:x 2 :y 10} {:x 3 :y 10}]

  ;; Nested access:
  (query (let [m {:a {:b 42}}]
           (:b (:a m))))
  ;=> [42]

  ;; count:
  (query (let [v [10 20 30]] (count v)))
  ;=> [3]

  ;; drop:
  (query (let [v [1 2 3 4 5]] (drop 2 v)))
  ;=> [[3 4 5]]

  ;; dissoc:
  (query (let [m {:x 1 :y 2 :z 3}] (dissoc m :x)))
  ;=> [{:y 2 :z 3}]

  (query (let [m {:x 1 :y 2 :z 3}] (dissoc m :x :y)))
  ;=> [{:z 3}]
  )
;; ════════════════════════════════════════════════════════════════
;; 11. DESTRUCTURING — maps, vectors, dot syntax
;; ════════════════════════════════════════════════════════════════
;;
;; Let bindings support destructuring with full constraint
;; propagation. The bound symbols get the tightest domains
;; the system can infer.

(comment

  ;; Map destructuring:
  (query (let [{:x x :y y} {:x (one-of 1 2 3) :y (one-of 1 2 3)}]
           (and (< x y) [x y])))
  ;=> [[1 2] [1 3] [2 3]]

  ;; Vector destructuring:
  (query (let [[a b c] [10 20 30]] (+ a b)))
  ;=> [30]

  ;; `as` — bind the whole value alongside extracted parts:
  (query (let [(as v [a b]) [(one-of 1 2 3) 10]]
           (and (> a 1) v)))
  ;=> [[2 10] [3 10]]

  ;; `ks` — keyword-symbol destructuring (extracts :x → x, :y → y):
  (query (let [(ks x y) {:x 1 :y 2}] (+ x y)))
  ;=> [3]

  ;; `or` — pattern fallback:
  (query (let [(or (ks x) [x]) {:x 42}] x))
  ;=> [42]

  ;; Dot syntax — vector rest:
  (query (let [[a b . xs] [1 2 3 4 5]] xs))
  ;=> [[3 4 5]]

  ;; Bare dot — ignore rest:
  (query (let [[a b .] [1 2 3 4]] [a b]))
  ;=> [[1 2]]

  ;; Head/middle/tail — two+ elements after dot:
  (query (let [[a . mid c] [1 2 3 4 5]] [a mid c]))
  ;=> [[1 [2 3 4] 5]]

  ;; Map rest:
  (query (let [{:x a . rest} {:x 1 :y 2 :z 3}] rest))
  ;=> [{:y 2 :z 3}]

  ;; Map rest with ks:
  (query (let [{:x a . (ks y z)} {:x 1 :y 2 :z 3}] [a y z]))
  ;=> [[1 2 3]]
  )
;; ════════════════════════════════════════════════════════════════
;; 12. `fn` — bidirectional functions
;; ════════════════════════════════════════════════════════════════
;;
;; A function is a bidirectional computation:
;;   - expression position: call it → constructs a value
;;   - pattern position: use it → destructures a value
;;
;; This is what functions *are* in mufl. Not a special mode.

(comment

  ;; Basic function — constraints propagate through:
  (query (let [double (fn [x] (+ x x))
               n (between 1 5)]
           (and (= (double n) 6) n)))
  ;=> [3]

  ;; Bidirectional: construct and destruct with the same fn

  ;; Expression position → construct:
  (query (let [point (fn [x y] {:x (integer x) :y (integer y)})]
           (point 1 2)))
  ;=> [{:x 1 :y 2}]

  ;; Pattern position → destruct:
  (query (let [point (fn [x y] {:x (integer x) :y (integer y)})
               (point a b) {:x 1 :y 2}]
           [a b]))
  ;=> [[1 2]]

  ;; Destructuring narrows — the integer guard eliminates "hello":
  (query (let [point (fn [x y] {:x (integer x) :y (integer y)})
               (point a b) {:x (one-of 1 "hello" 2) :y 10}]
           [a b]))
  ;=> [[1 10] [2 10]]

  ;; Body-level constraints apply in both directions:
  (query (let [small-point (fn [x y]
                             (and (>= x 1) (<= x 3)
                                  {:x (integer x) :y (integer y)}))
               (small-point a b) {:x (one-of 0 1 2 3 4) :y 10}]
           [a b]))
  ;=> [[1 10] [2 10] [3 10]]
  )
;; ════════════════════════════════════════════════════════════════
;; 13. MULTI-BRANCH `fn` — pattern matching
;; ════════════════════════════════════════════════════════════════
;;
;; fn supports multiple branches — each [params] starts a new
;; clause. First satisfiable branch wins. Params support literals,
;; type wrappers, destructuring. Branches can differ in arity.

(comment

  ;; Literal dispatch:
  (query (let [f (fn [0] :zero
                   [1] :one
                   [n] :other)]
           (f 0)))
  ;=> [:zero]

  ;; Type-based dispatch:
  (query (let [describe (fn [(integer x)] :int
                          [(string x)]  :str)]
           [(describe 42) (describe "hi")]))
  ;=> [[:int :str]]

  ;; Different arities per branch:
  (query (let [f (fn [x]   (* x 2)
                   [x y] (+ x y))]
           [(f 5) (f 3 4)]))
  ;=> [[10 7]]

  ;; Default (trailing expression, no params vector):
  (query (let [f (fn [0] :zero
                   42)]
           (f 5)))
  ;=> [42]
  )
;; ════════════════════════════════════════════════════════════════
;; 14. RECURSION — natural with multi-branch
;; ════════════════════════════════════════════════════════════════
;;
;; Multi-branch makes recursion clean: the base case is a
;; pattern, no `if` needed.

(comment

  ;; Factorial:
  (query (let [fact (fn [0] 1
                      [n] (* n (fact (- n 1))))]
           (fact 5)))
  ;=> [120]

  ;; Fibonacci:
  (query (let [fib (fn [0] 0
                     [1] 1
                     [n] (+ (fib (- n 1)) (fib (- n 2))))]
           (fib 10)))
  ;=> [55]

  ;; Single-branch with if works too:
  (query (let [base 2
               pow (fn [n] (if (= n 0) 1 (* base (pow (- n 1)))))]
           (pow 5)))
  ;=> [32]

  ;; GCD:
  (query (let [gcd (fn [a b] (if (= b 0) a (gcd b (mod a b))))]
           (gcd 48 18)))
  ;=> [6]
  )
;; ════════════════════════════════════════════════════════════════
;; 15. `defn` — named functions, constructor/destructor
;; ════════════════════════════════════════════════════════════════
;;
;; `defn` is sugar for (def name (fn ...)). Since fn is
;; bidirectional, defn inherits both call and destructure.
;; Use `do` to scope definitions within a query.

(comment

  ;; Named constructor/destructor:
  (query (do (defn point [x y] {:x (integer x) :y (integer y)})
             (point 1 2)))
  ;=> [{:x 1 :y 2}]

  (query (do (defn point [x y] {:x (integer x) :y (integer y)})
             (let [(point a b) {:x 1 :y 2}]
               [a b])))
  ;=> [[1 2]]

  ;; `as` + destructor: bind whole value and parts:
  (query (do (defn point [x y] {:x (integer x) :y (integer y)})
             (let [(as p (point x y)) {:x (one-of 1 2 3) :y 10}]
               (and (> x 1) [p x y]))))
  ;=> [[{:x 2 :y 10} 2 10] [{:x 3 :y 10} 3 10]]

  ;; Named constraint functions:
  (query (defn positive [x] (> x 0))
         (defn ordered [a b] (< a b))
         (let [x (one-of -1 0 1 2 3)
               y (one-of -1 0 1 2 3)]
           (positive x)
           (positive y)
           (ordered x y)
           [x y]))
  ;=> [[1 2] [1 3] [2 3]]

  ;; Multi-branch defn:
  (query (do (defn fact
               [0] 1
               [n] (* n (fact (- n 1))))
             (fact 5)))
  ;=> [120]
  )
;; ════════════════════════════════════════════════════════════════
;; 16. `def`, `narrow` — domains and schemas
;; ════════════════════════════════════════════════════════════════
;;
;; `def` names a value — just an alias. `narrow` constrains
;; a value against a domain (scalar, composite, or structural).

(comment

  ;; Scalar narrow:
  (query (let [x (one-of 1 "hello" :foo)]
           (narrow x integer)
           x))
  ;=> [1]

  ;; Structural schema: person with name (string) and age (0..150)
  (query (def person {:name string :age (between 0 150)})
         (let [p {:name "Alice" :age (one-of 10 25 200)}]
           (narrow p person)
           (:age p)))
  ;=> [10 25]

  ;; Schemas compose with `and`:
  (query (def person {:name string :age (between 0 150)})
         (def employee (and person {:company string}))
         (let [e {:name "Alice" :age (one-of 30 200) :company "Acme"}]
           (narrow e employee)
           (:age e)))
  ;=> [30]

  ;; Vector tuple as schema:
  (query (def point [integer integer])
         (let [p [(one-of 3 "x") (one-of 4 "y")]]
           (narrow p point)
           p))
  ;=> [[3 4]]

  ;; Schema + additional constraint:
  (query (def person {:name string :age (between 0 150)})
         (let [p {:name "Alice" :age (one-of 10 25 30)}]
           (narrow p person)
           (>= (:age p) 18)
           p))
  ;=> [{:name "Alice" :age 25} {:name "Alice" :age 30}]
  )
;; ════════════════════════════════════════════════════════════════
;; 17. `vector-of`, `tuple`, `map-of` — type constructors
;; ════════════════════════════════════════════════════════════════
;;
;; Constrain entire collections at once. Walk elements at bind
;; time with full constraint propagation.

(comment

  ;; vector-of: constrain every element
  (query (let [v [(one-of 1 "a") (one-of 2 "b") (one-of 3 "c")]]
           (narrow v (vector-of integer))
           v))
  ;=> [[1 2 3]]

  ;; In a def schema:
  (query (def int-vec (vector-of integer))
         (let [v [(one-of 1 "x") (one-of 2 "y")]]
           (narrow v int-vec)
           v))
  ;=> [[1 2]]

  ;; tuple: per-position types, exact length
  (query (let [v [(one-of 42 "x") (one-of "hello" 99) (one-of true 0)]]
           (narrow v (tuple [integer string boolean]))
           v))
  ;=> [[42 "hello" true]]

  ;; map-of: constrain all keys and values
  (query (let [m {:a (one-of 1 "x") :b (one-of 2 "y")}]
           (narrow m (map-of keyword integer))
           m))
  ;=> [{:a 1 :b 2}]

  ;; Nesting: domain with a vector-of field
  (query (def student (and {:name string}
                           {:scores (vector-of integer)}))
         (let [s {:name "Alice" :scores [(one-of 90 "x") (one-of 80 "y")]}]
           (narrow s student)
           [(get s :name) (get s :scores)]))
  ;=> [["Alice" [90 80]]]
  )
;; ════════════════════════════════════════════════════════════════
;; 17b. Callable domains — domains as functions
;; ════════════════════════════════════════════════════════════════
;;
;; Any bound domain value can be called as a function.
;; (my-domain x) ≡ (narrow x my-domain)
;; Works in both expression and pattern position.

(comment

  ;; Expression position: call a domain to constrain
  (query (let [intvec (vector-of integer)
               v [(one-of 1 "a") (one-of 2 "b")]]
           (intvec v) v))
  ;=> [[1 2]]

  ;; Struct domain called as function:
  (query (def person {:name string :age integer})
         (let [p {:name (one-of "Alice" 42) :age (one-of 30 "old")}]
           (person p) p))
  ;=> [{:name "Alice" :age 30}]

  ;; Pattern position: (let [(domain x) v] x)
  (query (let [intvec (vector-of integer)
               v [(one-of 1 "a") (one-of 2 "b")]]
           (let [(intvec x) v] x)))
  ;=> [[1 2]]

  ;; Pattern + inner destructuring:
  (query (let [intvec (vector-of integer)
               v [(one-of 1 "a") (one-of 2 "b")]]
           (let [(intvec [a b]) v] [a b])))
  ;=> [[1 2]]

  ;; Scalar domain as pattern guard:
  (query (def small (between 1 10))
         (let [v (one-of 5 20 "x")]
           (let [(small x) v] x)))
  ;=> [5]
  )
;; ════════════════════════════════════════════════════════════════
;; 18. `map`, `filter`, `reduce` — higher-order functions
;; ════════════════════════════════════════════════════════════════
;;
;; HOFs on vectors. They unroll at bind time — constraint
;; propagation works through them, not just on ground values.

(comment

  ;; map:
  (query (let [v [1 2 3]
               doubled (map (fn [x] (* x 2)) v)]
           doubled))
  ;=> [[2 4 6]]

  ;; filter:
  (query (let [v [1 2 3 4 5]
               evens (filter even v)]
           evens))
  ;=> [[2 4]]

  ;; reduce:
  (query (let [v [1 2 3 4 5]
               total (reduce + 0 v)]
           total))
  ;=> [15]

  ;; HOFs compose with uncertain values:
  (query (let [v [(one-of 1 5) (one-of 2 6) (one-of 3 7)]
               total (reduce + 0 v)]
           (and (> total 10)
                [v total])))
  ;=> [[[5 6 3] 14] [[1 6 7] 14] [[5 2 7] 14] [[5 6 7] 18]]

  ;; Filter with a named constraint:
  (query (do (defn big [x] (> x 3))
             (let [v [1 2 3 4 5]
                   bigs (filter big v)]
               bigs)))
  ;=> [[4 5]]

  ;; Chaining: sum of doubled values
  (query (let [v [1 2 3]
               doubled (map (fn [x] (* x 2)) v)
               total (reduce + 0 doubled)]
           total))
  ;=> [12]
  )
;; ════════════════════════════════════════════════════════════════
;; 19. `distinct` — all-different constraint
;; ════════════════════════════════════════════════════════════════
;;
;; Every variable in the vector must take a different value.

(comment

  (query (let [a (one-of 1 2 3)
               b (one-of 1 2 3)
               c (one-of 1 2 3)]
           (and (distinct [a b c])
                [a b c])))
  ;=> [[3 2 1] [2 3 1] [3 1 2] [1 3 2] [2 1 3] [1 2 3]]
  ;; All 6 permutations — no duplicates within any solution.

  ;; Classic: graph coloring (3 nodes, all connected)
  (query (let [a (one-of :red :green :blue)
               b (one-of :red :green :blue)
               c (one-of :red :green :blue)]
           (and (!= a b) (!= b c) (!= a c)
                [a b c])))
  ;; 6 solutions — every valid 3-coloring.
  )
;; ════════════════════════════════════════════════════════════════
;; 20. ALTERNATIVE QUERY MODES
;; ════════════════════════════════════════════════════════════════
;;
;; query  — all solutions (eager)
;; query+ — all solutions, full environments
;; query-lazy — lazy seq (compute on demand)
;; query1 — one at a time with continuation

(comment

  ;; query+ returns full variable bindings, not just the result:
  (query+ (let [x (one-of 1 2 3)
                y (one-of 1 2 3)]
            (and (< x y) [x y])))
  ;=> [{x 1, y 2} {x 1, y 3} {x 2, y 3}]

  ;; query-lazy: only compute what you take
  (take 5 (query-lazy (let [x (between 1 100)] x)))
  ;=> (1 2 3 4 5)

  (take 3 (query-lazy (let [x (between 1 10)]
                        (and (> x 3)
                             (= 0 (mod x 2))
                             x))))
  ;=> (4 6 8)

  ;; query1: step through solutions one at a time
  (let [[v state] (query1 (let [x (one-of 1 2 3)] x))
        [v2 state'] (query1* state)]
    [v v2])
  ;=> [1 2]

  ;; nil when no solutions remain:
  (query1 (let [x (one-of 1 2 3)]
            (and (> x 10) x)))
  ;=> nil
  )
;; ════════════════════════════════════════════════════════════════
;; 21. CAPSTONE — Pythagorean triples
;; ════════════════════════════════════════════════════════════════
;;
;; No loops, no generate-and-test. Declare what a Pythagorean
;; triple *is*, and the constraint engine finds them all.

(comment

  (query (let [a (between 1 15)
               b (between 1 15)
               c (between 1 15)]
           (and (< a b)
                (<= b c)
                (= (+ (* a a) (* b b)) (* c c))
                [a b c])))
  ;=> [[3 4 5] [6 8 10] [5 12 13] [9 12 15]]
  )
;; ════════════════════════════════════════════════════════════════
;; 22. CAPSTONE — putting it all together
;; ════════════════════════════════════════════════════════════════
;;
;; Bidirectional functions + destructuring + constraints.
;; Define a domain, construct values, destructure them,
;; and constrain — all composing naturally.

(comment

  ;; Graph coloring: 3 adjacent regions, 3 colors, all different
  (query (let [wa (one-of :red :green :blue)
               nt (one-of :red :green :blue)
               sa (one-of :red :green :blue)]
           (!= wa nt) (!= wa sa) (!= nt sa)
           [wa nt sa]))
  ;=> [[:red :green :blue] [:green :red :blue] [:red :blue :green]
  ;;   [:blue :red :green] [:green :blue :red] [:blue :green :red]]

  ;; Bidirectional point: define once, use both ways + constrain
  (query (do (defn point [x y] {:x (integer x) :y (integer y)})
             (let [(point a b) {:x (one-of 1 2 3 4 5) :y (one-of 1 2 3 4 5)}]
               (< a b)
               (= (+ a b) 6)
               [a b])))
  ;=> [[2 4] [1 5]]

  ;; Schema + HOFs + recursion in one query:
  ;; Compute factorial of each score, keep only those > 100
  (query (do (defn fact [0] 1 [n] (* n (fact (- n 1))))
             (let [scores [3 5 4 6]
                   factorials (map fact scores)
                   big (filter (fn [x] (> x 100)) factorials)]
               big)))
  ;=> [[120 720]]
  )
;; ════════════════════════════════════════════════════════════════
;; THE MENTAL MODEL
;; ════════════════════════════════════════════════════════════════
;;
;; You're not computing a result step by step — you're
;; *describing* the space of valid answers, and the system
;; narrows it down for you.
;;
;; Variables are domains. Constraints remove impossibilities.
;; Functions are invertible. Every expression participates in
;; constraint propagation. What remains is the answer.
