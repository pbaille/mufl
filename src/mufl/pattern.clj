(ns mufl.pattern
  "Pattern-position destructuring for mufl.

   bind-pattern recursively decomposes patterns (symbols, maps, vectors,
   operator forms) against a seed binding, propagating domain information
   to every bound symbol.

   Protocol: (fn [env pattern seed-sym seed-domain] → [env' bound-syms])
     env         — tree node at current scope position
     pattern     — unevaluated syntax (symbol, map, vector, or list)
     seed-sym    — symbol already bound in env to the value being destructured
     seed-domain — domain of the seed (dom/any when unknown)
     Returns [env' bound-syms] — updated env + ordered vector of all symbols bound"
  (:require [mufl.tree :as tree]
            [mufl.domain :as dom]
            [mufl.narrow :as narrow]))

;; Late binding for bind functions (breaks circular dep)
(defn- lazy-bind []
  @(requiring-resolve 'mufl.bind/bind))

(defn- lazy-ensure-node-abs []
  @(requiring-resolve 'mufl.bind/ensure-node-abs))

(declare bind-pattern bind-pattern-map bind-pattern-vec bind-pattern-operator)

(defn substitute-expr
  "Walk an expression tree, replacing occurrences of symbols in the
   substitution map with their replacement expressions.
   Used by defn/defc for macro-expansion of constraint functions."
  [expr subs-map]
  (cond
    (symbol? expr)   (get subs-map expr expr)
    (seq? expr)      (apply list (map #(substitute-expr % subs-map) expr))
    (vector? expr)   (mapv #(substitute-expr % subs-map) expr)
    (map? expr)      (into {} (map (fn [[k v]] [k (substitute-expr v subs-map)])) expr)
    :else            expr))

;; ════════════════════════════════════════════════════════════════
;; Domain extraction helpers
;; ════════════════════════════════════════════════════════════════

(defn- extract-field-domain
  "Extract the domain for a specific map field key from a seed.
   Checks the seed node's tree children first (structural maps),
   then falls back to composite seed-domain info (:map-of), then dom/any."
  [env seed-sym key seed-domain]
  (or
   ;; Try tree children — the common case for structural maps.
   (when-let [found (tree/find env [seed-sym])]
     (let [seed-node (narrow/resolve found)]
       (when (:map seed-node)
         (when-let [child (tree/cd seed-node [key])]
           (let [target (narrow/resolve child)]
             (:domain target))))))
   ;; map-of composite → all values share the value domain
   (when (= :map-of (:kind seed-domain))
     (:value seed-domain))
   ;; Fallback
   dom/any))

(defn- extract-element-domain
  "Extract the domain for a vector element at index i.
   Checks composite seed-domain info first (tuple per-position, vector-of uniform),
   then falls back to tree children, then dom/any."
  [env seed-sym i seed-domain]
  (or
   ;; Tuple composite: per-position domains
   (when (= :tuple (:kind seed-domain))
     (get (:elements seed-domain) i))
   ;; Vector-of composite: uniform element domain
   (when (= :vector-of (:kind seed-domain))
     (:element seed-domain))
   ;; Try tree children — structural vector with per-index children
   (when-let [found (tree/find env [seed-sym])]
     (let [seed-node (narrow/resolve found)]
       (when (:vector seed-node)
         (when-let [child (tree/cd seed-node [i])]
           (let [target (narrow/resolve child)]
             (:domain target))))))
   ;; Fallback
   dom/any))

;; ════════════════════════════════════════════════════════════════
;; domain-node — typed child binding
;; ════════════════════════════════════════════════════════════════

(defn domain-node
  "Create a child binding with a known domain.
   Uses `bind` to create the node (which handles get/nth dispatch,
   link creation, constraint watching, etc.), then narrows its domain to
   the intersection of its current domain and `domain`.
   This is mufl's equivalent of immucode's `typed-node`."
  [env sym domain build-expr]
  (let [bind* (lazy-bind)
        env' (bind* env sym build-expr)]
    (if (and domain (not (dom/any? domain)))
      (let [found (tree/find env' [sym])
            _ (when-not found
                (throw (ex-info (str "domain-node: cannot find just-bound symbol: " sym)
                                {:sym sym})))
            target (narrow/resolve found)
            target-path (tree/position target)
            current (or (:domain target) dom/any)
            narrowed (dom/intersect current domain)]
        (if (dom/void? narrowed)
          (throw (ex-info "Domain contradiction in destructuring"
                          {:sym sym :domain domain :current current}))
          (-> (tree/root env')
              (tree/put target-path :domain narrowed)
              (tree/cd (tree/position env')))))
      env')))

;; ════════════════════════════════════════════════════════════════
;; bind-pattern — main entry point
;; ════════════════════════════════════════════════════════════════

(defn bind-pattern
  "Recursively destructure a pattern against a seed symbol with domain propagation.
   Returns [env' bound-syms] where bound-syms is an ordered vector of all
   symbols bound (including scaffolding gensyms before user symbols)."
  [env pattern seed-sym seed-domain]
  (cond
    ;; Symbol: trivial bind — link to seed with its domain
    (symbol? pattern)
    (let [env' (domain-node env pattern seed-domain seed-sym)]
      [env' [pattern]])

    ;; Map pattern: {:k1 pat1 :k2 pat2 . rest}
    (map? pattern)
    (bind-pattern-map env pattern seed-sym seed-domain)

    ;; Vector pattern: [a b c] or [a b . xs]
    (vector? pattern)
    (bind-pattern-vec env pattern seed-sym seed-domain)

    ;; List pattern (operator): (ks a b), (as m pat), etc.
    (seq? pattern)
    (bind-pattern-operator env pattern seed-sym seed-domain)

    ;; Literal: constrain seed to equal this value
    :else
    (if (or (number? pattern) (string? pattern) (keyword? pattern)
            (boolean? pattern) (nil? pattern))
      (let [found (tree/find env [seed-sym])
            _ (when-not found
                (throw (ex-info "Cannot find seed for literal pattern"
                                {:seed seed-sym :pattern pattern})))
            target (narrow/resolve found)
            target-path (tree/position target)
            d (or (:domain target) dom/any)
            literal-dom (dom/finite #{pattern})
            narrowed (dom/intersect d literal-dom)]
        (if (dom/void? narrowed)
          (throw (ex-info "Literal pattern mismatch"
                          {:pattern pattern :domain d}))
          (let [env' (narrow/set-domain (tree/root env) target-path narrowed)
                env' (or (tree/cd env' (tree/position env)) env')]
            [env' []])))
      (throw (ex-info "Unknown pattern form" {:pattern pattern})))))

(defn bind-pattern-map
  "Destructure a map pattern {:k1 pat1 :k2 pat2 . rest} with domain flow.
   Extracts per-field domains from the seed's tree children or composite domain.
   Returns [env' bound-syms]."
  [env pattern seed-sym seed-domain]
  (let [dot-val (get pattern '.)
        entries (dissoc pattern '.)
        ;; Helpful error for legacy {:keys [...]} usage
        _ (when (:keys pattern)
            (throw (ex-info (str "The {:keys [...]} form is not supported. "
                                 "Use {:x x :y y} or (ks x y) instead.")
                            {:pattern pattern})))
        _ (doseq [k (keys entries)]
            (when-not (keyword? k)
              (throw (ex-info (str "Map destructuring keys must be keywords, got: " (pr-str k))
                              {:pattern pattern :key k}))))
        ;; Fold over entries, propagating field domains
        [env syms]
        (reduce
         (fn [[env syms] [k v-pattern]]
           (let [field-dom (extract-field-domain env seed-sym k seed-domain)
                 get-expr (list 'get seed-sym k)]
             (if (symbol? v-pattern)
               ;; Simple symbol: bind directly with field domain
               (let [env' (domain-node env v-pattern field-dom get-expr)]
                 [env' (conj syms v-pattern)])
               ;; Nested pattern: create scaffolding gensym, then recurse
               (let [gsym (gensym "map_")
                     env' (domain-node env gsym field-dom get-expr)
                     [env'' inner-syms] (bind-pattern env' v-pattern gsym field-dom)]
                 [env'' (into (conj syms gsym) inner-syms)]))))
         [env []]
         entries)]
    ;; Handle dot-rest: {... . rest-pattern}
    (if dot-val
      (let [remove-keys (keys entries)
            dissoc-expr (apply list 'dissoc seed-sym remove-keys)
            ;; Rest domain: if seed is map-of, rest is also map-of; else any
            rest-dom (if (= :map-of (:kind seed-domain))
                       seed-domain
                       dom/any)]
        (if (symbol? dot-val)
          (let [env' (domain-node env dot-val rest-dom dissoc-expr)]
            [env' (conj syms dot-val)])
          ;; Nested pattern on rest
          (let [gsym (gensym "maprest_")
                env' (domain-node env gsym rest-dom dissoc-expr)
                [env'' inner-syms] (bind-pattern env' dot-val gsym rest-dom)]
            [env'' (into (conj syms gsym) inner-syms)])))
      [env syms])))

(defn bind-pattern-vec
  "Destructure a vector pattern [a b c] or [a b . xs] with domain flow.
   Supports tail decomposition: [a . mid c] where mid captures the middle.
   Extracts per-element domains from tuple/vector-of composites or tree children.
   Returns [env' bound-syms]."
  [env pattern seed-sym seed-domain]
  (let [dot-idx (.indexOf ^java.util.List pattern '.)
        has-dot? (>= dot-idx 0)
        positional (if has-dot?
                     (subvec pattern 0 dot-idx)
                     pattern)
        after-dot (when has-dot? (subvec pattern (inc dot-idx)))
        rest-pattern (when (and has-dot? (seq after-dot)) (first after-dot))
        tail-positionals (when (and has-dot? (> (count after-dot) 1))
                           (subvec after-dot 1))
        ;; Fold over head positional elements
        [env syms]
        (reduce
         (fn [[env syms] [i elem-pattern]]
           (let [elem-dom (extract-element-domain env seed-sym i seed-domain)
                 nth-expr (list 'nth seed-sym i)]
             (if (symbol? elem-pattern)
               (let [env' (domain-node env elem-pattern elem-dom nth-expr)]
                 [env' (conj syms elem-pattern)])
               (let [gsym (gensym "vec_")
                     env' (domain-node env gsym elem-dom nth-expr)
                     [env'' inner-syms] (bind-pattern env' elem-pattern gsym elem-dom)]
                 [env'' (into (conj syms gsym) inner-syms)]))))
         [env []]
         (map-indexed vector positional))]
    (cond
      ;; Tail positionals: [a . mid c d]
      tail-positionals
      (let [seed-node (when-let [found (tree/find env [seed-sym])]
                        (narrow/resolve found))
            _ (when-not (and seed-node (:vector seed-node))
                (throw (ex-info "Cannot resolve vector for tail positionals"
                                {:pattern pattern :seed seed-sym})))
            children (sort-by ::tree/name
                              (filter #(integer? (::tree/name %))
                                      (tree/children seed-node)))
            total (count children)
            head-count (count positional)
            tail-count (count tail-positionals)
            _ (when (> (+ head-count tail-count) total)
                (throw (ex-info (str "Vector has " total " elements but pattern requires at least "
                                     (+ head-count tail-count))
                                {:pattern pattern :total total})))
            middle-start head-count
            middle-end (- total tail-count)
            ;; Bind tail positionals from the end
            [env syms]
            (reduce
             (fn [[env syms] [i pat]]
               (let [idx (+ middle-end i)
                     elem-dom (extract-element-domain env seed-sym idx seed-domain)
                     nth-expr (list 'nth seed-sym idx)]
                 (if (symbol? pat)
                   (let [env' (domain-node env pat elem-dom nth-expr)]
                     [env' (conj syms pat)])
                   (let [gsym (gensym "tail_")
                         env' (domain-node env gsym elem-dom nth-expr)
                         [env'' inner-syms] (bind-pattern env' pat gsym elem-dom)]
                     [env'' (into (conj syms gsym) inner-syms)]))))
             [env syms]
             (map-indexed vector tail-positionals))
            ;; Build middle slice as vector of nth expressions
            middle-exprs (mapv (fn [i] (list 'nth seed-sym i))
                               (range middle-start middle-end))
            rest-dom (if (= :vector-of (:kind seed-domain))
                       seed-domain
                       dom/any)]
        (if rest-pattern
          (if (symbol? rest-pattern)
            (let [env' (domain-node env rest-pattern rest-dom (vec middle-exprs))]
              [env' (conj syms rest-pattern)])
            (let [gsym (gensym "mid_")
                  env' (domain-node env gsym rest-dom (vec middle-exprs))
                  [env'' inner-syms] (bind-pattern env' rest-pattern gsym rest-dom)]
              [env'' (into (conj syms gsym) inner-syms)]))
          [env syms]))

      ;; Simple rest: [a b . xs]
      rest-pattern
      (let [drop-n (count positional)
            rest-expr (list 'drop drop-n seed-sym)
            rest-dom (cond
                       ;; Tuple: remaining elements form a sub-tuple
                       (= :tuple (:kind seed-domain))
                       (let [remaining (vec (drop drop-n (:elements seed-domain)))]
                         (if (seq remaining)
                           (dom/tuple-dom remaining)
                           dom/any))
                       ;; Vector-of: rest keeps the same element type
                       (= :vector-of (:kind seed-domain))
                       seed-domain
                       :else dom/any)]
        (if (symbol? rest-pattern)
          (let [env' (domain-node env rest-pattern rest-dom rest-expr)]
            [env' (conj syms rest-pattern)])
          (let [gsym (gensym "rest_")
                env' (domain-node env gsym rest-dom rest-expr)
                [env'' inner-syms] (bind-pattern env' rest-pattern gsym rest-dom)]
            [env'' (into (conj syms gsym) inner-syms)])))

      ;; No rest — just head positionals
      :else [env syms])))

;; ════════════════════════════════════════════════════════════════
;; fn-destruct — inverse of multi-branch fn
;; ════════════════════════════════════════════════════════════════

(defn- fn-destruct-branch
  "Try to invert a single fn branch for destructuring.
   Works for branches with symbol/type-wrapper/literal params.
   If body is (and constraint... value), splits constraints from value template.
   Returns [env' bound-syms] or throws on contradiction."
  [env params body pattern-syms seed-sym seed-domain]
  (let [bind* (lazy-bind)
        ensure-node-abs* (lazy-ensure-node-abs)
        ;; Classify params and build substitution
        param-info (mapv (fn [p ps]
                           (cond
                             (symbol? p)
                             {:kind :symbol :raw-sym p :pattern-sym ps}

                             (seq? p)
                             {:kind :type-wrapper :raw-sym (last p) :wrapper (first p)
                              :pattern-sym ps}

                             (or (number? p) (string? p) (keyword? p) (boolean? p))
                             {:kind :literal :value p :pattern-sym ps}

                             ;; Complex patterns (maps, vectors) → can't invert
                             :else
                             (throw (ex-info "Cannot invert branch with complex param pattern"
                                             {:param p}))))
                         params pattern-syms)
        ;; Substitution: symbol/type-wrapper raw-syms → pattern-syms
        substitution (into {} (keep (fn [{:keys [kind raw-sym pattern-sym]}]
                                      (when (#{:symbol :type-wrapper} kind)
                                        [raw-sym pattern-sym])))
                            param-info)
        ;; Split body: if (and ...), intermediates are constraints, last is value
        [body-constraints body-value]
        (if (and (seq? body) (= 'and (first body)))
          [(butlast (rest body)) (last body)]
          [[] body])
        ;; Substitute to create destruct pattern and constraints
        destruct-pattern (substitute-expr body-value substitution)
        destruct-constraints (mapv #(substitute-expr % substitution) body-constraints)
        ;; Type constraints from wrapper params
        param-constraints (keep (fn [{:keys [kind wrapper pattern-sym]}]
                                  (when (= :type-wrapper kind)
                                    [wrapper pattern-sym]))
                                param-info)
        ;; Literal params to bind
        literal-bindings (keep (fn [{:keys [kind value pattern-sym]}]
                                 (when (= :literal kind)
                                   [pattern-sym value]))
                               param-info)
        ;; Destructure seed against inverted body pattern
        [env' bound-syms] (bind-pattern env destruct-pattern seed-sym seed-domain)
        ;; Apply body-level constraints (from and-wrapped body)
        env' (reduce (fn [e c]
                       (let [e' (bind* e c)]
                         (dissoc e' :link)))
                     env'
                     destruct-constraints)
        ;; Bind literal params' pattern-syms to their literal values
        env' (reduce (fn [e [ps val]]
                       (let [e' (bind* e ps val)]
                         (dissoc e' :link)))
                     env'
                     literal-bindings)
        ;; Apply param-level type constraints
        env' (reduce (fn [e [wrapper-head target-sym]]
                       (let [my-pos (tree/position e)
                             [e' path] (ensure-node-abs* e target-sym)
                             wrapper-node (tree/find e' [wrapper-head])
                             wrapper-resolved (when wrapper-node
                                                (narrow/resolve wrapper-node))
                             type-dom (:type-domain wrapper-resolved)]
                         (if type-dom
                           (let [d (narrow/domain-of (tree/root e') path)
                                 narrowed (dom/intersect d type-dom)]
                             (if (dom/void? narrowed)
                               (throw (ex-info "Contradiction: destructuring type constraint"
                                               {:wrapper wrapper-head
                                                :target target-sym}))
                               (let [e'' (narrow/set-domain (tree/root e') path narrowed)]
                                 (or (tree/cd e'' my-pos) e''))))
                           (let [e' (bind* e (list wrapper-head target-sym))]
                             (dissoc e' :link)))))
                     env'
                     param-constraints)]
    [env' bound-syms]))

(defn fn-destruct
  "Derive a destructor from a multi-branch fn definition.
   Tries each branch's inverse in order. First satisfiable wins."
  [env {:keys [branches] :as mufl-fn} pattern-syms seed-sym seed-domain]
  (let [n-syms (count pattern-syms)]
    (loop [remaining branches
           last-err nil]
      (if (empty? remaining)
        (throw (or last-err
                   (ex-info "No matching branch for destructuring"
                            {:pattern-syms pattern-syms})))
        (let [{:keys [params body]} (first remaining)]
          (if (not= (count params) n-syms)
            (recur (rest remaining) last-err)
            (let [result (try
                           (fn-destruct-branch env params body pattern-syms
                                               seed-sym seed-domain)
                           (catch clojure.lang.ExceptionInfo e
                             e))]
              (if (instance? Throwable result)
                (recur (rest remaining) result)
                result))))))))

(defn bind-pattern-operator
  "Dispatch an operator pattern like (ks a b), (as m pat), (point x y).
   Checks for :destruct (built-in operators), then :mufl-fn (user functions),
   then domain values (callable domains as type guards).
   Returns [env' bound-syms]."
  [env [head & args] seed-sym seed-domain]
  (let [node (tree/find env [head])
        _ (when-not node
            (throw (ex-info (str "Unresolved pattern head: " head)
                            {:pattern (cons head args)})))
        resolved (narrow/resolve node)]
    (cond
      (:destruct resolved)
      ((:destruct resolved) env args seed-sym seed-domain)

      (:mufl-fn resolved)
      (fn-destruct env (:mufl-fn resolved) args seed-sym seed-domain)

      ;; Domain value as type guard: (intvec x) ≡ (integer x) but for any domain
      ;; Binds inner pattern to seed, then applies domain constraint structurally.
      ;; Always use raw node (pre-resolve): resolve is lossy and collapses
      ;; link+map (and-composition) into plain map nodes.
      ;; resolve-schema-from-tree follows links internally, so pre-resolving is unnecessary.
      :else
      (let [try-resolve @(requiring-resolve 'mufl.schema/try-resolve-schema-from-tree)
            domain (try-resolve node)]
        (if domain
          (do
            (when (not= 1 (count args))
              (throw (ex-info (str "Domain pattern '" head "' takes exactly one argument")
                              {:pattern (cons head args) :arg-count (count args)})))
            (let [;; For simple scalar domains (non-composite, non-structural), use fast intersect path
                  scalar? (not (#{:vector-of :tuple :map-of :map-fields :intersection} (:kind domain)))]
              (if scalar?
                ;; Scalar domain: intersect seed domain + recurse (like type-domain-destruct)
                (let [narrowed (dom/intersect (or seed-domain dom/any) domain)]
                  (if (dom/void? narrowed)
                    (throw (ex-info (str "Domain guard contradiction in pattern: " head)
                                    {:domain domain :seed-domain seed-domain}))
                    (bind-pattern env (first args) seed-sym narrowed)))
                ;; Composite/structural: bind inner pattern first, then apply constraint
                (let [inner-pattern (first args)
                      [env' bound-syms] (bind-pattern env inner-pattern seed-sym seed-domain)
                      apply-constraint (fn [] @(requiring-resolve 'mufl.schema/apply-domain-constraint))
                      env'' ((apply-constraint) env' domain [seed-sym])]
                  [env'' bound-syms]))))
          (throw (ex-info (str "Cannot use '" head "' as pattern: not a function, destructor, or domain")
                          {:pattern (cons head args)})))))))
