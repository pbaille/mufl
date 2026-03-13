(ns mufl.schema
  "Domain resolution and structural constraint application.

   Resolves domain expressions (type names, map/vector shapes, composites)
   into domain values from the domain algebra, and applies them as
   constraints to tree nodes.

   After the domain algebra extension, all type information is represented
   as domain values — there is no separate 'schema' representation."
  (:require [mufl.tree :as tree]
            [mufl.domain :as dom]
            [mufl.narrow :as narrow]))

;; Re-use the canonical definition from narrow
(def ^:private composite-kinds narrow/composite-kinds)

(defn resolve-schema-from-tree
  "Interpret a bound tree node as a domain value.
   Walks the tree structure to reconstruct a domain.
   Handles: type-domains, scalar domains, composite domains,
   map nodes, vector nodes, links, and link+map (and-composition)."
  [node]
  (cond
    ;; Type domain primitive (string, integer, etc.)
    (:type-domain node)
    (:type-domain node)

    ;; Link AND map → and-composition (e.g., from (and Person {:company string}))
    (and (:link node) (:map node))
    (let [linked-node (narrow/resolve (tree/at node (:link node)))
          linked-domain (resolve-schema-from-tree linked-node)
          local-children (tree/kw-children node)
          local-fields (into {} (map (fn [child]
                                       [(::tree/name child)
                                        (resolve-schema-from-tree (narrow/resolve child))])
                                     local-children))]
      (dom/intersection-dom [linked-domain (dom/map-fields-dom local-fields)]))

    ;; Link only → follow it
    (:link node)
    (resolve-schema-from-tree (narrow/resolve (tree/at node (:link node))))

    ;; Domain value (between, one-of, composite vector-of/tuple/map-of, etc.)
    (:domain node)
    (:domain node)

    ;; Map node → structural map-fields domain
    (:map node)
    (let [children (tree/kw-children node)
          fields (into {} (map (fn [child]
                                 [(::tree/name child)
                                  (resolve-schema-from-tree (narrow/resolve child))])
                               children))]
      (dom/map-fields-dom fields))

    ;; Vector node → tuple domain
    (:vector node)
    (let [children (tree/int-children node)
          elem-domains (mapv #(resolve-schema-from-tree (narrow/resolve %)) children)]
      (dom/tuple-dom elem-domains))

    :else
    (throw (ex-info "Cannot interpret node as domain"
                    {:node (select-keys node [:domain :link :map :vector ::tree/name])}))))

(defn try-resolve-schema-from-tree
  "Like resolve-schema-from-tree but returns nil if the node cannot be
   interpreted as a domain, instead of throwing."
  [node]
  (try
    (resolve-schema-from-tree node)
    (catch clojure.lang.ExceptionInfo _ nil)))

(defn resolve-domain-schema
  "Resolve a domain expression into a domain value.
   
   Domain expression forms:
   - `string`, `integer`, etc. → type domain reference
   - `(range lo hi)` → int-range domain
   - `(one-of a b c)` → finite domain
   - `{:key1 type1 :key2 type2}` → map-fields domain
   - `[type1 type2 ...]` → tuple domain
   - `(and DomainName {:extra-key type})` → intersection domain
   - `DomainName` → reference to another defined domain"
  [env schema-expr]
  (cond
    ;; Symbol → resolve to type domain or bound value
    (symbol? schema-expr)
    (if-let [found (tree/find env [schema-expr])]
      (resolve-schema-from-tree found)
      (throw (ex-info (str "Unresolved domain type: " schema-expr)
                      {:sym schema-expr})))

    ;; Vector literal → tuple domain
    (vector? schema-expr)
    (let [elem-domains (mapv #(resolve-domain-schema env %) schema-expr)]
      (dom/tuple-dom elem-domains))

    ;; Map literal → map-fields domain
    (map? schema-expr)
    (dom/map-fields-dom
     (into {} (map (fn [[k v]]
                     [k (resolve-domain-schema env v)])
                   schema-expr)))

    ;; List → dispatch on head
    (seq? schema-expr)
    (let [[head & args] schema-expr]
      (case head
        between (let [[lo hi] args]
                  (dom/int-range lo hi))
        one-of (dom/finite (set args))
        and (dom/intersection-dom (mapv #(resolve-domain-schema env %) args))

        ;; Type constructor domains for collections
        vector-of (let [[elem-type] args
                         elem-domain (resolve-domain-schema env elem-type)]
                    (dom/vector-of-dom elem-domain))

        tuple (let [[types-vec] args]
                (when-not (vector? types-vec)
                  (throw (ex-info "tuple schema requires a vector of types"
                                  {:form schema-expr})))
                (let [elem-domains (mapv #(resolve-domain-schema env %) types-vec)]
                  (dom/tuple-dom elem-domains)))

        map-of (let [[key-type val-type] args
                      key-domain (resolve-domain-schema env key-type)
                      val-domain (resolve-domain-schema env val-type)]
                 (dom/map-of-dom key-domain val-domain))

        (throw (ex-info (str "Unknown domain schema form: " head)
                        {:form schema-expr}))))

    :else
    (throw (ex-info "Invalid domain schema expression" {:expr schema-expr}))))

(defn- domain-is-scalar?
  "Is this domain a scalar (non-composite, non-structural) domain?"
  [d]
  (not (composite-kinds (:kind d))))

(defn apply-domain-constraint
  "Apply a domain as a constraint to a pre-resolved target path.
   target-path should be an absolute path from root.
   Returns the env at the same position it was called from.
   
   Now works with domain values directly — no separate schema representation."
  [env domain target-path]
  (let [my-pos (tree/position env)]
    (letfn [(apply-dom [env target-path domain]
              (case (:kind domain)
                ;; Scalar domain → intersect
                (:void :any :single :finite :type :range :spiral)
                (let [env-root (tree/root env)
                      current-dom (narrow/domain-of env-root target-path)
                      narrowed (dom/intersect current-dom domain)]
                  (if (dom/void? narrowed)
                    (throw (ex-info "Domain constraint contradiction"
                                    {:path target-path :domain domain
                                     :current current-dom}))
                    (let [env' (narrow/set-domain env-root target-path narrowed)]
                      (or (narrow/propagate-watchers env' target-path)
                          (throw (ex-info "Contradiction during domain propagation"
                                          {:path target-path}))))))

                ;; Composite domains (vector-of, tuple, map-of) → apply structural constraints
                (:vector-of :tuple :map-of)
                (let [env-root (tree/root env)
                      result (narrow/apply-composite-constraint env-root target-path domain)]
                  (if-not result
                    (throw (ex-info "Domain constraint contradiction"
                                    {:path target-path :domain domain}))
                    (let [env' (:env result)
                          changed (:changed result)]
                      ;; Propagate watchers for all changed paths
                      (reduce (fn [e p]
                                (or (narrow/propagate-watchers e p)
                                    (throw (ex-info "Contradiction during domain propagation"
                                                    {:path p}))))
                              env'
                              changed))))

                ;; Map-fields domain → ensure target is a map, constrain each field
                :map-fields
                (let [env-root (tree/root env)
                      target-node (narrow/resolve-at env-root target-path)]
                  (when-not (:map target-node)
                    (throw (ex-info "Domain constraint: expected a map"
                                    {:path target-path})))
                  (let [target-resolved-path (tree/position target-node)]
                    (reduce (fn [env-root [field-key field-domain]]
                              (let [child-path (conj target-resolved-path field-key)
                                    child (tree/cd env-root child-path)]
                                (if child
                                  (let [resolved-child (narrow/resolve (tree/cd env-root child-path))
                                        resolved-path (tree/position resolved-child)]
                                    (apply-dom (tree/cd env-root my-pos) resolved-path field-domain))
                                  (throw (ex-info (str "Domain constraint: missing field " field-key)
                                                  {:field field-key :path target-path})))))
                            env-root
                            (:fields domain))))

                ;; Intersection domain → apply each sub-domain in sequence
                :intersection
                (reduce (fn [env-root sub-domain]
                          (apply-dom (tree/cd env-root my-pos) target-path sub-domain))
                        (tree/root env)
                        (:domains domain))

                ;; Fallback for other domain kinds — try as scalar intersect
                (let [env-root (tree/root env)
                      current-dom (narrow/domain-of env-root target-path)
                      narrowed (dom/intersect current-dom domain)]
                  (if (dom/void? narrowed)
                    (throw (ex-info "Domain constraint contradiction"
                                    {:path target-path :domain domain
                                     :current current-dom}))
                    (let [env' (narrow/set-domain env-root target-path narrowed)]
                      (or (narrow/propagate-watchers env' target-path)
                          (throw (ex-info "Contradiction during domain propagation"
                                          {:path target-path}))))))))]

      ;; target-path is already resolved by caller
      (let [resolved-target (narrow/resolve-at (tree/root env) target-path)
            resolved-path (tree/position resolved-target)
            result-root (apply-dom env resolved-path domain)]
        (or (tree/cd result-root my-pos) result-root)))))
