(ns mufl.schema
  "Domain schema resolution and structural constraint application.

   Resolves schema expressions (type names, map/vector shapes, composites)
   into schema maps, and applies them as constraints to tree nodes."
  (:require [mufl.tree :as tree]
            [mufl.domain :as dom]
            [mufl.narrow :as narrow]))

;; Late binding for bind functions (breaks circular dep)
(defn- lazy-ensure-node-abs []
  @(requiring-resolve 'mufl.bind/ensure-node-abs))

;; Re-use the canonical definition from narrow
(def ^:private composite-kinds narrow/composite-kinds)

(defn resolve-schema-from-tree
  "Interpret a bound tree node as a domain schema.
   Walks the tree structure to reconstruct a schema map.
   Handles: type-domains, scalar domains, composite domains,
   map nodes, vector nodes, links, and link+map (and-composition)."
  [node]
  (cond
    ;; Type domain primitive (string, integer, etc.)
    (:type-domain node)
    {:kind :type-constraint :domain (:type-domain node)}

    ;; Link AND map → and-composition (e.g., from (and Person {:company string}))
    (and (:link node) (:map node))
    (let [linked-node (narrow/resolve (tree/at node (:link node)))
          linked-schema (resolve-schema-from-tree linked-node)
          local-children (filter #(keyword? (::tree/name %)) (tree/children node))
          local-fields (into {} (map (fn [child]
                                       [(::tree/name child)
                                        (resolve-schema-from-tree (narrow/resolve child))])
                                     local-children))]
      {:kind :and-schema
       :schemas [linked-schema {:kind :map-schema :fields local-fields}]})

    ;; Link only → follow it
    (:link node)
    (resolve-schema-from-tree (narrow/resolve (tree/at node (:link node))))

    ;; Domain value (between, one-of, composite vector-of/tuple/map-of, etc.)
    (:domain node)
    {:kind :type-constraint :domain (:domain node)}

    ;; Map node → structural map schema
    (:map node)
    (let [children (filter #(keyword? (::tree/name %)) (tree/children node))
          fields (into {} (map (fn [child]
                                 [(::tree/name child)
                                  (resolve-schema-from-tree (narrow/resolve child))])
                               children))]
      {:kind :map-schema :fields fields})

    ;; Vector node → tuple schema
    (:vector node)
    (let [children (sort-by ::tree/name
                            (filter #(integer? (::tree/name %)) (tree/children node)))
          elem-schemas (mapv #(resolve-schema-from-tree (narrow/resolve %)) children)]
      (if (every? #(= :type-constraint (:kind %)) elem-schemas)
        {:kind :type-constraint
         :domain (dom/tuple-dom (mapv :domain elem-schemas))}
        {:kind :tuple-schema
         :element-schemas elem-schemas}))

    :else
    (throw (ex-info "Cannot interpret node as domain schema"
                    {:node (select-keys node [:domain :link :map :vector ::tree/name])}))))

(defn resolve-domain-schema
  "Resolve a domain schema expression into a schema map.
   
   Schema forms:
   - `string`, `integer`, etc. → type domain reference
   - `(range lo hi)` → int-range domain
   - `(one-of a b c)` → finite domain
   - `{:key1 type1 :key2 type2}` → map schema (structural domain)
   - `[type1 type2 ...]` → tuple schema (sugar for (tuple [...]))
   - `(and DomainName {:extra-key type})` → composed domain (intersection)
   - `DomainName` → reference to another defined domain"
  [env schema-expr]
  (cond
    ;; Symbol → resolve to type domain or bound value
    (symbol? schema-expr)
    (if-let [found (tree/find env [schema-expr])]
      (resolve-schema-from-tree found)
      (throw (ex-info (str "Unresolved domain type: " schema-expr)
                      {:sym schema-expr})))

    ;; Vector literal → tuple schema (sugar for (tuple [t1 t2 ...]))
    (vector? schema-expr)
    (let [elem-schemas (mapv #(resolve-domain-schema env %) schema-expr)]
      (if (every? #(= :type-constraint (:kind %)) elem-schemas)
        {:kind :type-constraint
         :domain (dom/tuple-dom (mapv :domain elem-schemas))}
        {:kind :tuple-schema
         :element-schemas elem-schemas}))

    ;; Map literal → structural (map) domain
    (map? schema-expr)
    {:kind :map-schema
     :fields (into {} (map (fn [[k v]]
                             [k (resolve-domain-schema env v)])
                           schema-expr))}

    ;; List → dispatch on head
    (seq? schema-expr)
    (let [[head & args] schema-expr]
      (case head
        between (let [[lo hi] args]
                  {:kind :type-constraint :domain (dom/int-range lo hi)})
        one-of {:kind :type-constraint :domain (dom/finite (set args))}
        and {:kind :and-schema
             :schemas (mapv #(resolve-domain-schema env %) args)}

        ;; Type constructor schemas for collections
        vector-of (let [[elem-type] args
                         elem-schema (resolve-domain-schema env elem-type)]
                    (if (= :type-constraint (:kind elem-schema))
                      {:kind :type-constraint
                       :domain (dom/vector-of-dom (:domain elem-schema))}
                      {:kind :vector-of-schema
                       :element-schema elem-schema}))

        tuple (let [[types-vec] args]
                (when-not (vector? types-vec)
                  (throw (ex-info "tuple schema requires a vector of types"
                                  {:form schema-expr})))
                (let [elem-schemas (mapv #(resolve-domain-schema env %) types-vec)]
                  (if (every? #(= :type-constraint (:kind %)) elem-schemas)
                    {:kind :type-constraint
                     :domain (dom/tuple-dom (mapv :domain elem-schemas))}
                    {:kind :tuple-schema
                     :element-schemas elem-schemas})))

        map-of (let [[key-type val-type] args
                      key-schema (resolve-domain-schema env key-type)
                      val-schema (resolve-domain-schema env val-type)]
                 (if (and (= :type-constraint (:kind key-schema))
                          (= :type-constraint (:kind val-schema)))
                   {:kind :type-constraint
                    :domain (dom/map-of-dom (:domain key-schema) (:domain val-schema))}
                   {:kind :map-of-schema
                    :key-schema key-schema
                    :value-schema val-schema}))

        (throw (ex-info (str "Unknown domain schema form: " head)
                        {:form schema-expr}))))

    :else
    (throw (ex-info "Invalid domain schema expression" {:expr schema-expr}))))

(defn apply-domain-constraint
  "Apply a domain schema as a constraint to an argument.
   (narrow target Schema) constrains target to match the schema.
   Returns the env at the same position it was called from."
  [env schema args]
  (let [my-pos (tree/position env)
        target-expr (first args)]
    (when (not= 1 (count args))
      (throw (ex-info "Domain constraint takes exactly one argument"
                      {:args args})))
    (letfn [(apply-schema [env target-path schema]
              (case (:kind schema)
                ;; Simple type/value constraint → intersect domain
                :type-constraint
                (let [constraint-dom (:domain schema)]
                  (if (composite-kinds (:kind constraint-dom))
                    ;; Composite domain → apply structural constraints
                    (let [env-root (tree/root env)
                          result (narrow/apply-composite-constraint env-root target-path constraint-dom)]
                      (if-not result
                        (throw (ex-info "Domain constraint contradiction"
                                        {:path target-path :domain constraint-dom}))
                        (let [env' (:env result)
                              changed (:changed result)]
                          ;; Propagate watchers for all changed paths
                          (reduce (fn [e path]
                                    (let [node (tree/cd e path)
                                          watchers (:watched-by node)]
                                      (if (seq watchers)
                                        (or (narrow/propagate e (vec watchers))
                                            (throw (ex-info "Contradiction during domain propagation"
                                                            {:path path})))
                                        e)))
                                  env'
                                  changed))))
                    ;; Scalar domain → existing intersect logic
                    (let [env-root (tree/root env)
                          current-dom (narrow/domain-of env-root target-path)
                          narrowed (dom/intersect current-dom constraint-dom)]
                      (if (dom/void? narrowed)
                        (throw (ex-info "Domain constraint contradiction"
                                        {:path target-path :domain constraint-dom
                                         :current current-dom}))
                        (let [env' (narrow/set-domain env-root target-path narrowed)]
                          ;; Propagate any watchers
                          (let [node (tree/cd env' target-path)
                                watchers (:watched-by node)]
                            (if (seq watchers)
                              (or (narrow/propagate env' (vec watchers))
                                  (throw (ex-info "Contradiction during domain propagation"
                                                  {:path target-path})))
                              env')))))))

                ;; Map schema → ensure target is a map, constrain each field
                :map-schema
                (let [env-root (tree/root env)
                      target-node (narrow/resolve-at env-root target-path)]
                  (when-not (:map target-node)
                    (throw (ex-info "Domain constraint: expected a map"
                                    {:path target-path})))
                  (let [target-resolved-path (tree/position target-node)]
                    (reduce (fn [env-root [field-key field-schema]]
                              (let [child-path (conj target-resolved-path field-key)
                                    child (tree/cd env-root child-path)]
                                (if child
                                  (let [resolved-child (narrow/resolve (tree/cd env-root child-path))
                                        resolved-path (tree/position resolved-child)]
                                    (apply-schema (tree/cd env-root my-pos) resolved-path field-schema))
                                  (throw (ex-info (str "Domain constraint: missing field " field-key)
                                                  {:field field-key :path target-path})))))
                            env-root
                            (:fields schema))))

                ;; And-schema → apply each sub-schema in sequence
                :and-schema
                (reduce (fn [env-root sub-schema]
                          (apply-schema (tree/cd env-root my-pos) target-path sub-schema))
                        (tree/root env)
                        (:schemas schema))

                ;; Vector-of schema → ensure target is a vector, constrain each element
                :vector-of-schema
                (let [env-root (tree/root env)
                      target-node (narrow/resolve-at env-root target-path)]
                  (when-not (:vector target-node)
                    (throw (ex-info "Domain constraint: expected a vector for vector-of"
                                    {:path target-path})))
                  (let [target-resolved-path (tree/position target-node)
                        children (sort-by ::tree/name
                                          (filter #(integer? (::tree/name %))
                                                               (tree/children target-node)))
                        elem-schema (:element-schema schema)]
                    (reduce (fn [env-root child]
                              (let [resolved-child (narrow/resolve child)
                                    resolved-path (tree/position resolved-child)]
                                (apply-schema (tree/cd env-root my-pos) resolved-path elem-schema)))
                            env-root
                            children)))

                ;; Tuple schema → ensure vector, check length, constrain per position
                :tuple-schema
                (let [env-root (tree/root env)
                      target-node (narrow/resolve-at env-root target-path)]
                  (when-not (:vector target-node)
                    (throw (ex-info "Domain constraint: expected a vector for tuple"
                                    {:path target-path})))
                  (let [target-resolved-path (tree/position target-node)
                        children (sort-by ::tree/name
                                          (filter #(integer? (::tree/name %))
                                                               (tree/children target-node)))
                        elem-schemas (:element-schemas schema)
                        n (count children)
                        expected (count elem-schemas)]
                    (when (not= n expected)
                      (throw (ex-info (str "tuple domain: vector has " n " elements but tuple specifies " expected)
                                      {:actual n :expected expected})))
                    (reduce (fn [env-root [idx elem-schema]]
                              (let [child (nth children idx)
                                    resolved-child (narrow/resolve child)
                                    resolved-path (tree/position resolved-child)]
                                (apply-schema (tree/cd env-root my-pos) resolved-path elem-schema)))
                            env-root
                            (map-indexed vector elem-schemas))))

                ;; Map-of schema → ensure target is a map, constrain all entries
                :map-of-schema
                (let [env-root (tree/root env)
                      target-node (narrow/resolve-at env-root target-path)]
                  (when-not (:map target-node)
                    (throw (ex-info "Domain constraint: expected a map for map-of"
                                    {:path target-path})))
                  (let [children (tree/children target-node)
                        key-schema (:key-schema schema)
                        val-schema (:value-schema schema)
                        key-dom (when (= :type-constraint (:kind key-schema))
                                  (:domain key-schema))]
                    ;; Validate keys
                    (doseq [child children]
                      (let [k (::tree/name child)]
                        (when (and key-dom (not (dom/contains-val? key-dom k)))
                          (throw (ex-info (str "map-of domain: key " k " does not match key type")
                                          {:key k})))))
                    ;; Constrain values
                    (reduce (fn [env-root child]
                              (let [resolved-child (narrow/resolve child)
                                    resolved-path (tree/position resolved-child)]
                                (apply-schema (tree/cd env-root my-pos) resolved-path val-schema)))
                            env-root
                            children)))))]

      ;; Resolve the target argument (uses late-bound ensure-node-abs from bind)
      (let [ensure-node-abs* (lazy-ensure-node-abs)
            [env' target-path] (ensure-node-abs* env target-expr)
            ;; Follow links to the real target
            resolved-target (narrow/resolve-at (tree/root env') target-path)
            resolved-path (tree/position resolved-target)
            result-root (apply-schema env' resolved-path schema)]
        (or (tree/cd result-root my-pos) result-root)))))
