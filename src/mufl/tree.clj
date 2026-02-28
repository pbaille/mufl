(ns mufl.tree
  "Immutable navigable tree — the environment structure for mufl."
  (:refer-clojure :exclude [find]))

;; ════════════════════════════════════════════════════════════════
;; Navigation
;; ════════════════════════════════════════════════════════════════

(defn cd
  "Navigate down to a child path. Returns nil if path doesn't exist."
  [tree path]
  (if (empty? path)
    tree
    (let [s (first path)]
      (when-let [child (get-in tree [::node s])]
        (recur (assoc child ::parent tree ::name s)
               (next path))))))

(defn parent
  "Navigate up, writing this node back into the parent."
  [{:as tree ::keys [parent name]}]
  (when parent
    (assoc-in parent [::node name]
              (dissoc tree ::parent ::name))))

(defn root
  "Navigate to root, writing back all the way up."
  [tree]
  (if-let [p (parent tree)]
    (recur p)
    tree))

(defn position
  "Path from root to here."
  [{::keys [parent name]}]
  (if parent
    (conj (position parent) name)
    []))

(defn at
  "Navigate from root to the given absolute path."
  [tree path]
  (cd (root tree) path))

;; ════════════════════════════════════════════════════════════════
;; Lexical lookup
;; ════════════════════════════════════════════════════════════════

(defn find
  "Walk upward from current position looking for path.
   Returns the node if found, nil otherwise."
  [tree path]
  (when tree
    (or (cd tree path)
        (find (parent tree) path))))

;; ════════════════════════════════════════════════════════════════
;; Mutation (returns new tree)
;; ════════════════════════════════════════════════════════════════

(defn- subnode-path
  "Convert a path to the nested ::node accessor path."
  [path]
  (interleave (repeat ::node) path))

(defn ensure-path
  "Create empty nodes along path if they don't exist."
  [tree path]
  (if (seq path)
    (update-in tree
               (subnode-path path)
               (fn [x] (or x {})))
    tree))

(defn put
  "Set fields on a node at path."
  ([tree path k v]
   (if (seq path)
     (update-in tree (subnode-path path) assoc k v)
     (assoc tree k v)))
  ([tree path m]
   (reduce-kv (fn [t k v] (put t path k v)) tree m)))

(defn upd
  "Apply f to the node at path, then navigate back."
  [tree path f]
  (let [node (cd tree path)
        result (f node)
        n (count path)]
    (nth (iterate parent result) n)))

(defn children
  "List child nodes (navigated, with ::parent set)."
  [tree]
  (when-let [node (::node tree)]
    (mapv (fn [k] (cd tree [k]))
          (keys node))))

;; ════════════════════════════════════════════════════════════════
;; Display
;; ════════════════════════════════════════════════════════════════

(defn- remove-nav
  "Remove navigation fields for display."
  [tree]
  (let [cleaned (dissoc tree ::parent ::name)]
    (if (::node cleaned)
      (update cleaned ::node
              (fn [children]
                (into {} (map (fn [[k v]] [k (remove-nav v)])) children)))
      cleaned)))

(defn show
  "Display-friendly view of the tree."
  [tree]
  (assoc (remove-nav tree) ::at (position tree)))
