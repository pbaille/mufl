(ns mufl.showcase.sudoku
  "Sudoku solver — canonical constraint-satisfaction benchmark for mufl.
   
   Demonstrates:
   - Programmatic form generation (building mufl code as Clojure data)
   - Domain variables with `between`
   - All-different constraints with `distinct`
   - Using `query1*` for single-solution search
   
   A Sudoku puzzle is represented as a flat vector (row-major order):
   - 16 elements for 4×4 mini-Sudoku
   - 81 elements for 9×9 standard Sudoku
   - 0 = unknown cell, 1-n = given value
   
   The solver generates a mufl `let` form with:
   - One variable per cell (bound to given value or `(between 1 n)`)
   - Row constraints: each row has distinct values 1..n
   - Column constraints: each column has distinct values 1..n
   - Box constraints: each √n × √n box has distinct values 1..n
   
   This is a showcase of constraint propagation at scale. Mufl's domain
   narrowing eliminates invalid values automatically; DFS search handles
   remaining choices."
  (:require [mufl.core :as m]))

;; ════════════════════════════════════════════════════════════════
;; CORE: Programmatic Form Generation
;; ════════════════════════════════════════════════════════════════
;;
;; The key insight: mufl forms are just Clojure data. We build a
;; `let` form with bindings and constraints, then pass it to query1*.

(defn- cell-sym
  "Generate a symbol for cell i: c0, c1, ..., c80."
  [i]
  (symbol (str "c" i)))

(defn- build-form
  "Build a mufl let-form for an n×n Sudoku puzzle.
   
   puzzle: vector of n² values (0=unknown, 1-n=given)
   
   Returns a Clojure list representing:
     (let [c0 (between 1 n) ... c15 3 ... board [c0 c1 ... c15]]
       (distinct [c0 c1 c2 c3])   ; row constraints
       (distinct [c0 c4 c8 c12])  ; column constraints
       (distinct [c0 c1 c4 c5])   ; box constraints
       board)                      ; return the board vector"
  [n puzzle]
  (let [box  (int (Math/sqrt n))
        n2   (* n n)
        vars (mapv cell-sym (range n2))
        
        ;; Build bindings: [(between 1 n) for unknowns, literal for givens]
        cell-bindings (vec (mapcat (fn [i]
                                     [(cell-sym i)
                                      (let [v (nth puzzle i)]
                                        (if (zero? v)
                                          (list 'between 1 n)
                                          v))])
                                   (range n2)))
        
        ;; Final binding: board = [c0 c1 ... cN]
        all-bindings (into cell-bindings ['board (vec vars)])
        
        ;; Row constraints: each row has n cells
        row-cs (for [r (range n)]
                 (list 'distinct
                       (mapv #(cell-sym (+ (* r n) %)) (range n))))
        
        ;; Column constraints: cells spaced by n
        col-cs (for [c (range n)]
                 (list 'distinct
                       (mapv #(cell-sym (+ c (* % n))) (range n))))
        
        ;; Box constraints: √n × √n sub-grids
        box-cs (for [br (range box) bc (range box)]
                 (list 'distinct
                       (vec (for [dr (range box) dc (range box)]
                              (cell-sym (+ (* (+ (* br box) dr) n)
                                           (* bc box) dc))))))]
    
    ;; Assemble: (let bindings constraint1 constraint2 ... board)
    (list* 'let all-bindings
           (concat row-cs col-cs box-cs ['board]))))

;; ════════════════════════════════════════════════════════════════
;; 1. MINI-SUDOKU (4×4) — validate the approach
;; ════════════════════════════════════════════════════════════════
;;
;; A 4×4 Sudoku has 4 rows, 4 columns, 4 2×2 boxes. Each contains
;; the values 1, 2, 3, 4 exactly once. This is a tiny search space
;; — perfect for validating our constraint model.

(def mini-puzzle
  "A 4×4 Sudoku puzzle (row-major):
     1 . . .
     . . . 2
     . 1 . .
     . 3 . ."
  [1 0 0 0
   0 0 0 2
   0 1 0 0
   0 3 0 0])

(defn solve-mini
  "Solve a 4×4 Sudoku puzzle.
   
   puzzle: 16-element vector (0=unknown, 1-4=given)
   
   Returns: 16-element solution vector, or nil if unsolvable."
  [puzzle]
  (when-let [[sol _state] (m/query1* (build-form 4 puzzle))]
    sol))

(comment
  ;; Solve the sample 4×4 puzzle
  (solve-mini mini-puzzle)
  ;=> [1 2 3 4  3 4 1 2  2 1 4 3  4 3 2 1]
  
  ;; Impossible puzzle (two 1s in row 0)
  (solve-mini [1 1 0 0  0 0 0 0  0 0 0 0  0 0 0 0])
  ;=> nil
  
  ;; Timing (should be instant)
  (time (solve-mini mini-puzzle))
  )

;; ════════════════════════════════════════════════════════════════
;; 2. FULL SUDOKU (9×9)
;; ════════════════════════════════════════════════════════════════
;;
;; Standard 9×9 Sudoku: 9 rows, 9 columns, 9 3×3 boxes.
;; Each contains the values 1-9 exactly once.
;;
;; Well-posed puzzles (one unique solution) solve quickly via
;; constraint propagation. Hard puzzles may require DFS search.

(def easy-puzzle
  "A well-known easy 9×9 Sudoku (many givens, unique solution)."
  [5 3 0 0 7 0 0 0 0
   6 0 0 1 9 5 0 0 0
   0 9 8 0 0 0 0 6 0
   8 0 0 0 6 0 0 0 3
   4 0 0 8 0 3 0 0 1
   7 0 0 0 2 0 0 0 6
   0 6 0 0 0 0 2 8 0
   0 0 0 4 1 9 0 0 5
   0 0 0 0 8 0 0 7 9])

(def easy-solution
  "The unique solution to easy-puzzle."
  [5 3 4 6 7 8 9 1 2
   6 7 2 1 9 5 3 4 8
   1 9 8 3 4 2 5 6 7
   8 5 9 7 6 1 4 2 3
   4 2 6 8 5 3 7 9 1
   7 1 3 9 2 4 8 5 6
   9 6 1 5 3 7 2 8 4
   2 8 7 4 1 9 6 3 5
   3 4 5 2 8 6 1 7 9])

(defn solve
  "Solve a 9×9 Sudoku puzzle.
   
   puzzle: 81-element vector (0=unknown, 1-9=given)
   
   Returns: 81-element solution vector, or nil if unsolvable."
  [puzzle]
  (when-let [[sol _state] (m/query1* (build-form 9 puzzle))]
    sol))

(comment
  ;; Solve the easy puzzle
  (solve easy-puzzle)
  ;=> [5 3 4 6 7 8 9 1 2 ...]
  
  ;; Verify against known solution
  (= (solve easy-puzzle) easy-solution)
  ;=> true
  
  ;; Timing (measure performance)
  (time (solve easy-puzzle))
  )

;; ════════════════════════════════════════════════════════════════
;; 3. VALIDATION
;; ════════════════════════════════════════════════════════════════
;;
;; Structural validator: checks that a solved board satisfies all
;; Sudoku constraints (rows, columns, boxes all contain 1..n).

(defn valid-solution?
  "Check if a solved n×n Sudoku board is structurally valid.
   
   n:   board size (4 or 9)
   sol: n²-element vector of values
   
   Returns true if all rows, columns, and boxes contain distinct values 1..n."
  [n sol]
  (let [box (int (Math/sqrt n))
        group-ok? (fn [idxs]
                    (= (set (map #(nth sol %) idxs))
                       (set (range 1 (inc n)))))
        
        ;; Row r: indices r*n, r*n+1, ..., r*n+(n-1)
        rows (for [r (range n)]
               (map #(+ (* r n) %) (range n)))
        
        ;; Column c: indices c, c+n, c+2n, ..., c+(n-1)*n
        cols (for [c (range n)]
               (map #(+ c (* % n)) (range n)))
        
        ;; Box (br, bc): indices in √n × √n sub-grid
        boxes (for [br (range box) bc (range box)]
                (for [dr (range box) dc (range box)]
                  (+ (* (+ (* br box) dr) n) (* bc box) dc)))]
    
    (every? group-ok? (concat rows cols boxes))))

(comment
  ;; Validate the easy solution
  (valid-solution? 9 easy-solution)
  ;=> true
  
  ;; Validate a mini solution
  (valid-solution? 4 [1 2 3 4  3 4 1 2  2 1 4 3  4 3 2 1])
  ;=> true
  
  ;; Invalid board (duplicate in row 0)
  (valid-solution? 4 [1 1 3 4  3 4 1 2  2 1 4 3  4 3 2 1])
  ;=> false
  )

;; ════════════════════════════════════════════════════════════════
;; PERFORMANCE FINDINGS
;; ════════════════════════════════════════════════════════════════
;;
;; Measured on:
;; - clj -e "(require '[mufl.showcase.sudoku :as s]) (time (s/solve-mini s/mini-puzzle))"
;; - clj -e "(require '[mufl.showcase.sudoku :as s]) (time (s/solve s/easy-puzzle))"
;;
;; Results:
;; - 4×4 mini: ~8.8ms   (tiny search space, pure propagation)
;; - 9×9 easy: ~31.6ms  (many givens → strong propagation, minimal DFS)
;;
;; Mufl's constraint propagation is highly effective for well-posed
;; Sudoku puzzles. The easy 9×9 has enough givens that most cells
;; are determined by propagation alone, requiring minimal search.
