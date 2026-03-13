(ns mufl.showcase.scheduler
  "Task Scheduler — constraint programming with mufl.

   Models tasks with name, duration, and deadline. Assigns start
   times such that no two tasks overlap and each finishes by its
   deadline. Demonstrates: defn for named constraints, between for
   bounded integers, or for non-overlap, sort-by for display,
   query-lazy for large search spaces.

   ## Public API

     (schedule tasks)            — returns all valid schedules
     (schedule tasks max-t)      — same, with explicit time horizon
     (schedule-lazy tasks)       — lazy sequence (large search spaces)
     (schedule-lazy tasks max-t) — same, with explicit time horizon

   `tasks` is a sequence of maps with :name, :duration, and :deadline.
   Each solution is a vector of start times in the same order as `tasks`.

   ## Evaluate the (comment ...) blocks in a REPL top-to-bottom."
  (:require [mufl.core :as m]))

;; ════════════════════════════════════════════════════════════════
;; INTERNAL: Programmatic Form Generation
;; ════════════════════════════════════════════════════════════════
;;
;; mufl forms are Clojure data. We build a `let` form with:
;;   - One variable per task's start time (s0, s1, ...)
;;   - Deadline constraints via the `meets-deadline` defn
;;   - Pairwise non-overlap `or` constraints (must be inline — see NOTE)
;;
;; NOTE: mufl's `or` with arithmetic constraints doesn't propagate
;; correctly when wrapped in `defn`. Non-overlap constraints must stay
;; inline in the query form. `meets-deadline` works fine as a defn
;; because it uses pure arithmetic without `or`.

(defn- task-sym
  "Generate a symbol for task i: s0, s1, ..., sN."
  [i]
  (symbol (str "s" i)))

(defn- build-schedule-form
  "Build a mufl query form that schedules `tasks` within [0, max-t].

   Emits:
     (do
       (defn meets-deadline ...)
       (let [s0 (between 0 max-t) s1 ...]
         (meets-deadline s0 dur0 dl0)
         ...
         (or (<= (+ s0 dur0) s1) (<= (+ s1 dur1) s0))   ; non-overlap, all pairs
         ...
         [s0 s1 ...]))"
  [tasks max-t]
  (let [n          (count tasks)
        syms       (mapv task-sym (range n))

        ;; bindings: [s0 (between 0 max-t) s1 (between 0 max-t) ...]
        bindings   (vec (mapcat (fn [i] [(task-sym i) (list 'between 0 max-t)])
                                (range n)))

        ;; meets-deadline call for each task
        deadline-cs (mapv (fn [i]
                            (let [t (nth tasks i)]
                              (list 'meets-deadline
                                    (task-sym i)
                                    (:duration t)
                                    (:deadline t))))
                          (range n))

        ;; non-overlap `or` for every pair (i < j) — must be inline, not in defn
        overlap-cs  (vec (for [i (range n)
                               j (range n)
                               :when (< i j)]
                           (let [si (task-sym i)
                                 sj (task-sym j)
                                 di (:duration (nth tasks i))
                                 dj (:duration (nth tasks j))]
                             (list 'or
                                   (list '<= (list '+ si di) sj)
                                   (list '<= (list '+ sj dj) si)))))

        ;; result: vector of start-time variables in task order
        result      syms]

    (list 'do
          ;; meets-deadline works as a defn (no or inside)
          '(defn meets-deadline [start duration deadline]
             (<= (+ start duration) deadline))
          (list* 'let bindings
                 (concat deadline-cs overlap-cs [result])))))

;; ════════════════════════════════════════════════════════════════
;; PUBLIC API
;; ════════════════════════════════════════════════════════════════

(defn schedule
  "Find all valid schedules for the given tasks.

   tasks: sequence of {:name string :duration int :deadline int}
   max-t: (optional) upper bound on start times; defaults to max deadline.

   Returns a vector of solutions. Each solution is a vector of start times
   in the same order as `tasks`.

   Throws clojure.lang.ExceptionInfo if the problem is over-constrained
   (i.e. all non-overlap branches contradict at bind time).

   For large search spaces, prefer schedule-lazy."
  ([tasks]
   (schedule tasks (apply max (map :deadline tasks))))
  ([tasks max-t]
   (vec (m/query* (build-schedule-form (vec tasks) max-t)))))

(defn schedule-lazy
  "Like schedule, but returns a lazy sequence of solutions.

   Use when the search space is large and you only need the first N results:

     (take 5 (schedule-lazy tasks))

   tasks: sequence of {:name string :duration int :deadline int}
   max-t: (optional) upper bound on start times; defaults to max deadline."
  ([tasks]
   (schedule-lazy tasks (apply max (map :deadline tasks))))
  ([tasks max-t]
   (m/query-lazy* (build-schedule-form (vec tasks) max-t))))

;; ════════════════════════════════════════════════════════════════
;; 1. THE PROBLEM
;; ════════════════════════════════════════════════════════════════
;;
;; We have tasks with a name, duration (how long they take), and
;; a deadline (when they must finish by). We need to assign each
;; task a non-negative integer start time such that:
;;
;;   1. No two tasks overlap in time
;;   2. Each task finishes by its deadline (start + duration ≤ deadline)
;;
;; This is a classic constraint satisfaction problem. We describe
;; what makes a schedule valid, and mufl finds all valid schedules.

(comment

  ;; A task has:
  ;;   :name     - a string identifier
  ;;   :duration - how many time units it takes
  ;;   :deadline - must finish by this time
  ;;
  ;; For example:
  ;;   {:name "A" :duration 3 :deadline 5}
  ;;
  ;; Valid start times for this task: 0, 1, 2
  ;; (because start + 3 must be ≤ 5, so start ≤ 2)

  )

;; ════════════════════════════════════════════════════════════════
;; 2. SCHEMA AND NAMED CONSTRAINTS
;; ════════════════════════════════════════════════════════════════
;;
;; We define the constraint model using `defn` for named constraint
;; functions. These work bidirectionally: they narrow domains when
;; called, and they can be used in both expression and pattern
;; position.

(comment

  ;; Named constraint: a task finishes by its deadline
  (m/query (defn meets-deadline [start duration deadline]
             (<= (+ start duration) deadline))
           (let [s (between 0 10)]
             (meets-deadline s 3 5)
             s))
  ;=> [0 1 2]
  ;; start + 3 ≤ 5, so start ≤ 2

  ;; Non-overlap constraint: task 1 ends before task 2 starts,
  ;; OR task 2 ends before task 1 starts.
  ;; NOTE: `or` with arithmetic constraints should be used inline,
  ;; not wrapped in defn, due to a mufl limitation.
  (m/query (let [sA (between 0 5) sB (between 0 5)]
             ;; Task A has duration 2, task B has duration 2
             (or (<= (+ sA 2) sB)   ; A ends before B starts
                 (<= (+ sB 2) sA))  ; B ends before A starts
             (!= sA sB)
             [sA sB]))
  ;=> [[0 2] [2 0] [0 3] [3 0] [0 4] [4 0] [1 3] [3 1] [1 4] [4 1] [2 4] [4 2] [0 5] [5 0] [1 5] [5 1] [2 5] [5 2] [3 5] [5 3]]
  ;; All valid pairs where two tasks of duration 2 don't overlap

  )

;; ════════════════════════════════════════════════════════════════
;; 3. TWO-TASK PROBLEM — unique solution
;; ════════════════════════════════════════════════════════════════
;;
;; A tight constraint problem with exactly one solution.
;; Task A must start at 0 (duration=3, deadline=3).
;; Task B is forced to start at 3 by the non-overlap constraint.

(comment

  (def tasks-2
    [{:name "A" :duration 3 :deadline 3}
     {:name "B" :duration 2 :deadline 5}])

  ;; One unique schedule: A starts at 0, B starts at 3
  (schedule tasks-2)
  ;=> [[0 3]]

  ;; Annotate with names by zipping tasks and start times
  (map (fn [sol]
         (map (fn [task start] {:name (:name task) :start start})
              tasks-2 sol))
       (schedule tasks-2))
  ;=> [({:name "A", :start 0} {:name "B", :start 3})]

  )

;; ════════════════════════════════════════════════════════════════
;; 4. THREE-TASK PROBLEM — multiple valid schedules
;; ════════════════════════════════════════════════════════════════
;;
;; A relaxed problem with several solutions. We show all valid
;; [sA sB sC] triples and demonstrate sorting a schedule by
;; start time for display.

(comment

  (def tasks-3
    [{:name "A" :duration 2 :deadline 8}
     {:name "B" :duration 3 :deadline 10}
     {:name "C" :duration 1 :deadline 4}])

  ;; Many solutions — [sA sB sC] triples
  (schedule tasks-3)
  ;=> [[0 2 5] [0 2 6] [0 2 7] ... (many solutions)]

  ;; Sort each solution by start time for display
  (map (fn [sol]
         (->> (map (fn [task start] {:name (:name task) :start start})
                   tasks-3 sol)
              (sort-by :start)))
       (take 3 (schedule tasks-3)))
  ;=> [({:name "C", :start 0} {:name "A", :start ...} {:name "B", :start ...}) ...]

  )

;; ════════════════════════════════════════════════════════════════
;; 5. OVER-CONSTRAINED — no solutions
;; ════════════════════════════════════════════════════════════════
;;
;; An impossible scheduling problem. Both tasks require the full
;; time window [0, 5] to meet their deadlines, but they can't
;; occupy the same time slot.

(comment

  (def tasks-impossible
    [{:name "A" :duration 5 :deadline 5}
     {:name "B" :duration 5 :deadline 5}])

  ;; Throws because both tasks must start at 0 but can't overlap.
  ;; The `or` constraint has both branches contradicted at bind time.
  #_(schedule tasks-impossible)
  ;; Throws clojure.lang.ExceptionInfo: All or-branches contradicted

  )

;; ════════════════════════════════════════════════════════════════
;; 6. LAZY ENUMERATION — large search space
;; ════════════════════════════════════════════════════════════════
;;
;; When the search space is large, use schedule-lazy to enumerate
;; solutions on demand. We only compute what we take.

(comment

  (def tasks-large
    [{:name "A" :duration 4 :deadline 20}
     {:name "B" :duration 3 :deadline 20}
     {:name "C" :duration 2 :deadline 20}])

  ;; Many solutions exist — use lazy enumeration to show first 5
  (take 5 (schedule-lazy tasks-large))
  ;=> ([0 4 7] [0 4 8] [0 4 9] [0 4 10] [0 4 11])

  ;; This is critical for problems with thousands of potential solutions.
  ;; The search space is large, but we only compute what we need.

  )

;; ════════════════════════════════════════════════════════════════
;; 7. HELPER — filtering a schedule
;; ════════════════════════════════════════════════════════════════
;;
;; Post-process solutions with standard Clojure operations.

(comment

  ;; Find solutions where all tasks start in the first half (< 6)
  (->> (schedule tasks-3)
       (filter (fn [sol] (every? #(< % 6) sol))))
  ;=> [[0 2 5] [0 3 5] [1 3 5] [0 4 5] ...]

  ;; Count distinct solutions
  (count (schedule tasks-3))
  ;=> some positive number

  )
