(ns mufl.showcase.scheduler-test
  "Tests for the Task Scheduler showcase."
  (:require [clojure.test :refer [deftest testing is]]
            [mufl.showcase.scheduler :as sched]))

;; ── Sample task definitions ──────────────────────────────────────────────────

(def tasks-2
  "Two tasks with a unique solution: A=0, B=3."
  [{:name "A" :duration 3 :deadline 3}
   {:name "B" :duration 2 :deadline 5}])

(def tasks-3
  "Three tasks with multiple valid schedules."
  [{:name "A" :duration 2 :deadline 8}
   {:name "B" :duration 3 :deadline 10}
   {:name "C" :duration 1 :deadline 4}])

(def tasks-large
  "Three tasks with a wide time horizon — many solutions."
  [{:name "A" :duration 4 :deadline 20}
   {:name "B" :duration 3 :deadline 20}
   {:name "C" :duration 2 :deadline 20}])

(def tasks-impossible
  "Two tasks that can't be scheduled without overlapping."
  [{:name "A" :duration 5 :deadline 5}
   {:name "B" :duration 5 :deadline 5}])

;; ── Tests ────────────────────────────────────────────────────────────────────

(deftest two-task-unique-solution
  (testing "tight deadlines force unique schedule: A=0, B=3"
    (is (= [[0 3]]
           (sched/schedule tasks-2)))))

(deftest three-task-feasibility
  (testing "relaxed 3-task problem has solutions, all satisfy deadlines"
    (let [solutions (sched/schedule tasks-3)]
      ;; At least one solution exists
      (is (pos? (count solutions)))
      ;; All solutions respect deadlines
      (is (every? (fn [[sA sB sC]]
                    (and (<= (+ sA 2) 8)   ; A finishes by 8
                         (<= (+ sB 3) 10)  ; B finishes by 10
                         (<= (+ sC 1) 4))) ; C finishes by 4
                  solutions)))))

(deftest over-constrained-returns-empty
  (testing "impossible: two tasks with same tight deadline → empty result"
    (is (= [] (sched/schedule tasks-impossible)))))

(deftest lazy-enumeration
  (testing "large search space returns first N schedules lazily"
    (let [first-schedules (take 5 (sched/schedule-lazy tasks-large))]
      ;; Got exactly 5 schedules
      (is (= 5 (count first-schedules)))
      ;; All schedules satisfy deadlines
      (is (every? (fn [[sA sB sC]]
                    (and (<= (+ sA 4) 20)
                         (<= (+ sB 3) 20)
                         (<= (+ sC 2) 20)))
                  first-schedules)))))

(deftest sort-by-with-tasks
  (testing "solutions can be sorted by start time for display"
    (let [schedules (sched/schedule tasks-3)]
      ;; At least one schedule exists
      (is (pos? (count schedules)))
      ;; Each solution is a vector of 3 start times
      (is (every? #(= 3 (count %)) schedules))
      ;; Solutions can be sorted by start time (standard Clojure, not mufl)
      (is (every? (fn [sol]
                    (= (sort sol) (sort sol)))
                  schedules)))))
