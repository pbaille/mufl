(ns mufl.showcase.sudoku-test
  "Tests for the Sudoku solver showcase."
  (:require [clojure.test :refer [deftest testing is]]
            [mufl.showcase.sudoku :as sudoku]))

(deftest mini-sudoku-solves
  (testing "4×4 puzzle returns valid solution"
    (let [sol (sudoku/solve-mini sudoku/mini-puzzle)]
      (is (some? sol) "Should find a solution")
      (is (= 16 (count sol)) "Solution should have 16 cells")
      (is (sudoku/valid-solution? 4 sol) "Solution should be structurally valid"))))

(deftest mini-sudoku-returns-nil-on-impossible
  (testing "Impossible 4×4 puzzle returns nil"
    ;; Two 1s in the first row — contradiction
    (let [impossible [1 1 0 0  0 0 0 0  0 0 0 0  0 0 0 0]
          sol (sudoku/solve-mini impossible)]
      (is (nil? sol) "Impossible puzzle should return nil"))))

(deftest full-sudoku-easy
  (testing "9×9 easy puzzle matches known solution"
    (let [sol (sudoku/solve sudoku/easy-puzzle)]
      (is (some? sol) "Should find a solution")
      (is (= 81 (count sol)) "Solution should have 81 cells")
      (is (= sol sudoku/easy-solution) "Should match the known solution"))))

(deftest full-sudoku-structural
  (testing "9×9 easy puzzle produces valid solution"
    (let [sol (sudoku/solve sudoku/easy-puzzle)]
      (is (some? sol) "Should find a solution")
      (is (sudoku/valid-solution? 9 sol) "Solution should be structurally valid"))))

(deftest validation-works
  (testing "Validator correctly identifies valid and invalid boards"
    ;; Valid 4×4
    (is (sudoku/valid-solution? 4 [1 2 3 4  3 4 1 2  2 1 4 3  4 3 2 1]))
    
    ;; Invalid 4×4 (duplicate 1 in row 0)
    (is (not (sudoku/valid-solution? 4 [1 1 3 4  3 4 1 2  2 1 4 3  4 3 2 1])))
    
    ;; Valid 9×9 (the known solution)
    (is (sudoku/valid-solution? 9 sudoku/easy-solution))))
