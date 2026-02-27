(ns mufl.scratch
  (:require [mufl.core :refer [query query1 query-lazy]]))

;; fresh with typed declarations
(query
 (fresh [(integer x) (integer y)]
        (>= x 0) (>= y 0)
        (= (+ x y) 8)
        [x y]))
;=> [[8 0] [7 1] [6 2] [5 3] [4 4] [3 5] [2 6] [1 7] [0 8]]

;; equivalent: bare symbols + body constraints
(query
 (fresh [x y]
        (integer x) (integer y)
        (>= x 0) (>= y 0)
        (= (+ x y) 8)
        [x y]))

;; let with nullary constructors
(query
 (let [x (integer) y (integer)]
   (and (>= x 0) (>= y 0)
        (= (+ x y) 8)
        [x y])))

;; mixed: typed + free
(query
 (fresh [(integer x) y]
        (>= x 1) (<= x 3)
        (= y "hello")
        [x y]))
;=> [[1 "hello"] [2 "hello"] [3 "hello"]]
