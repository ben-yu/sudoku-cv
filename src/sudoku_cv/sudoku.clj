(ns sudoku-cv.sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= (value-at board coord) 0)))

(defn row-values [board coord]
  (set (get board (get coord 0))))

(defn col-values [board coord]
  (set (map (fn [x] (get x (get coord 1))) board)))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
        [x y]))

(defn block-values [board coord]
  (let [col-3 (fn [x] (- x (mod x 3)))
        top-left (map (fn [x] (if (< (col-3 x) 0)
                                  0
                                  (col-3 x))) coord)]
          (set (map (fn [x] (value-at board x))
           (for [offset (coord-pairs [0 1 2])]
            (map + top-left offset))))))

(defn valid-values-for [board coord]
  (if (has-value? board coord) #{}
    (set/difference
      (set (range 1 10))
      (set/union
        (row-values board coord)
        (col-values board coord)
        (block-values board coord)))))

(defn filled? [board]
  (not (contains? (set (flatten board)) 0)))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (and
    (every? true? (map (fn [x] (= x (set (range 1 10)))) (rows board)))
    (filled? board)))

(defn cols [board]
  (map set (for [x (range 0 9)]
    (col-values board [0 x]))))

(defn valid-cols? [board]
  (and
    (every? true? (map (fn [x] (= x (set (range 1 10)))) (cols board)))
    (filled? board)))

(defn blocks [board]
  (map set
    (map
      (fn [x] (block-values board x))
      (for [x [1 4 7] y [1 4 7]] [x y]))))

(defn valid-blocks? [board]
  (and
    (every? true? (map (fn [x] (= x (set (range 1 10)))) (blocks board)))
    (filled? board)))

(defn valid-solution? [board]
  ((every-pred valid-rows? valid-cols? valid-blocks?) board))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (remove #(has-value? board %) (coord-pairs (range 9)))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      '[])
    (let [empty-loc (find-empty-point board)]
      (for [valid-val (valid-values-for board empty-loc)
            solution (solve (set-value-at board empty-loc valid-val))]
        solution))))
