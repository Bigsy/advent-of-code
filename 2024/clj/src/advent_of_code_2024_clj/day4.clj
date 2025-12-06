(ns advent-of-code-2024-clj.day4
  (:require
    [hashp.core]
    [clojure.string :as str]))

(defn parse-grid [input]
  (let [lines (str/split-lines input)]
    (mapv vec lines)))

(defn get-char [grid row col]
  (when (and (>= row 0)
             (< row (count grid))
             (>= col 0)
             (< col (count (first grid))))
    (nth (nth grid row) col)))

(defn check-direction [grid row col [dx dy] pattern]
  (let [chars (for [i (range (count pattern))]
                (get-char grid
                          (+ row (* i dy))
                          (+ col (* i dx))))]
    (when (every? some? chars)
      (= pattern (apply str chars)))))

(defn count-pattern-at-position [grid row col pattern]
  (let [directions [[1 0]   ; right
                    [0 1]    ; down
                    [1 1]    ; diagonal down-right
                    [-1 1]   ; diagonal down-left
                    [-1 0]   ; left
                    [0 -1]   ; up
                    [-1 -1]  ; diagonal up-left
                    [1 -1]]] ; diagonal up-right
    (->> directions
         (filter #(check-direction grid row col % pattern))
         count)))

(defn solve-part1 [input]
  (let [grid (parse-grid input)]
    (->> (for [row (range (count grid))
               col (range (count (first grid)))]
           (count-pattern-at-position grid row col "XMAS"))
         (reduce +))))

(defn check-mas-pattern [grid row col [dx dy]]
  (let [patterns #{"MAS" "SAM"}
        chars (for [i (range 3)]
                (get-char grid
                         (+ row (* i dy))
                         (+ col (* i dx))))]
    (when (every? some? chars)
      (let [pattern (apply str chars)]
        (or (patterns pattern)
            (patterns (apply str (reverse chars))))))))

(defn check-x-mas [grid row col]
  (when (= \A (get-char grid row col))
    (let [diagonal1 (check-mas-pattern grid (dec row) (dec col) [1 1])
          diagonal2 (check-mas-pattern grid (dec row) (inc col) [-1 1])]
      (and diagonal1 diagonal2))))

(defn solve-part2 [input]
  (let [grid (parse-grid input)]
    (->> (for [row (range 1 (dec (count grid)))
               col (range 1 (dec (count (first grid))))]
           (if (check-x-mas grid row col) 1 0))
         (reduce +))))

(comment
  (solve-part1 (slurp (clojure.java.io/resource "day4.txt"))) ;; 2493
  (solve-part2 (slurp (clojure.java.io/resource "day4.txt")))) ;; 1890
