(ns advent-of-code-2025-clj.day4
  (:require [clojure.string :as str]))

(def input-file "../inputs/day4.txt")

(defn parse-input [input]
  (let [lines (str/split-lines input)]
    {:grid (vec (map vec lines))
     :height (count lines)
     :width (count (first lines))}))

(def directions
  [[-1 -1] [-1 0] [-1 1]
   [0 -1]         [0 1]
   [1 -1]  [1 0]  [1 1]])

(defn count-adjacent-rolls [{:keys [grid height width]} [row col]]
  (->> directions
       (map (fn [[dr dc]] [(+ row dr) (+ col dc)]))
       (filter (fn [[r c]] (and (>= r 0) (< r height)
                                (>= c 0) (< c width))))
       (filter (fn [[r c]] (= \@ (get-in grid [r c]))))
       count))

(defn solve-part1 [input]
  (let [{:keys [grid height width] :as parsed} (parse-input input)]
    (->> (for [row (range height)
               col (range width)
               :when (= \@ (get-in grid [row col]))]
           [row col])
         (filter #(< (count-adjacent-rolls parsed %) 4))
         count)))

(defn find-accessible-rolls [{:keys [grid height width] :as parsed}]
  (for [row (range height)
        col (range width)
        :when (= \@ (get-in grid [row col]))
        :when (< (count-adjacent-rolls parsed [row col]) 4)]
    [row col]))

(defn remove-rolls [grid rolls]
  (reduce (fn [g [r c]] (assoc-in g [r c] \.)) grid rolls))

(defn solve-part2 [input]
  (loop [parsed (parse-input input)
         total-removed 0]
    (let [accessible (find-accessible-rolls parsed)]
      (if (empty? accessible)
        total-removed
        (recur (update parsed :grid remove-rolls accessible)
               (+ total-removed (count accessible)))))))

(defn solve [] (solve-part1 (slurp input-file))) ;;1533
(defn solve2 [] (solve-part2 (slurp input-file))) ;;9206