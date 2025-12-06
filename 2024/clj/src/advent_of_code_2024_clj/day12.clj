(ns advent-of-code-2024-clj.day12
  (:require
    [clojure.java.io :as io]
    [hashp.core]
    [clojure.string :as str]))

(defn get-neighbors [[x y]]
  [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]])

(defn flood-fill [grid start visited]
  (let [rows (count grid)
        cols (count (first grid))
        valid? (fn [[x y]]
                 (and (>= x 0) (< x rows)
                      (>= y 0) (< y cols)
                      (= (get-in grid [x y])
                         (get-in grid start))))]
    (loop [queue [start]
           visited visited
           perimeter 0]
      (if (empty? queue)
        {:area (count visited)
         :perimeter perimeter
         :visited visited}
        (let [pos (first queue)
              neighbors (get-neighbors pos)
              valid-unvisited (filter #(and (valid? %) (not (visited %))) neighbors)
              new-perimeter (+ perimeter
                               (count (remove valid? neighbors)))]
          (recur (into (vec (rest queue)) valid-unvisited)
                 (into visited valid-unvisited)
                 new-perimeter))))))

(defn solve-part1 [input]
  (let [grid (mapv vec (str/split-lines input))
        rows (count grid)
        cols (count (first grid))]
    (loop [x 0
           y 0
           visited #{}
           total 0]
      (cond
        (>= x rows) total
        (>= y cols) (recur (inc x) 0 visited total)
        (visited [x y]) (recur x (inc y) visited total)
        :else
        (let [result (flood-fill grid [x y] #{[x y]})
              price (* (:area result) (:perimeter result))]
          (recur x (inc y)
                 (into visited (:visited result))
                 (+ total price)))))))




(defn solve-part2 [input]
  )

(comment
  (solve-part1 (str/trim (slurp (io/resource "day12.txt")))) ;; 1352976
  (solve-part2 (str/trim (slurp (io/resource "day12.txt"))))) ;; less 1305965 - more 154205 not 676488
