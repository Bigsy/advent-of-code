(ns advent-of-code-2024-clj.day20
  (:require
   [clojure.java.io :as io]
   [hashp.core]
   [clojure.string :as str]))

(defn parse-maze [input]
  (let [lines (str/split-lines input)
        height (count lines)
        width (count (first lines))
        maze (into {} (for [y (range height)
                           x (range width)
                           :let [ch (get-in lines [y x])]]
                       [[x y] ch]))
        start-pos (first (keep (fn [[pos ch]] (when (= ch \S) pos)) maze))
        end-pos (first (keep (fn [[pos ch]] (when (= ch \E) pos)) maze))]
    {:maze maze :start start-pos :end end-pos}))

(defn neighbors [[x y]]
  [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]])

(defn bfs-distances [maze start]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [start 0])
         distances {}]
    (if (empty? queue)
      distances
      (let [[pos dist] (peek queue)
            queue (pop queue)]
        (if (contains? distances pos)
          (recur queue distances)
          (let [next-moves (for [next-pos (neighbors pos)
                                :when (and (not= \# (get maze next-pos \#))
                                         (not (contains? distances next-pos)))]
                            [next-pos (inc dist)])]
            (recur (into queue next-moves)
                  (assoc distances pos dist))))))))

(defn find-cheat-possibilities [{:keys [maze start end]}]
  (let [from-start (bfs-distances maze start)
        from-end (bfs-distances maze end)
        normal-distance (get from-start end)
        wall-positions (for [[pos ch] maze
                           :when (= ch \#)
                           [nx ny] (neighbors pos)
                           [nx2 ny2] (neighbors pos)
                           :when (and (contains? from-start [nx ny])
                                    (contains? from-end [nx2 ny2])
                                    (not= [nx ny] [nx2 ny2]))]
                       [[nx ny] [nx2 ny2]])]
    (for [[p1 p2] wall-positions
          :let [total-dist (+ (get from-start p1)
                             2
                             (get from-end p2))
                saved (- normal-distance total-dist)]
          :when (pos? saved)]
      saved)))

(defn solve-part1 [input]
  (let [maze-data (parse-maze input)
        cheats (find-cheat-possibilities maze-data)]
    (count (filter #(>= % 100) cheats))))

(defn solve-part2 [input]
  )

(comment
  (solve-part1 (str/trim (slurp (io/resource "day20.txt")))) ;; 1343
  (solve-part2 (str/trim (slurp (io/resource "day20.txt"))))) ;; 2070 low