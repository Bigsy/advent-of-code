(ns advent-of-code-2024-clj.day10
  (:require
    [hashp.core]
    [clojure.string :as str]))

(defn solve-part1 [input]
  (let [lines (clojure.string/split-lines input)
        height (count lines)
        width (count (first lines))
        grid (vec (for [y (range height)
                       x (range width)]
                   [(Character/digit (get-in lines [y x]) 10) [x y]]))
        trailheads (filter #(zero? (first %)) grid)
        neighbors [[0 1] [0 -1] [1 0] [-1 0]]
        valid-pos? (fn [[x y]] (and (>= x 0) (< x width) (>= y 0) (< y height)))
        get-height (fn [[x y]] (Character/digit (get-in lines [y x]) 10))
        find-reachable-nines (fn [[_ start-pos]]
                              (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [0 start-pos])
                                     seen #{start-pos}
                                     nines #{}]
                                (if (empty? queue)
                                  (count nines)
                                  (let [[curr-height pos] (peek queue)
                                        [x y] pos
                                        next-height (inc curr-height)
                                        next-positions (for [[dx dy] neighbors
                                                           :let [nx (+ x dx)
                                                                 ny (+ y dy)
                                                                 next-pos [nx ny]]
                                                           :when (and (valid-pos? next-pos)
                                                                     (not (seen next-pos))
                                                                     (= next-height (get-height next-pos)))]
                                                       [next-height next-pos])]
                                    (recur (into (pop queue) next-positions)
                                           (into seen (map second next-positions))
                                           (if (= curr-height 9)
                                             (conj nines pos)
                                             nines))))))]
    (->> trailheads
         (map find-reachable-nines)
         (reduce +))))

(defn solve-part2 [input]
  (let [lines (clojure.string/split-lines input)
        height (count lines)
        width (count (first lines))
        grid (vec (for [y (range height)
                       x (range width)]
                   [(Character/digit (get-in lines [y x]) 10) [x y]]))
        trailheads (filter #(zero? (first %)) grid)
        neighbors [[0 1] [0 -1] [1 0] [-1 0]]
        valid-pos? (fn [[x y]] (and (>= x 0) (< x width) (>= y 0) (< y height)))
        get-height (fn [[x y]] (Character/digit (get-in lines [y x]) 10))
        find-distinct-paths (fn [[_ start-pos]]
                            (loop [stack (list [[0 start-pos] #{start-pos}])
                                   paths #{}]
                              (if (empty? stack)
                                (count paths)
                                (let [[[curr-height pos] seen] (first stack)
                                      [x y] pos
                                      next-height (inc curr-height)
                                      next-positions (for [[dx dy] neighbors
                                                         :let [nx (+ x dx)
                                                               ny (+ y dy)
                                                               next-pos [nx ny]]
                                                         :when (and (valid-pos? next-pos)
                                                                   (not (seen next-pos))
                                                                   (= next-height (get-height next-pos)))]
                                                     [[next-height next-pos] (conj seen next-pos)])]
                                  (recur (into (rest stack) next-positions)
                                         (if (= curr-height 9)
                                           (conj paths seen)
                                           paths))))))]
    (->> trailheads
         (map find-distinct-paths)
         (reduce +))))

(comment
  (solve-part1 (str/trim (slurp (clojure.java.io/resource "day10.txt")))) ;; 682
  (solve-part2 (str/trim (slurp (clojure.java.io/resource "day10.txt"))))) ;; 1511
