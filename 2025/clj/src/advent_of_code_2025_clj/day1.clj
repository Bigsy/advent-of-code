(ns advent-of-code-2025-clj.day1
  (:require [clojure.string :as str]))

(def input-file "../inputs/day1.txt")

(defn parse-input [input]
  (->> (str/split-lines input)
       (map (fn [line]
              (let [dir (first line)
                    dist (parse-long (subs line 1))]
                [dir dist])))))

(defn rotate [pos [dir dist]]
  (mod (if (= dir \L)
         (- pos dist)
         (+ pos dist))
       100))

(defn solve-part1 [input]
  (let [rotations (parse-input input)]
    (->> rotations
         (reductions rotate 50)
         rest
         (filter zero?)
         count)))

(defn count-zeros [pos [dir dist]]
  "Count how many times the dial points at 0 during a rotation"
  (if (= dir \L)
    (if (zero? pos)
      (quot dist 100)
      (if (> pos dist)
        0
        (+ 1 (quot (- dist pos) 100))))
    (if (zero? pos)
      (quot dist 100)
      (let [first-zero (- 100 pos)]
        (if (> first-zero dist)
          0
          (+ 1 (quot (- (+ dist pos) 100) 100)))))))

(defn solve-part2 [input]
  (let [rotations (parse-input input)
        positions (reductions rotate 50 rotations)]
    (->> (map count-zeros positions rotations)
         (reduce +))))

(defn solve [] (solve-part1 (slurp input-file)))  ;;1084
(defn solve2 [] (solve-part2 (slurp input-file)))  ;;6475