(ns advent-of-code-2025-clj.day3
  (:require [clojure.string :as str]))

(def input-file "../inputs/day3.txt")

(defn parse-input [input]
  (->> (str/split-lines input)
       (map #(mapv (fn [c] (- (int c) (int \0))) %))))

(defn max-joltage [digits]
  (apply max
         (for [i (range (dec (count digits)))
               :let [first-digit (nth digits i)
                     max-after (apply max (subvec digits (inc i)))]]
           (+ (* 10 first-digit) max-after))))

(defn solve-part1 [input]
  (->> (parse-input input)
       (map max-joltage)
       (reduce +)))

(defn max-joltage-n [digits n]
  (loop [remaining n
         start 0
         result 0]
    (if (zero? remaining)
      result
      (let [end (- (count digits) remaining)
            candidates (range start (inc end))
            max-val (apply max (map #(nth digits %) candidates))
            best-idx (first (filter #(= max-val (nth digits %)) candidates))]
        (recur (dec remaining)
               (inc best-idx)
               (+ (* result 10) max-val))))))

(defn solve-part2 [input]
  (->> (parse-input input)
       (map #(max-joltage-n % 12))
       (reduce +)))

(defn solve [] (solve-part1 (slurp input-file))) ;;17766
(defn solve2 [] (solve-part2 (slurp input-file))) ;;176582889354075