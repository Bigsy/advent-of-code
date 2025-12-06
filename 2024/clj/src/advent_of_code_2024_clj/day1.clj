(ns advent-of-code-2024-clj.day1
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map #(mapv parse-long (str/split % #"\s+")))))

(defn solve-part1 [input]
  (let [data (parse-input input)
        pairs (map vector 
                  (sort (map first data))
                  (sort (map second data)))]
    (reduce + (map #(Math/abs (apply - %)) pairs))))

(defn solve-part2 [input]
  (let [data (parse-input input)]
    (->> (map first data)
         (map #(* % (count (filter #{%} (map second data)))))
         (reduce +))))

(defn solve [] (solve-part1 (slurp (io/resource "day1.txt"))))
(defn solve2 [] (solve-part2 (slurp (io/resource "day1.txt"))))
