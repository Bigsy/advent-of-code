(ns advent-of-code-2024-clj.day2
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (->> input
       str/split-lines
       (mapv (fn [line]
               (->> (str/split (str/trim line) #"\s+")
                    (mapv (fn [x] (Integer/parseInt x))))))))

(defn valid-differences? [nums]
  (let [diffs (map - (rest nums) nums)]
    (and (every? #(<= 1 (Math/abs %) 3) diffs)
         (or (every? pos? diffs)
             (every? neg? diffs)))))

(defn solve-part1 [input]
  (->> input parse-input (filter valid-differences?) count))

(defn solve-part2 [input]
  (->> (parse-input input)
       (filter (fn [nums]
                 (or (valid-differences? nums)
                     (some (fn [i]
                             (valid-differences? 
                              (into (subvec nums 0 i) 
                                    (subvec nums (inc i)))))
                           (range (count nums))))))
       count))

(solve-part1 (slurp (clojure.java.io/resource "day2.txt"))) ;; 411
(solve-part2 (slurp (clojure.java.io/resource "day2.txt"))) ;; 465