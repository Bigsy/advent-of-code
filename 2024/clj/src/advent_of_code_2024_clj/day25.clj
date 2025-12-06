(ns advent-of-code-2024-clj.day25
  (:require
   [clojure.java.io :as io]
   [hashp.core]
   [clojure.string :as str]))

(defn parse-schematic [lines]
  (let [height (count lines)
        width (count (first lines))]
    (vec (for [x (range width)]
          (let [col (map #(nth % x) lines)]
            (if (= \# (first col))  ; if top is #, it's a lock
              (count (take-while #(= \# %) col))  ; count from top
              (count (take-while #(= \# %) (reverse col))))))))) ; count from bottom

(defn can-fit? [lock key]
  (every? #(<= % 7) (map + lock key)))

(defn parse-input [input]
  (let [schematics (str/split input #"\n\n")
        parse-group #(parse-schematic (str/split-lines %))]
    [(filter #(= \# (first (first (str/split-lines %)))) schematics)  ; locks
     (filter #(= \. (first (first (str/split-lines %)))) schematics)])) ; keys

(defn solve-part1 [input]
  (let [[locks keys] (parse-input input)
        locks (map parse-schematic (map str/split-lines locks))
        keys (map parse-schematic (map str/split-lines keys))]
    (->> (for [l locks k keys
               :when (can-fit? l k)]
           [l k])
         count)))

(defn solve-part2 [input]
  )

(comment
  (solve-part1 (str/trim (slurp (io/resource "day25.txt")))) ;; 3365
  (solve-part2 (str/trim (slurp (io/resource "day25.txt"))))) ;;