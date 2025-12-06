(ns advent-of-code-2024-clj.day14
  (:require
    [clojure.java.io :as io]
    [hashp.core]
    [clojure.string :as str]))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map (fn [line]
              (let [[_ x y dx dy] (re-matches #"p=(\-?\d+),(\-?\d+) v=(\-?\d+),(\-?\d+)" line)]
                {:pos [(parse-long x) (parse-long y)]
                 :vel [(parse-long dx) (parse-long dy)]})))
       vec))

(defn wrap-position [pos size]
  (let [wrapped (mod pos size)]
    (if (neg? wrapped)
      (+ wrapped size)
      wrapped)))

(defn move-robot [{:keys [pos vel]} width height]
  (let [[x y] pos
        [dx dy] vel
        new-x (wrap-position (+ x dx) width)
        new-y (wrap-position (+ y dy) height)]
    {:pos [new-x new-y]
     :vel vel}))

(defn simulate-step [robots width height]
  (mapv #(move-robot % width height) robots))

(defn get-quadrant [[x y] width height]
  (let [mid-x (/ (dec width) 2)  ; Account for 0-based indexing
        mid-y (/ (dec height) 2)]
    (cond
      (= x mid-x) nil
      (= y mid-y) nil
      (and (< x mid-x) (< y mid-y)) :top-left
      (and (> x mid-x) (< y mid-y)) :top-right
      (and (< x mid-x) (> y mid-y)) :bottom-left
      (and (> x mid-x) (> y mid-y)) :bottom-right)))

(defn count-robots-in-quadrants [robots width height]
  (->> robots
       (keep (fn [{:keys [pos]}] (get-quadrant pos width height)))
       (group-by identity)
       (map (fn [[k v]] [k (count v)]))
       (into {})))

(defn solve-part1 [input]
  (let [robots (parse-input input)
        width 101
        height 103
        final-state (nth (iterate #(simulate-step % width height) robots) 100)
        quadrant-counts (count-robots-in-quadrants final-state width height)]
    (* (get quadrant-counts :top-left 0)
       (get quadrant-counts :top-right 0)
       (get quadrant-counts :bottom-left 0)
       (get quadrant-counts :bottom-right 0))))

(defn solve-part2 [input]
  )

(comment
  (solve-part1 (str/trim (slurp (io/resource "day14.txt")))) ;; 215476074
  (solve-part2 (str/trim (slurp (io/resource "day14.txt"))))) ;; 10001 high 1001 low
