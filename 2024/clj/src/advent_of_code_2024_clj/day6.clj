(ns advent-of-code-2024-clj.day6
  (:require
    [hashp.core]
    [clojure.string :as str]))

(def directions {:up [-1 0]
                :right [0 1]
                :down [1 0]
                :left [0 -1]})

(def turn-right {:up :right
                 :right :down
                 :down :left
                 :left :up})

(defn parse-grid [input]
  (let [lines (str/split-lines input)
        height (count lines)
        width (count (first lines))
        grid (into {} (for [y (range height)
                           x (range width)
                           :let [char (get-in lines [y x])]
                           :when (#{\# \^} char)]
                       [[y x] char]))
        start-pos (first (keep (fn [[pos char]] (when (= char \^) pos)) grid))]
    {:grid (into {} (for [[pos char] grid :when (= char \#)] [pos char]))
     :width width
     :height height
     :start-pos start-pos}))

(defn in-bounds? [{:keys [width height]} [y x]]
  (and (>= y 0) (< y height)
       (>= x 0) (< x width)))

(defn next-pos [[y x] [dy dx]]
  [(+ y dy) (+ x dx)])

(defn solve-part1 [input]
  (let [{:keys [grid width height start-pos] :as state} (parse-grid input)
        initial-dir :up]
    (loop [pos start-pos
           dir initial-dir
           visited #{start-pos}]
      (let [dir-vec (directions dir)
            next-p (next-pos pos dir-vec)]
        (if-not (in-bounds? state next-p)
          (count visited)
          (if (grid next-p)
            (recur pos (turn-right dir) visited)
            (recur next-p dir (conj visited next-p))))))))

(defn simulate-path [{:keys [grid width height] :as state} start-pos extra-obstacle]
  (loop [pos start-pos
         dir :up
         path []
         visited #{}]
    (let [dir-vec (directions dir)
          next-p (next-pos pos dir-vec)]
      (cond
        (not (in-bounds? state next-p)) nil
        (or (grid next-p) (= next-p extra-obstacle))
        (let [new-pos pos
              new-dir (turn-right dir)
              new-path (conj path [new-pos dir])]
          (if (visited [new-pos new-dir])
            (drop-while #(not= % [new-pos new-dir]) new-path)
            (recur new-pos new-dir new-path (conj visited [new-pos new-dir]))))
        :else
        (recur next-p dir (conj path [pos dir]) (conj visited [pos dir]))))))

(defn solve-part2 [input]
  (let [{:keys [grid width height start-pos] :as state} (parse-grid input)]
    (->> (for [y (range height)
               x (range width)
               :let [pos [y x]]
               :when (and (not= pos start-pos)
                         (not (grid pos)))]
           pos)
         (filter #(simulate-path state start-pos %))
         count)))

(comment
  (solve-part1 (slurp (clojure.java.io/resource "day6.txt"))) ;;4515
  (solve-part2 (slurp (clojure.java.io/resource "day6.txt")))) ;;1309
