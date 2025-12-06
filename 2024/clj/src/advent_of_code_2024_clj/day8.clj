(ns advent-of-code-2024-clj.day8
  (:require
    [hashp.core]
    [clojure.string :as str]))

(defn parse-grid [input]
  (let [lines (clojure.string/split-lines input)
        height (count lines)
        width (count (first lines))]
    {:grid (into {} (for [y (range height)
                         x (range width)
                         :let [char (get-in lines [y x])]
                         :when (not= char \.)]
                     [[x y] char]))
     :width width
     :height height}))

(defn get-antennas-by-freq [grid]
  (reduce-kv (fn [acc pos freq]
               (update acc freq (fnil conj []) pos))
             {}
             (:grid grid)))

(defn in-bounds? [{:keys [width height]} [x y]]
  (and (>= x 0) (< x width)
       (>= y 0) (< y height)))

(defn distance-squared [[x1 y1] [x2 y2]]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    (+ (* dx dx) (* dy dy))))

(defn aligned? [[x1 y1] [x2 y2] [x3 y3]]
  (let [dx1 (- x2 x1)
        dy1 (- y2 y1)
        dx2 (- x3 x2)
        dy2 (- y3 y2)]
    (= (* dx1 dy2) (* dy1 dx2))))

(defn find-antinodes [grid p1 p2]
  (let [d12 (distance-squared p1 p2)]
    (for [x (range (:width grid))
          y (range (:height grid))
          :let [p3 [x y]]
          :when (and (aligned? p1 p2 p3)
                    (not= p3 p1)
                    (not= p3 p2)
                    (let [d23 (distance-squared p2 p3)
                          d13 (distance-squared p1 p3)]
                      (or (and (= d23 (* 4 d12)) (= d13 d12))  
                          (and (= d13 (* 4 d12)) (= d23 d12)))))] 
      p3)))

(defn solve-part1 [input]
  (let [grid (parse-grid input)
        antennas-by-freq (get-antennas-by-freq grid)
        _ (println "Found frequencies:" (keys antennas-by-freq))
        antinodes (reduce (fn [acc [freq positions]]
                           (if (< (count positions) 2)
                             acc
                             (into acc
                                   (for [p1 positions
                                         p2 positions
                                         :when (not= p1 p2)
                                         antinode (find-antinodes grid p1 p2)]
                                     antinode))))
                         #{}
                         antennas-by-freq)]
    (count antinodes)))

(defn solve-part2 [input]
  (let [grid (parse-grid input)
        antennas-by-freq (get-antennas-by-freq grid)
        antinodes (reduce (fn [acc [freq positions]]
                           (if (< (count positions) 2)
                             acc
                             (into acc
                                   (for [p1 positions
                                         p2 positions
                                         :when (not= p1 p2)
                                         x (range (:width grid))
                                         y (range (:height grid))
                                         :let [p3 [x y]]
                                         :when (aligned? p1 p2 p3)]
                                     p3))))
                         #{}
                         antennas-by-freq)]
    (count antinodes)))

(comment
  (solve-part1 (slurp (clojure.java.io/resource "day8.txt"))) ;;308
  (solve-part2 (slurp (clojure.java.io/resource "day8.txt")))) ;; 1147
