(ns advent-of-code-2024-clj.day18
  (:require
   [clojure.java.io :as io]
   [hashp.core]
   [clojure.string :as str]))

(defn parse-coords [line]
  (let [[x y] (str/split line #",")]
    [(Integer/parseInt x) (Integer/parseInt y)]))

(defn parse-input [input]
  (->> (str/split-lines input)
       (take 1024)  ; Only take first kilobyte
       (map parse-coords)
       (into #{})))

(defn valid-pos? [pos max-size corrupted]
  (let [[x y] pos]
    (and (>= x 0) (>= y 0)
         (< x max-size) (< y max-size)
         (not (corrupted pos)))))

(defn neighbors [[x y] max-size corrupted]
  (for [[dx dy] [[0 1] [1 0] [0 -1] [-1 0]]
        :let [new-pos [(+ x dx) (+ y dy)]]
        :when (valid-pos? new-pos max-size corrupted)]
    new-pos))

(defn shortest-path [start end corrupted max-size]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [start 0])
         visited #{start}]
    (when-let [[pos steps] (peek queue)]
      (if (= pos end)
        steps
        (let [next-positions (remove visited (neighbors pos max-size corrupted))]
          (recur
           (into (pop queue) (map #(vector % (inc steps)) next-positions))
           (into visited next-positions)))))))

(defn solve-part1 [input]
  (let [corrupted (parse-input input)
        max-size 71  ; Grid is 71x71 (0 to 70 inclusive)
        start [0 0]
        end [70 70]]
    (shortest-path start end corrupted max-size)))

(defn first-blocking-byte [input]
  (let [coords (map parse-coords (str/split-lines input))
        max-size 71
        start [0 0]
        end [70 70]]
    (loop [i 0
           corrupted #{}]
      (let [new-corrupted (conj corrupted (nth coords i))
            path-exists? (shortest-path start end new-corrupted max-size)]
        (if (nil? path-exists?)
          (nth coords i)  ; Return the coordinates that blocked the path
          (recur (inc i) new-corrupted))))))

(defn solve-part2 [input]
  (let [[x y] (first-blocking-byte input)]
    (str x "," y)))

(comment
  (solve-part1 (str/trim (slurp (io/resource "day18.txt")))) ;; 334
  (solve-part2 (str/trim (slurp (io/resource "day18.txt"))))) ;; 20,12