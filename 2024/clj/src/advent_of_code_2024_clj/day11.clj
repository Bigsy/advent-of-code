(ns advent-of-code-2024-clj.day11
  (:require
    [clojure.string :as str]))

(defn transform-stone [stone]
  (cond
    (zero? stone) [1]
    (even? (count (str stone))) [(Long/parseLong (subs (str stone) 0 (/ (count (str stone)) 2)))
                                (Long/parseLong (subs (str stone) (/ (count (str stone)) 2)))]
    :else [(* stone 2024)]))

(defn blink [stones]
  (->> stones
       (mapcat transform-stone)))

(defn solve-part1 [input]
  (let [initial-stones (->> (str/split input #" ")
                           (map #(Integer/parseInt %)))]
    (->> (iterate blink initial-stones)
         (drop 25)
         first
         count)))

(def count-stones
  (memoize
    (fn [stone blinks]
      (cond
        (zero? blinks) 1
        (zero? stone)
        (count-stones 1 (dec blinks))
        (even? (count (str stone)))
        (+ (count-stones (Long/parseLong (subs (str stone) 0 (/ (count (str stone)) 2))) (dec blinks))
           (count-stones (Long/parseLong (subs (str stone) (/ (count (str stone)) 2))) (dec blinks)))
        :else
        (count-stones (* stone 2024) (dec blinks))))))

(defn solve-part2 [input]
  (let [initial-stones (->> (str/split input #" ")
                            (map #(Long/parseLong %)))]
    (->> initial-stones
         (map #(count-stones % 75))
         (reduce +))))

(defn -main []
  (let [input (str/trim (slurp "resources/day11.txt"))]
    (println "Part 2:" (solve-part2 input))))

(comment
  (solve-part1 (str/trim (slurp (clojure.java.io/resource "day11.txt")))) ;; 217443
  (solve-part2 (str/trim (slurp (clojure.java.io/resource "day11.txt"))))) ;; 257246536026785
