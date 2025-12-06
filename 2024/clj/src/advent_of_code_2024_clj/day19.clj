(ns advent-of-code-2024-clj.day19
  (:require
   [clojure.java.io :as io]
   [hashp.core]
   [clojure.string :as str]))

(defn parse-input [input]
  (let [[patterns designs] (str/split input #"\n\n")]
    {:patterns (set (str/split patterns #", "))
     :designs (str/split-lines designs)}))

(defn can-make-design? [patterns design]
  (let [len (count design)
        dp (boolean-array (inc len) false)]
    (aset dp 0 true)  ; empty string is always possible
    (doseq [i (range len)
            :when (aget dp i)
            pattern patterns
            :let [pattern-len (count pattern)
                  end (+ i pattern-len)]
            :when (<= end len)]
      (when (= (subs design i end) pattern)
        (aset dp end true)))
    (aget dp len)))

(defn count-ways-to-make-design [patterns design]
  (let [len (count design)
        dp (long-array (inc len) 0)]
    (aset dp 0 1)  ; empty string has one way to make it
    (doseq [i (range len)
            :when (> (aget dp i) 0)
            pattern patterns
            :let [pattern-len (count pattern)
                  end (+ i pattern-len)]
            :when (<= end len)]
      (when (= (subs design i end) pattern)
        (aset dp end (+ (aget dp end) (aget dp i)))))
    (aget dp len)))

(defn solve-part1 [input]
  (let [{:keys [patterns designs]} (parse-input input)]
    (->> designs
         (filter #(can-make-design? patterns %))
         count)))

(defn solve-part2 [input]
  (let [{:keys [patterns designs]} (parse-input input)]
    (->> designs
         (map #(count-ways-to-make-design patterns %))
         (reduce +))))

(comment
  (solve-part1 (str/trim (slurp (io/resource "day19.txt")))) ;; 251
  (solve-part2 (str/trim (slurp (io/resource "day19.txt"))))) ;; 616957151871345