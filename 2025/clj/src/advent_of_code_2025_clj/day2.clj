(ns advent-of-code-2025-clj.day2
  (:require [clojure.string :as str]))

(def input-file "../inputs/day2.txt")

(defn parse-input [input]
  (->> (str/split (str/trim input) #",")
       (map #(let [[start end] (str/split % #"-")]
               [(parse-long start) (parse-long end)]))))

(defn pattern->invalid-id [pattern]
  ;; pattern 5 -> 55, pattern 64 -> 6464, pattern 123 -> 123123
  (let [s (str pattern)
        k (count s)]
    (+ (* pattern (long (Math/pow 10 k))) pattern)))

(defn generate-all-invalid-ids [max-val]
  ;; Generate all invalid IDs up to max-val
  ;; For each pattern length k, patterns range from 10^(k-1) to 10^k - 1
  ;; (except k=1 which is 1-9)
  (loop [k 1
         result []]
    (let [min-pattern (if (= k 1) 1 (long (Math/pow 10 (dec k))))
          max-pattern (dec (long (Math/pow 10 k)))
          multiplier (inc (long (Math/pow 10 k)))
          ;; max valid pattern for this k that produces ID <= max-val
          max-usable (min max-pattern (quot max-val multiplier))]
      (if (< max-usable min-pattern)
        result
        (recur (inc k)
               (into result (map pattern->invalid-id (range min-pattern (inc max-usable)))))))))

(defn in-range? [[start end] n]
  (and (>= n start) (<= n end)))

(defn solve-part1 [input]
  (let [ranges (parse-input input)
        max-val (apply max (map second ranges))
        all-invalid (generate-all-invalid-ids max-val)]
    (->> all-invalid
         (filter (fn [id] (some #(in-range? % id) ranges)))
         (reduce +))))

(defn repeated-pattern? [s]
  ;; Check if string s is made of some pattern repeated >= 2 times
  (let [len (count s)]
    (some (fn [pattern-len]
            (when (zero? (mod len pattern-len))
              (let [pattern (subs s 0 pattern-len)
                    repetitions (/ len pattern-len)]
                (and (>= repetitions 2)
                     (= s (apply str (repeat repetitions pattern)))))))
          (range 1 (inc (/ len 2))))))

(defn pattern->invalid-id-v2 [pattern reps]
  ;; Create an invalid ID by repeating pattern reps times
  (parse-long (apply str (repeat reps (str pattern)))))

(defn generate-all-invalid-ids-v2 [max-val]
  ;; Generate all invalid IDs up to max-val (patterns repeated >= 2 times)
  (let [max-digits (count (str max-val))]
    (->> (for [pattern-len (range 1 (inc (/ max-digits 2)))
               pattern (range (if (= pattern-len 1) 1 (long (Math/pow 10 (dec pattern-len))))
                              (long (Math/pow 10 pattern-len)))
               reps (range 2 (inc (/ max-digits pattern-len)))
               :let [id (pattern->invalid-id-v2 pattern reps)]
               :when (<= id max-val)]
           id)
         (into #{})))) ;; Use set to avoid duplicates (e.g., 1111 = 11×2 = 1×4)

(defn solve-part2 [input]
  (let [ranges (parse-input input)
        max-val (apply max (map second ranges))
        all-invalid (generate-all-invalid-ids-v2 max-val)]
    (->> all-invalid
         (filter (fn [id] (some #(in-range? % id) ranges)))
         (reduce +))))

(defn solve [] (solve-part1 (slurp input-file))) ;;26255179562
(defn solve2 [] (solve-part2 (slurp input-file))) ;;31680313976