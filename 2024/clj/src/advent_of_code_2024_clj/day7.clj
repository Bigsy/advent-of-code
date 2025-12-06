(ns advent-of-code-2024-clj.day7
  (:require
    [hashp.core]
    [clojure.string :as str]))

(defn parse-line [line]
  (let [[target nums] (str/split line #":")
        target (parse-long target)
        nums (map parse-long (str/split (str/trim nums) #"\s+"))]
    [target nums]))

(defn evaluate [nums operators]
  (reduce
    (fn [result [n op]]
      ((case op \+ + \* *) result n))
    (first nums)
    (map vector (rest nums) operators)))

(defn evaluate-with-concat [nums operators]
  (loop [result (first nums)
         remaining-nums (rest nums)
         remaining-ops operators]
    (if (empty? remaining-nums)
      result
      (let [next-num (first remaining-nums)
            op (first remaining-ops)]
        (recur
          (case op
            \+ (+ result next-num)
            \* (* result next-num)
            \| (parse-long (str result next-num)))  ; Concatenation operator
          (rest remaining-nums)
          (rest remaining-ops))))))

(defn generate-operator-combinations [n]
  (if (zero? n)
    [[]]
    (for [combo (generate-operator-combinations (dec n))
          op [\+ \*]]
      (conj combo op))))

(defn generate-operator-combinations-with-concat [n]
  (if (zero? n)
    [[]]
    (for [combo (generate-operator-combinations-with-concat (dec n))
          op [\+ \* \|]]  ; Add concatenation operator
      (conj combo op))))

(defn can-equation-be-solved? [[target nums]]
  (let [operator-count (dec (count nums))
        operator-combinations (generate-operator-combinations operator-count)]
    (some #(= target (evaluate nums %)) operator-combinations)))

(defn can-equation-be-solved-part2? [[target nums]]
  (let [operator-count (dec (count nums))
        operator-combinations (generate-operator-combinations-with-concat operator-count)]
    (some #(= target (evaluate-with-concat nums %)) operator-combinations)))

(defn solve-part1 [input]
  (->> input
       str/split-lines
       (map parse-line)
       (filter can-equation-be-solved?)
       (map first)
       (reduce +)))

(defn solve-part2 [input]
  (->> input
       str/split-lines
       (map parse-line)
       (filter can-equation-be-solved-part2?)
       (map first)
       (reduce +)))

(comment
  (solve-part1 (slurp (clojure.java.io/resource "day7.txt"))) ;;14711933466277
  (solve-part2 (slurp (clojure.java.io/resource "day7.txt")))) ;;286580387663654
