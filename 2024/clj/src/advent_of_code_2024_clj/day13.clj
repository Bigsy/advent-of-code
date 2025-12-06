(ns advent-of-code-2024-clj.day13
  (:require
    [hashp.core]
    [clojure.string :as str]))

(defn parse-line [line]
  (let [[_ ax ay bx by px py] (re-matches #"Button A: X\+(\d+), Y\+(\d+)\nButton B: X\+(\d+), Y\+(\d+)\nPrize: X=(\d+), Y=(\d+)" line)]
    {:a {:x (parse-long ax) :y (parse-long ay)}
     :b {:x (parse-long bx) :y (parse-long by)}
     :prize {:x (parse-long px) :y (parse-long py)}}))

(defn parse-input [input]
  (->> (str/split input #"\n\n")
       (map parse-line)))

(defn can-reach-prize? [{:keys [a b prize]}]
  (for [a-presses (range 101)
        b-presses (range 101)
        :let [x-pos (+ (* a-presses (:x a)) (* b-presses (:x b)))
              y-pos (+ (* a-presses (:y a)) (* b-presses (:y b)))
              tokens (+ (* 3 a-presses) b-presses)]
        :when (and (= x-pos (:x prize))
                  (= y-pos (:y prize)))]
    tokens))

(defn solve-part1 [input]
  (->> (parse-input input)
       (keep #(when-let [solutions (seq (can-reach-prize? %))]
                (apply min solutions)))
       (reduce +)))

(defn solve-part2 [input]
  )

(comment
  (solve-part1 (str/trim (slurp (clojure.java.io/resource "day13.txt")))) ;; 39748 
  (solve-part2 (str/trim (slurp (clojure.java.io/resource "day13.txt"))))) ;; 20244 low - 74868948249476 high
