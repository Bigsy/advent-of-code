(ns advent-of-code-2024-clj.day5-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2024-clj.day5 :refer :all]))

(deftest verify-outputs
  (let [input (slurp (clojure.java.io/resource "day5.txt"))]
    (testing "Part 1 output"
      (is (= 5639 (solve-part1 input))))
    (testing "Part 2 output"
      (is (= 5273 (solve-part2 input))))))
