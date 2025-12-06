(ns advent-of-code-2024-clj.day1-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent-of-code-2024-clj.day1 :as day1]))

(deftest part1-test
  (testing "Part 1 solution"
    (is (= 1603498 (day1/solve)))))

(deftest part2-test
  (testing "Part 2 solution"
    (is (= 25574739 (day1/solve2)))))