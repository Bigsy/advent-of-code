(ns advent-of-code-2024-clj.day21-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2024-clj.day21 :refer :all]))

(deftest example-test
  (testing "Example from puzzle description"
    (let [input "029A\n980A\n179A\n456A\n379A"] 
      (is (= 126384 (solve-part1 input))))))
