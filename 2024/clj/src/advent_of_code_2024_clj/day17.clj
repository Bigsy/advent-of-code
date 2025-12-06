(ns advent-of-code-2024-clj.day17
  (:require
   [clojure.java.io :as io]
   [hashp.core]
   [clojure.string :as str]))

(defn parse-input [input]
  (let [[registers program] (str/split input #"\n\n")
        [a b c] (map #(Integer/parseInt (second (str/split % #": "))) (str/split-lines registers))
        program (map #(Integer/parseInt %) (str/split (second (str/split program #": ")) #","))]
    {:registers {:A a :B b :C c}
     :program program}))

(defn get-combo-operand [operand registers]
  (case operand
    0 0
    1 1
    2 2
    3 3
    4 (:A registers)
    5 (:B registers)
    6 (:C registers)))

(defn execute-instruction [state]
  (let [{:keys [registers program ip output]} state
        opcode (nth program ip)
        operand (nth program (inc ip))]
    (case opcode
      0 (let [denominator (Math/pow 2 (get-combo-operand operand registers))]
          (assoc state :registers (assoc registers :A (int (/ (:A registers) denominator))) :ip (+ ip 2)))
      1 (assoc state :registers (assoc registers :B (bit-xor (:B registers) operand)) :ip (+ ip 2))
      2 (assoc state :registers (assoc registers :B (mod (get-combo-operand operand registers) 8)) :ip (+ ip 2))
      3 (if (zero? (:A registers))
          (assoc state :ip (+ ip 2))
          (assoc state :ip operand))
      4 (assoc state :registers (assoc registers :B (bit-xor (:B registers) (:C registers))) :ip (+ ip 2))
      5 (assoc state :output (conj output (mod (get-combo-operand operand registers) 8)) :ip (+ ip 2))
      6 (let [denominator (Math/pow 2 (get-combo-operand operand registers))]
          (assoc state :registers (assoc registers :B (int (/ (:A registers) denominator))) :ip (+ ip 2)))
      7 (let [denominator (Math/pow 2 (get-combo-operand operand registers))]
          (assoc state :registers (assoc registers :C (int (/ (:A registers) denominator))) :ip (+ ip 2))))))

(defn run-program [state]
  (loop [state state]
    (if (>= (:ip state) (count (:program state)))
      state
      (recur (execute-instruction state)))))

(defn solve-part1 [input]
  (let [{:keys [registers program]} (parse-input input)
        initial-state {:registers registers :program program :ip 0 :output []}
        final-state (run-program initial-state)]
    (str/join "," (:output final-state))))

(defn solve-part2 [input])

(comment
  (solve-part1 (str/trim (slurp (io/resource "day17.txt")))) ;; 1,5,7,4,1,6,0,3,0
  (solve-part2 (str/trim (slurp (io/resource "day17.txt