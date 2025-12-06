(ns advent-of-code-2024-clj.day24
  (:require
   [clojure.java.io :as io]
   [hashp.core]
   [clojure.string :as str]))

(defn parse-initial-values [lines]
  (->> lines
       (take-while not-empty)
       (map #(str/split % #":\s+"))
       (map (fn [[wire val]] [wire (Integer/parseInt val)]))
       (into {})))

(defn parse-gate [line]
  (let [[_ in1 op in2 out] (re-matches #"(\w+)\s+(\w+)\s+(\w+)\s+->\s+(\w+)" line)]
    {:op op :in1 in1 :in2 in2 :out out}))

(defn parse-gates [lines]
  (->> lines
       (drop-while not-empty)
       (drop 1)
       (map parse-gate)))

(defn gate-fn [op]
  (case op
    "AND" #(bit-and %1 %2)
    "OR" #(bit-or %1 %2)
    "XOR" #(bit-xor %1 %2)))

(defn evaluate-gate [{:keys [op in1 in2]} wire-values]
  (when (and (get wire-values in1) (get wire-values in2))
    ((gate-fn op) (get wire-values in1) (get wire-values in2))))

(defn evaluate-circuit [initial-values gates]
  (loop [wire-values initial-values
         prev-size 0]
    (let [new-values (reduce
                      (fn [values gate]
                        (if-let [result (evaluate-gate gate values)]
                          (assoc values (:out gate) result)
                          values))
                      wire-values
                      gates)]
      (if (= (count new-values) prev-size)
        new-values
        (recur new-values (count new-values))))))

(defn z-wires-to-decimal [wire-values]
  (let [z-wires (->> wire-values
                     keys
                     (filter #(.startsWith % "z"))
                     sort
                     reverse)]
    (->> z-wires
         (map wire-values)
         (reduce (fn [acc bit] (+ (* 2 acc) bit)) 0))))

(defn solve-part1 [input]
  (let [lines (str/split-lines input)
        initial-values (parse-initial-values lines)
        gates (parse-gates lines)
        final-values (evaluate-circuit initial-values gates)]
    (z-wires-to-decimal final-values)))

(defn solve-part2 [input]
  )

(comment
  (solve-part1 (str/trim (slurp (io/resource "day24.txt")))) ;; 48508229772400
  (solve-part2 (str/trim (slurp (io/resource "day24.txt"))))) ;;