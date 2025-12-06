(ns advent-of-code-2024-clj.day15
  (:require
   [clojure.java.io :as io]
   [hashp.core]
   [clojure.string :as str]))

(defn parse-input [input]
  (let [[map-str moves-str] (str/split input #"\n\n")
        grid (vec (map vec (str/split-lines map-str)))
        moves (str/replace moves-str #"\n" "")]
    [grid moves]))

(defn find-objects [grid]
  (let [objects (atom {})]
    (doseq [y (range (count grid))
            x (range (count (first grid)))]
      (let [c (get-in grid [y x])]
        (when (or (= c \@) (= c \O))
          (swap! objects assoc [y x] c))))
    @objects))

(defn move-object [grid objects pos dir]
  (let [[y x] pos
        [dy dx] (case dir
                  \^ [-1 0]
                  \v [1 0]
                  \< [0 -1]
                  \> [0 1])
        ny (+ y dy)
        nx (+ x dx)]
    (if (or (>= ny 0) (< ny (count grid)) (>= nx 0) (< nx (count (first grid))))
      (let [next-c (get-in grid [ny nx])]
        (if (= next-c \#)
          [objects false]
          (if (contains? objects [ny nx])
            (if (= (objects [ny nx]) \O)
              (let [[new-objects pushed] (move-object grid objects [ny nx] dir)]
                (if pushed
                  [(-> new-objects
                       (dissoc pos)
                       (assoc [ny nx] (objects pos))) true]
                  [objects false]))
              [objects false])
            [(-> objects
                 (dissoc pos)
                 (assoc [ny nx] (objects pos))) true])))
      [objects false])))

(defn simulate-moves [grid moves]
  (let [objects (atom (find-objects grid))]
    (doseq [move moves]
      (let [robot-pos (first (filter #(= (val %) \@) @objects))]
        (when robot-pos
          (let [[new-objects _] (move-object grid @objects (key robot-pos) move)]
            (reset! objects new-objects)))))
    @objects))


(defn calculate-gps-sum [objects]
  (reduce + (map (fn [[pos _]]
                   (let [[y x] pos]
                     (+ (* y 100) x)))
                 (filter #(= (val %) \O) objects))))

(defn solve-part1 [input]
  (let [[grid moves] (parse-input input)
        final-objects (simulate-moves grid moves)]
    (calculate-gps-sum final-objects)))

(defn solve-part2 [input])

(comment
  (solve-part1 (str/trim (slurp (io/resource "day15.txt")))) ;; 1465152
  (solve-part2 (str/trim (slurp (io/resource "day15.txt"))))) ;; 1506113 low