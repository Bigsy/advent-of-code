(ns advent-of-code-2024-clj.day16
  (:require
   [clojure.java.io :as io]
   [hashp.core]
   [clojure.string :as str]))

(def directions {:north [-1 0]
                :south [1 0]
                :east [0 1]
                :west [0 -1]})

(defn parse-map [input]
  (let [lines (str/split-lines input)
        height (count lines)
        width (count (first lines))
        chars (into {} (for [y (range height)
                            x (range width)
                            :let [c (get-in lines [y x])]
                            :when (not= c \#)]
                        [[y x] c]))]
    {:grid chars
     :height height
     :width width
     :start (first (keep (fn [[pos c]] (when (= c \S) pos)) chars))
     :end (first (keep (fn [[pos c]] (when (= c \E) pos)) chars))}))

(defn get-next-positions [[y x] dir]
  (let [[dy dx] (directions dir)
        new-pos [(+ y dy) (+ x dx)]]
    new-pos))

(defn turn-cost [] 1000)
(defn move-cost [] 1)

(defn get-neighbors [state]
  (let [{:keys [pos dir]} state]
    (concat
     ;; Continue straight
     [{:pos (get-next-positions pos dir)
       :dir dir
       :cost (move-cost)}]
     ;; Turn left or right
     (for [new-dir (case dir
                     :north [:west :east]
                     :south [:west :east]
                     :east [:north :south]
                     :west [:north :south])]
       {:pos pos
        :dir new-dir
        :cost (turn-cost)}))))

(defn dijkstra [maze]
  (let [start-state {:pos (:start maze)
                     :dir :east}]
    (loop [queue (sorted-map 0 #{start-state})
           visited #{}]
      (if (empty? queue)
        nil
        (let [[current-cost states] (first queue)
              current-state (first states)
              remaining-states (disj states current-state)
              next-queue (if (empty? remaining-states)
                          (dissoc queue current-cost)
                          (assoc queue current-cost remaining-states))]
          (if (= (:pos current-state) (:end maze))
            current-cost
            (let [neighbors (for [neighbor (get-neighbors current-state)
                                :let [new-pos (:pos neighbor)]
                                :when (and ((:grid maze) new-pos)
                                         (not (visited {:pos new-pos :dir (:dir neighbor)})))]
                            neighbor)
                  new-queue (reduce (fn [q {:keys [pos dir cost]}]
                                    (let [new-cost (+ current-cost cost)
                                          new-state {:pos pos :dir dir}]
                                      (update q new-cost (fnil conj #{}) new-state)))
                                  next-queue
                                  neighbors)]
              (recur new-queue (conj visited current-state)))))))))

(defn solve-part1 [input]
  (dijkstra (parse-map input)))

(defn solve-part2 [input])

(comment
  (solve-part1 (str/trim (slurp (io/resource "day16.txt")))) ;; 90460
  (solve-part2 (str/trim (slurp (io/resource "day16.txt"))))) ;; 8767 high