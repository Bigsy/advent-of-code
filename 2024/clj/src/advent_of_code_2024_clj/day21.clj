(ns advent-of-code-2024-clj.day21
  (:require
   [clojure.java.io :as io]
   [hashp.core]
   [clojure.string :as str]))

(def numeric-keypad
  {[0 0] \7, [1 0] \8, [2 0] \9,
   [0 1] \4, [1 1] \5, [2 1] \6,
   [0 2] \1, [1 2] \2, [2 2] \3,
             [1 3] \0, [2 3] \A})

(def directional-keypad
  {          [1 0] \^, [2 0] \A,
   [0 1] \<, [1 1] \v, [2 1] \>})

(def numeric-positions (into {} (map (fn [[k v]] [v k]) numeric-keypad)))
(def directional-positions (into {} (map (fn [[k v]] [v k]) directional-keypad)))

(defn valid-pos? [keypad [x y]]
  (contains? keypad [x y]))

(defn get-neighbors [[x y]]
  [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]])

(defn find-all-shortest-paths [keypad start-char end-char]
  (let [start (get (if (= keypad numeric-keypad) numeric-positions directional-positions) start-char)
        end (get (if (= keypad numeric-keypad) numeric-positions directional-positions) end-char)]
    (if (= start end)
      [[]]
      (let [queue (atom [[start []]])
            visited (atom {})
            all-paths (atom [])
            min-length (atom nil)]
        (while (seq @queue)
          (let [[pos path] (first @queue)]
            (swap! queue rest)
            (when (or (nil? @min-length) (<= (count path) @min-length))
              (when (or (not (contains? @visited pos))
                        (<= (count path) (get @visited pos Long/MAX_VALUE)))
                (swap! visited assoc pos (count path))
                (if (= pos end)
                  (do
                    (when (nil? @min-length)
                      (reset! min-length (count path)))
                    (when (= (count path) @min-length)
                      (swap! all-paths conj path)))
                  (doseq [neighbor (get-neighbors pos)]
                    (when (valid-pos? keypad neighbor)
                      (let [move (cond
                                 (< (first neighbor) (first pos)) \<
                                 (> (first neighbor) (first pos)) \>
                                 (< (second neighbor) (second pos)) \^
                                 (> (second neighbor) (second pos)) \v)]
                        (swap! queue conj [neighbor (conj path move)])))))))))
        @all-paths))))

(defn get-sequence-for-code [keypad code]
  (let [chars (str \A code)]
    (loop [i 0
           all-sequences [[]]]
      (if (>= i (dec (count chars)))
        all-sequences
        (let [from-char (nth chars i)
              to-char (nth chars (inc i))
              paths (find-all-shortest-paths keypad from-char to-char)]
          (recur (inc i)
                 (for [seq all-sequences
                       path paths]
                   (concat seq path [\A]))))))))

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x2 x1)) (Math/abs (- y2 y1))))

(defn get-move-sequence [keypad from-char to-char]
  (let [positions (if (= keypad numeric-keypad) numeric-positions directional-positions)
        [x1 y1] (get positions from-char)
        [x2 y2] (get positions to-char)
        dx (- x2 x1)
        dy (- y2 y1)
        h-moves (vec (repeat (Math/abs dx) (if (pos? dx) \> \<)))
        v-moves (vec (repeat (Math/abs dy) (if (pos? dy) \v \^)))]
    (if (= from-char to-char)
      [[]]
      ;; Generate all possible paths and filter out invalid ones
      (let [invalid-pos (if (= keypad numeric-keypad) [0 3] [0 0])
            all-moves (concat h-moves v-moves)
            ;; Generate all permutations that preserve relative order of same moves
            paths (if (or (empty? h-moves) (empty? v-moves))
                    [(vec all-moves)]
                    ;; For mixed moves, try both orderings
                    [(vec (concat h-moves v-moves))
                     (vec (concat v-moves h-moves))])]
        ;; Filter out paths that would go through invalid position
        (filter (fn [path]
                  (let [positions-in-path
                        (reductions (fn [[x y] move]
                                      (case move
                                        \< [(dec x) y]
                                        \> [(inc x) y]
                                        \^ [x (dec y)]
                                        \v [x (inc y)]))
                                    [x1 y1]
                                    path)]
                    (not-any? #(= % invalid-pos) positions-in-path)))
                paths)))))

(def memo-expand (atom {}))

(defn expand-length [sequence level]
  (if (= level 0)
    (count sequence)
    (let [cache-key [(vec sequence) level]]
      (if-let [cached (get @memo-expand cache-key)]
        cached
        (let [result
              (if (empty? sequence)
                0
                (let [str-seq (apply str sequence)
                      chars (str \A str-seq)]
                  (reduce +
                          (for [i (range (dec (count chars)))]
                            (let [from-char (nth chars i)
                                  to-char (nth chars (inc i))
                                  move-sequences (get-move-sequence directional-keypad from-char to-char)]
                              ;; Try all valid move sequences and pick the shortest result
                              (if (empty? move-sequences)
                                Long/MAX_VALUE
                                (apply min
                                       (map (fn [moves]
                                              (expand-length (conj moves \A) (dec level)))
                                            move-sequences))))))))]
          (swap! memo-expand assoc cache-key result)
          result)))))

(defn solve-code [code]
  (reset! memo-expand {})
  (let [chars (str \A code)
        ;; Find all possible sequences for the numeric keypad
        all-numeric-sequences
        (reduce (fn [sequences i]
                  (let [from-char (nth chars i)
                        to-char (nth chars (inc i))
                        move-seqs (get-move-sequence numeric-keypad from-char to-char)]
                    (for [seq sequences
                          moves move-seqs]
                      (vec (concat seq moves [\A])))))
                [[]]
                (range (dec (count chars))))
        ;; Find the minimum length after full expansion
        min-length (apply min
                          (map #(expand-length % 2) all-numeric-sequences))
        numeric-part (Long/parseLong (re-find #"\d+" code))]
    (* min-length numeric-part)))

(defn solve-part1 [input]
  (let [codes (str/split-lines input)]
    (reduce + (map solve-code codes))))

(defn solve-part2 [input]
  (let [codes (str/split-lines input)]
    (reduce + (map (fn [code]
                     (reset! memo-expand {})
                     (let [chars (str \A code)
                           ;; Find all possible sequences for the numeric keypad
                           all-numeric-sequences
                           (reduce (fn [sequences i]
                                     (let [from-char (nth chars i)
                                           to-char (nth chars (inc i))
                                           move-seqs (get-move-sequence numeric-keypad from-char to-char)]
                                       (for [seq sequences
                                             moves move-seqs]
                                         (vec (concat seq moves [\A])))))
                                   [[]]
                                   (range (dec (count chars))))
                           ;; Find the minimum length after full expansion with 25 levels
                           min-length (apply min
                                             (map #(expand-length % 25) all-numeric-sequences))
                           numeric-part (Long/parseLong (re-find #"\d+" code))]
                       (* min-length numeric-part)))
                   codes))))

(comment
  (solve-part1 (str/trim (slurp (io/resource "day21.txt")))) ;; 237342
  (solve-part2 (str/trim (slurp (io/resource "day21.txt"))))) ;; 294585598101704