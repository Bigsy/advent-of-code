(ns advent-of-code-2024-clj.day9
  (:require
    [hashp.core]
    [clojure.string :as str]))

(defn parse-input [input]
  (mapv #(Character/digit % 10) input))

(defn create-disk-map [lengths]
  (let [total-size (reduce + lengths)
        result (transient (vec (repeat total-size ".")))]
    (loop [pos 0
           file-id 0
           [len & rest-lens] lengths]
      (if (or (nil? len) (neg? len))
        (persistent! result)
        (do
          (dotimes [i len]
            (assoc! result (+ pos i) file-id))
          (recur (+ pos len (if (seq rest-lens) (first rest-lens) 0))
                 (inc file-id)
                 (rest rest-lens)))))))

(defn find-leftmost-free-space [disk]
  (.indexOf disk "."))

(defn find-rightmost-file [disk]
  (loop [pos (dec (count disk))]
    (if (< pos 0)
      nil
      (if (number? (disk pos))
        pos
        (recur (dec pos))))))

(defn move-file-left [disk from to length]
  (let [file-id (disk from)
        result (transient disk)]
    (assoc! result from ".")
    (assoc! result to file-id)
    (persistent! result)))

(defn compact-disk [disk]
  (loop [current disk
         free-pos (find-leftmost-free-space current)
         file-pos (find-rightmost-file current)]
    (if (or (nil? free-pos)
            (nil? file-pos)
            (>= free-pos file-pos))
      current
      (let [file-id (current file-pos)
            file-length (count (take-while #(= file-id %) (subvec current file-pos)))
            next-disk (move-file-left current file-pos free-pos file-length)]
        (recur next-disk
               (find-leftmost-free-space next-disk)
               (find-rightmost-file next-disk))))))

(defn calculate-checksum [disk]
  (let [len (count disk)]
    (loop [idx 0
           sum 0]
      (if (>= idx len)
        sum
        (let [val (disk idx)]
          (recur (inc idx)
                 (if (number? val)
                   (+ sum (* idx val))
                   sum)))))))

(defn solve-part1 [input]
  (let [lengths (parse-input (str/trim input))  ; Make sure to trim the input
        initial-disk (create-disk-map lengths)
        compacted-disk (compact-disk initial-disk)]
    (calculate-checksum compacted-disk)))

(defn find-file-bounds [disk file-id]
  (let [len (count disk)]
    (loop [start nil
           pos 0]
      (cond
        (>= pos len) [start (dec pos)]
        (and (nil? start) (= (disk pos) file-id)) (recur pos (inc pos))
        (and start (not= (disk pos) file-id)) [start (dec pos)]
        :else (recur start (inc pos))))))

(defn find-free-space [disk size]
  (let [len (count disk)]
    (loop [pos 0]
      (when (< pos len)
        (if (= (disk pos) ".")
          (let [free-count (count (take-while #(= % ".") (subvec disk pos)))]
            (if (>= free-count size)
              pos
              (recur (+ pos free-count))))
          (recur (inc pos)))))))

(defn move-whole-file [disk from to size]
  (let [file-id (disk from)
        result (transient disk)]
    (dotimes [i size]
      (assoc! result (+ from i) ".")
      (assoc! result (+ to i) file-id))
    (persistent! result)))

(defn compact-disk-v2 [disk]
  (let [file-ids (->> disk
                      (filter number?)
                      (distinct)
                      (sort >))]
    (loop [current disk
           [file-id & rest-ids] file-ids]
      (if (nil? file-id)
        current
        (let [[start end] (find-file-bounds current file-id)
              file-size (inc (- end start))
              free-pos (find-free-space current file-size)]
          (if (and free-pos (< free-pos start))
            (recur (move-whole-file current start free-pos file-size) rest-ids)
            (recur current rest-ids)))))))

(defn solve-part2 [input]
  (let [lengths (parse-input (str/trim input))
        initial-disk (create-disk-map lengths)
        compacted-disk (compact-disk-v2 initial-disk)]
    (calculate-checksum compacted-disk)))


(comment
  (solve-part1 (str/trim (slurp (clojure.java.io/resource "day9.txt")))) ;; 6415184586041
  (solve-part2 (str/trim (slurp (clojure.java.io/resource "day9.txt"))))) ;; 6415184586041
