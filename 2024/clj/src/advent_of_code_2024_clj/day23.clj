(ns advent-of-code-2024-clj.day23
  (:require
   [clojure.java.io :as io]
   [hashp.core]
   [clojure.string :as str]
   [clojure.math.combinatorics :as combo]))

(defn parse-connections [input]
  (->> (str/split-lines input)
       (reduce (fn [acc line]
                 (let [[a b] (str/split line #"-")]
                   (-> acc
                       (update a (fnil conj #{}) b)
                       (update b (fnil conj #{}) a))))
               {})))

(defn find-connected-triples [graph]
  (let [nodes (keys graph)]
    (->> (for [a nodes
               b nodes
               c nodes
               :when (and (not= a b)
                         (not= b c)
                         (not= a c)
                         (contains? (get graph a) b)
                         (contains? (get graph b) c)
                         (contains? (get graph a) c))]
           #{a b c})
         (into #{}))))

(defn has-t-computer? [triple]
  (some #(str/starts-with? % "t") triple))

(defn solve-part1 [input]
  (->> input
       parse-connections
       find-connected-triples
       (filter has-t-computer?)
       count))

(defn fully-connected? [graph computers]
  (every? (fn [[a b]]
            (contains? (get graph a) b))
          (for [a computers
                b computers
                :when (not= a b)]
            [a b])))

(defn bron-kerbosch
  ([graph] (bron-kerbosch graph #{} (set (keys graph)) #{}))
  ([graph r p x]
   (if (and (empty? p) (empty? x))
     [r]
     (let [pivot (first (or (seq p) (seq x)))
           nodes (if pivot
                  (remove #(contains? (get graph pivot) %) p)
                  p)]
       (mapcat
        (fn [v]
          (let [neighbors (get graph v)]
            (bron-kerbosch
             graph
             (conj r v)
             (set (filter neighbors p))
             (set (filter neighbors x)))))
        nodes)))))

(defn find-largest-clique [graph]
  (->> (bron-kerbosch graph)
       (sort-by count >)
       first))

(defn solve-part2 [input]
  (->> input
       parse-connections
       find-largest-clique
       sort
       (str/join ",")))

(comment
  (solve-part1 (str/trim (slurp (io/resource "day23.txt")))) ;; 1083
  (solve-part2 (str/trim (slurp (io/resource "day23.txt"))))) ;; "as,bu,cp,dj,ez,fd,hu,it,kj,nx,pp,xh,yu"