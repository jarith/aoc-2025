(ns day4.part1
  (:require [clojure.java.io :as io]))

(defn read-instructions [path]
  (with-open [reader (io/reader path)]
    (into [] (line-seq reader))))

(def shifts [[-1 -1]
             [-1 0]
             [-1 1]
             [0 -1]
             [0 1]
             [1 -1]
             [1 0]
             [1 1]])

(defn in-bounds? [rows cols [r c]]
  (and (<= 0 r)
       (< r rows)
       (<= 0 c)
       (< c cols)))

(defn neighbors [row col]
  (map (fn [[dr dc]] [(+ row dr) (+ col dc)]) shifts))

(defn count-paper-rolls [diagram]
  (let [rows (count diagram)
        cols (count (first diagram))]
    (->> (for [row (range rows)
               col (range cols)
               :let [pos [row col]
                     cell (get-in diagram pos)
                     around (->> (neighbors row col)
                                 (filter (partial in-bounds? rows cols))
                                 (filter #(= \@ (get-in diagram %)))
                                 count)]
               :when (and (= cell \@) (< around 4))]
           1)
         (reduce +))))

(defn run []
  (->> (read-instructions "src/day4/input.txt")
       (count-paper-rolls)))

(run)