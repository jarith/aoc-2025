(ns day4.part2
  (:require [clojure.java.io :as io])
  (:import (clojure.lang PersistentQueue)))

(def shifts [[-1 -1]
             [-1 0]
             [-1 1]
             [0 -1]
             [0 1]
             [1 -1]
             [1 0]
             [1 1]])

(defn read-instructions [path]
  (with-open [reader (io/reader path)]
    (vec (line-seq reader))))

(defn ->matrix [diagram]
  (mapv vec diagram))

(defn in-bounds? [rows cols [row col]]
  (and (<= 0 row (dec rows))
       (<= 0 col (dec cols))))

(defn ->neighbor-positions [rows cols [row col]]
  (->> shifts
       (map (fn [[drow dcol]] [(+ row drow) (+ col dcol)]))
       (filter (partial in-bounds? rows cols))))

(defn ->neighbor-count [diagram rows cols pos]
  (->> (->neighbor-positions rows cols pos)
       (filter #(= \@ (get-in diagram %)))
       count))

(defn ->queue [diagram rows cols]
  (->> (for [r (range rows)
             c (range cols)
             :let [pos [r c]
                   cell  (get-in diagram pos)
                   neighbor-count (->neighbor-count diagram rows cols pos)]
             :when (and (= cell \@)
                        (< neighbor-count 4))]
         pos)
       (into PersistentQueue/EMPTY)))

(defn count-paper-rolls [input]
  (let [diagram (->matrix input)
        rows (count diagram)
        cols (count (first diagram))]
    (loop [diagram diagram
           queue (->queue diagram rows cols)
           removed 0]
      (if-let [pos (peek queue)]
        (let [queue' (pop queue)]
          (if (not= \@ (get-in diagram pos))
            (recur diagram queue' removed)
            (let [neighbor-count (->neighbor-count diagram rows cols pos)]
              (if (< neighbor-count 4)
                (let [diagram' (assoc-in diagram pos \.)
                      neighbors (for [cell (->neighbor-positions rows cols pos)
                                      :when (= \@ (get-in diagram' cell))]
                                  cell)
                      queue'' (into queue' neighbors)]
                  (recur diagram' queue'' (inc removed)))
                (recur diagram queue' removed)))))
        removed))))

(defn run []
  (-> (read-instructions "src/day4/input.txt")
      count-paper-rolls))

(run)
