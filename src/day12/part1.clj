(ns day12.part1
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (let [[region presents] (str/split line #": ")
        [height width] (str/split region #"x")
        presents (str/split presents #" ")
        height (parse-long height)
        width (parse-long width)
        presents (mapv parse-long presents)]
    [(* height width) presents]))

(defn read-instructions [path]
  (->> (slurp path)
       (str/split-lines)
       (drop 30)
       (map parse-line)))

(defn calculate-answer [[square presents]]
  (if (>= square (reduce + 0 (map #(* % 9) presents))) 1 0))

(defn run []
  (->> (read-instructions "src/day12/input.txt")
       (map calculate-answer)
       (reduce + 0)))

(run)
