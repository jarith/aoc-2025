(ns day9.part1
  (:require [clojure.string :as str]))

(defn read-instructions [path]
  (->> (slurp path)
       (str/split-lines)
       (map #(str/split % #","))
       (map #(mapv parse-long %))))

(defn ->square [[[x1 y1] [x2 y2]]]
  (* (inc (abs (- x1 x2)))
     (inc (abs (- y1 y2)))))

(defn run []
  (let [tiles (read-instructions "src/day9/input.txt")
        len (count tiles)]
    (->> (for [i (range len)
               j (range (inc i) len)]
           [(nth tiles i) (nth tiles j)])
         (map ->square)
         (apply max))))

(run)