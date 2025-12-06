(ns day6.part1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-lines [lines]
  (->> lines
       (map str/trim)
       (remove str/blank?)
       (map #(str/split % #"\s+"))
       (apply mapv vector)
       (map reverse)))

(defn read-instructions [path]
  (with-open [reader (io/reader path)]
    (->> (line-seq reader)
         (parse-lines))))

(defn run []
  (->> (read-instructions "src/day6/input.txt")
       (map (fn [v]
              (let [[op & numbers] v
                    numbers' (map #(parse-long %) numbers)]
                (cond
                  (= op "*")
                  (reduce * numbers')

                  (= op "+")
                  (reduce + numbers')))))
       (reduce +)))

(run)