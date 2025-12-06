(ns day6.part2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-lines [s]
  (let [lines      (str/split-lines s)
        op-line    (last lines)
        num-lines  (butlast lines)
        width      (apply max (map count (conj (vec num-lines) op-line)))
        pad        (fn [line] (format (str "%-" width "s") line))
        num-lines  (mapv pad num-lines)
        op-line    (pad op-line)
        separator? (fn [col]
                     (and (every? #(Character/isWhitespace (.charAt ^String % col)) num-lines)
                          (Character/isWhitespace (.charAt ^String op-line col))))
        separators (filterv separator? (range width))
        boundaries (distinct (concat [-1] separators [width]))
        problems   (for [[start end] (partition 2 1 boundaries)
                         :let [start (inc start)]
                         :when (< start end)]
                     (let [op (->> (range start end)
                                   (map #(.charAt ^String op-line %))
                                   (filter #(not (Character/isWhitespace %)))
                                   first str)
                           numbers (->> (range (dec end) (dec start) -1)
                                        (map (fn [col]
                                               (->> num-lines
                                                    (map #(.charAt ^String % col))
                                                    (filter #(Character/isDigit %))
                                                    (apply str))))
                                        (remove str/blank?))]
                       {:op op :numbers (map parse-long numbers)}))]
    problems))

(defn read-instructions [path]
  (with-open [reader (io/reader path)]
    (->> (vec (line-seq reader))
         (str/join "\n")
         (parse-lines))))

(defn run []
  (->> (read-instructions "src/day6/input.txt")
       (map (fn [{:keys [op numbers]}]
              (case op
                "*" (reduce * numbers)
                "+" (reduce + numbers))))
       (reduce +)))

(run)
