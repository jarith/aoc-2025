(ns day5.part1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-instructions [path]
  (with-open [reader (io/reader path)]
    (let [lines (vec (line-seq reader))
          [intervals ids] (split-with (complement str/blank?) lines)
          ids (drop-while str/blank? ids)]
      [(->> (map #(str/split % #"-") intervals)
            (mapv (fn [[start end]]
                    [(parse-long start) (parse-long end)]))
            (sort-by first))
       (map parse-long ids)])))

(defn compact-intervals [intervals]
  (reduce
   (fn [acc [start end :as interval]]
     (if-let [[acc-start acc-end] (peek acc)]
       (cond
         (> start acc-end)
         (conj acc interval)

         (<= end acc-end)
         acc

         :else
         (conj (pop acc) [acc-start end]))
       (conj acc interval)))
   []
   intervals))

(defn run []
  (let [[intervals ids] (read-instructions "src/day5/input.txt")
        intervals' (compact-intervals intervals)]
    (->> (for [id ids
               [start end] intervals']
           (if (<= start id end) 1 0))
         (reduce +))))

(run)
