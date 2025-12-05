(ns day5.part2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-instructions [path]
  (with-open [reader (io/reader path)]
    (->> (line-seq reader)
         (take-while (complement str/blank?))
         (map #(str/split % #"-"))
         (mapv (fn [[start end]]
                 [(parse-long start) (parse-long end)]))
         (sort-by first))))

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
  (->> (compact-intervals (read-instructions "src/day5/input.txt"))
       (map (fn [[start end]]
              (inc (- end start))))
       (reduce + 0)))

(run)