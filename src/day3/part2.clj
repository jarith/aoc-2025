(ns day3.part2
  (:require [clojure.java.io :as io]))

(defn read-instructions [path]
  (with-open [reader (io/reader path)]
    (into [] (line-seq reader))))

(defn max-digit [block]
  (->> block
       (map-indexed vector)
       (reduce (fn [acc pair]
                 (if (> (long (second pair)) (long (second acc)))
                   pair
                   acc)))))

(defn max-joltage [n batteries]
  (let [len (count batteries)]
    (loop [i   0
           pos 0
           acc ""]
      (if (= i n)
        acc
        (let [remaining (- n i)
              end (- len remaining)
              block (subs batteries pos (inc end))
              [idx battery] (max-digit block)
              new-pos (inc (+ pos idx))]
          (recur (inc i) new-pos (str acc battery)))))))

(defn run []
  (->> (read-instructions "src/day3/input.txt")
       (map #(max-joltage 12 %))
       (map #(Long/parseLong %))
       (reduce + 0)))

(run)