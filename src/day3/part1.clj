(ns day3.part1
  (:require [clojure.java.io :as io]))

(defn read-instructions [path]
  (with-open [reader (io/reader path)]
    (into [] (line-seq reader))))

(defn max-joltage
  ([batteries]
   (max-joltage 0 batteries))

  ([max-joltage block]
   (if-let [[battery1 & tail] (seq block)]
     (if-let [tail' (seq tail)]
       (let [battery2 (apply max-key int tail')
             joltage  (Integer/parseInt (str battery1 battery2))]
         (recur (max max-joltage joltage) tail))
       max-joltage)
     max-joltage)))

(defn run []
  (->> (read-instructions "src/day3/input.txt")
       (map max-joltage)
       (reduce + 0)))

(run)