(ns day2.part1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-line [line]
  (->> (str/split line #",")
       (map #(str/split % #"-"))))

(defn read-instructions [path]
  (with-open [reader (io/reader path)]
    (some->>
     (first (line-seq reader))
     (parse-line))))

(defn pow10 [n]
  (->> (repeat n 10N)
       (reduce * 1N)))

(defn produce-seq [digits-count]
  (when (and (pos? digits-count)
             (even? digits-count))
    (let [half (/ digits-count 2)
          from (pow10 (dec half))
          to   (pow10 half)]
      (->> (range from to)
           (map #(+ (* % to) %))))))

(defn sum-seq [{:keys [digits-counts interval]}]
  (let [[start end] interval]
    (->> digits-counts
         (mapcat produce-seq)
         (filter #(<= start % end))
         (reduce + 0N))))

(defn get-digits-counts [[start end]]
  (let [start-digits-count (count start)
        end-digits-count (count end)]
    {:digits-counts  (range start-digits-count (inc end-digits-count))
     :interval [(Long/parseLong start) (Long/parseLong end)]}))

(defn run []
  (->> (read-instructions "src/day2/input.txt")
       (map get-digits-counts)
       (map sum-seq)
       (reduce + 0N)))

(run)