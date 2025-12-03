(ns day2.part2
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

(defn produce-block [times, [from, to]]
  (for [block (range from to)]
    (bigint (apply str (repeat times block)))))

(defn produce-seq [digits-count]
  (cond
    (= digits-count 1)
    '()

    (even? digits-count)
    (let [block-lengths (for [d (range 1 (inc (quot digits-count 2)))
                              :when (zero? (mod digits-count d))]
                          d)]
      (->> block-lengths
           (mapcat
            (fn [block-length]
              (let [times  (quot digits-count block-length)
                    from  (pow10 (dec block-length))
                    to    (pow10 block-length)]
                (produce-block times [from to]))))
           distinct))

    (or (> (mod digits-count 3) 0)
        (= digits-count 3))
    (produce-block digits-count [1 10])

    (= (mod digits-count 3) 0)
    (let [times  (quot digits-count 3)
          from  (pow10 2)
          to    (pow10 3)]
      (produce-block times [from to]))))


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