(ns day1.part2
  (:require [clojure.java.io :as io]))

(defn parse-line [line]
  (let [[direction & digits] line
        value (Long/parseLong (apply str digits))]
    [direction value]))

(defn read-instructions [path]
  (with-open [reader (io/reader path)]
    (->> (line-seq reader)
         (map parse-line)
         (into []))))

(defn crossed-zero? [initial result]
  (or (and (neg? initial) (not (neg? result)))
      (and (pos? initial) (not (pos? result)))))

(defn step [direction sum value]
  (let [op (case direction
             \R +
             \L -)
        new-sum (op sum value)
        quotient (quot new-sum 100)
        wrapped? (crossed-zero? sum new-sum)
        clicks (+ (Math/abs (long quotient))
                  (if wrapped? 1 0))
        wrapped-sum (mod new-sum 100)]
    [wrapped-sum clicks]))

(defn run []
  (->> (read-instructions "src/day1/input.txt")
       (reduce (fn [[sum total-clicks] [direction value]]
                 (let [[new-sum extra-clicks] (step direction sum value)]
                   [new-sum (+ total-clicks extra-clicks)]))
               [50 0])
       second))

(run)
