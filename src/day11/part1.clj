(ns day11.part1
  (:require [clojure.string :as str]))

(defn build-graph [lines]
  (->> lines
       (map #(str/split % #": "))
       (reduce (fn [acc [vertex edges]]
                 (assoc acc vertex (vec (str/split edges #" "))))
               {})))

(defn read-instructions [path]
  (->> (slurp path)
       (str/split-lines)
       (build-graph)))

(defn count-paths [graph]
  (loop [stack (graph "you")
         paths 0]
    (if (empty? stack)
      paths
      (let [vertex (peek stack)
            stack (pop stack)]
        (if (= vertex "out")
          (recur stack (inc paths))
          (recur (into stack (graph vertex)) paths))))))

(defn run []
  (->> (read-instructions "src/day11/input.txt")
       (count-paths)))

(run)