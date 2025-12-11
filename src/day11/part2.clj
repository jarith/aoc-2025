(ns day11.part2
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
  (let [helper (atom nil)
        _ (reset! helper
                  (memoize
                   (fn [vertex fft-reached dac-reached]
                     (let [fft-reached (or fft-reached (= vertex "fft"))
                           dac-reached (or dac-reached (= vertex "dac"))]
                       (if (= vertex "out")
                         (if (and fft-reached dac-reached) 1 0)
                         (reduce + 0 (map #(@helper % fft-reached dac-reached)
                                          (graph vertex))))))))]
    (@helper "svr" false false)))

(defn run []
  (->> (read-instructions "src/day11/sample.txt")
       (count-paths)))

(run)