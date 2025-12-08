(ns day8.part2
  (:require [clojure.string :as str]))

(defn read-instructions [path]
  (->> (slurp path)
       (str/split-lines)
       (map #(str/split % #","))
       (map #(mapv parse-long %))))

(defn diff-square [a b]
  (let [d (- a b)]
    (* d d)))

(defn distance [p1 p2]
  (Math/sqrt
   (reduce + (map diff-square p1 p2))))

(defn connected-component [graph start]
  (loop [stack   [start]
         visited #{}
         comp    #{}]
    (if-let [node (peek stack)]
      (if (visited node)
        (recur (pop stack) visited comp)
        (let [neighbors (get graph node #{})
              new-stack (into (pop stack) (remove visited neighbors))]
          (recur new-stack
                 (conj visited node)
                 (conj comp node))))
      comp)))

(defn find-last-edge [edges]
  (let [all-boxes (->> edges
                       (mapcat identity)
                       set)]
    (loop [remaining-edges edges
           circuit {}]
      (when-let [[box1 box2 :as edge] (first remaining-edges)]
        (let [circuit' (-> circuit
                           (update box1 (fnil conj #{}) box2)
                           (update box1 (fnil conj #{}) box2))
              component (connected-component circuit' box1)]
          (if (= (count component) (count all-boxes))
            edge
            (recur (rest remaining-edges) circuit')))))))

(defn run []
  (let [points (read-instructions "src/day8/input.txt")
        len (count points)
        edges (->> (for [i (range len)
                         j (range (inc i) len)]
                     [(nth points i) (nth points j)])
                   (sort-by (fn [[p1 p2]] (distance p1 p2))))
        [box1 box2] (find-last-edge edges)]
    (* (first box1) (first box2))))

(run)