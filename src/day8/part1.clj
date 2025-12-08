(ns day8.part1
  (:require [clojure.string :as str]
            [clojure.set :as set]))

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

(defn build-graph [edges]
  (reduce (fn [g [a b]]
            (-> g
                (update a (fnil conj #{}) b)
                (update b (fnil conj #{}) a)))
          {}
          edges))

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

(defn ->circuit-sizes [edges]
  (let [graph (build-graph edges)
        nodes (keys graph)]
    (loop [remaining (set nodes)
           sizes     []]
      (if-let [node (first remaining)]
        (let [component (connected-component graph node)]
          (recur (set/difference remaining component)
                 (conj sizes (count component))))
        sizes))))

(defn run []
  (let [points (read-instructions "src/day8/input.txt")
        len (count points)]
    (->> (for [i (range len)
               j (range (inc i) len)]
           [(nth points i) (nth points j)])
         (sort-by (fn [[p1 p2]]
                    (distance p1 p2)))
         (take 1000)
         (->circuit-sizes)
         (sort >)
         (take 3)
         (reduce * 1))))

(run)