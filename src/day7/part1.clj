(ns day7.part1
  (:require [clojure.string :as str])
  (:import [clojure.lang PersistentQueue]))

(defn read-instructions [path]
  (->> (slurp path)
       (str/split-lines)))

(defn find-start [diagram]
  (let [first-row (apply str (first diagram))]
    [0 (str/index-of first-row "S")]))

(defn count-beam-splits [diagram]
  (let [rows       (count diagram)
        cols       (count (first diagram))
        in-bounds? (fn [[row col]] (and (< -1 row rows) (< -1 col cols)))
        start      (find-start diagram)]
    (loop [queue        (conj PersistentQueue/EMPTY start)
           visited      #{start}
           splits-count 0]
      (if-let [[row col :as cell] (peek queue)]
        (let [queue (pop queue)
              sym   (get-in diagram cell)]
          (case sym
            (\. \S)
            (let [below [(inc row) col]]
              (if (and (in-bounds? below) (not (visited below)))
                (recur (conj queue below) (conj visited below) splits-count)
                (recur queue visited splits-count)))

            \^
            (let [neighbors [[row (dec col)] [row (inc col)]]
                  new-cells (filterv #(and (in-bounds? %) (not (visited %)))
                                     neighbors)]
              (recur (into queue new-cells)
                     (into visited new-cells)
                     (inc splits-count)))

            (recur queue visited splits-count)))
        splits-count))))

(defn run []
  (->> (read-instructions "src/day7/input.txt")
       (count-beam-splits)))

(run)