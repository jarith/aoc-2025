(ns day7.part2
  (:require [clojure.string :as str]))

(defn read-instructions [path]
  (->> (slurp path)
       (str/split-lines)))

(defn find-start [diagram]
  (let [first-row (apply str (first diagram))]
    [0 (str/index-of first-row "S")]))

(defn count-beam-split-timelines [diagram]
  (let [rows       (count diagram)
        cols       (count (first diagram))
        in-bounds? (fn [[row col]] (and (< -1 row rows) (< -1 col cols)))
        start      (find-start diagram)
        cache      (atom {})]
    (letfn [(trace [diagram [row col :as cell]]
              (if-let [cached (get @cache cell)]
                cached
                (let [result
                      (let [sym (get-in diagram cell)]
                        (case sym
                          (\. \S)
                          (let [below [(inc row) col]]
                            (if (in-bounds? below)
                              (+ 0 (trace diagram below))
                              1))

                          \^
                          (let [left-col (dec col)
                                right-col (inc col)
                                left-in-bounds? (in-bounds? [row left-col])
                                right-in-bounds? (in-bounds? [row right-col])]
                            (cond
                              (and left-in-bounds? right-in-bounds?)
                              (+
                               (trace diagram [row left-col])
                               (trace diagram [row right-col]))

                              left-in-bounds?
                              (trace diagram [row left-col])

                              right-in-bounds?
                              (trace diagram [row right-col])

                              :else
                              0))))]
                  (swap! cache assoc cell result)
                  result)))]
      (trace diagram start))))

(defn run []
  (->> (read-instructions "src/day7/input.txt")
       (count-beam-split-timelines)))

(run)