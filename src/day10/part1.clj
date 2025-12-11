(ns day10.part1
  (:require [clojure.string :as str])
  (:import [clojure.lang PersistentQueue]))

(defn parse-lights [lights-token]
  (let [lights   (subs lights-token 1 (dec (count lights-token)))
        bit-str (->> lights
                     (map #(case %
                             \. \0
                             \# \1))
                     (apply str))]
    (Integer/parseInt bit-str 2)))

(defn parse-button [button-tokens bit-count]
  (let [button   (subs button-tokens 1 (dec (count button-tokens)))
        indexes (->> (str/split button #",")
                     (mapv parse-long))
        bits    (reduce (fn [acc idx] (assoc acc idx 1))
                        (vec (repeat bit-count 0))
                        indexes)
        bit-str (apply str bits)]
    (Integer/parseInt bit-str 2)))

(defn parse-line [line]
  (let [[lights-token & button-tokens] (drop-last (str/split line #" "))
        bit-count (- (count lights-token) 2)]
    {:lights  (parse-lights lights-token)
     :buttons (mapv #(parse-button % bit-count) button-tokens)}))

(defn read-instructions [path]
  (->> (slurp path)
       (str/split-lines)
       (mapv parse-line)))

(defn press-button [lights button]
  (bit-xor lights button))

(defn count-min-button-presses [{lights :lights
                                 buttons :buttons}]
  (loop [queue (conj PersistentQueue/EMPTY 0)
         dp    {0 0}]
    (if (empty? queue)
      (get dp lights)
      (let [current-lights (peek queue)
            queue (pop queue)
            presses-count (dp current-lights)]
        (if (= current-lights lights)
          presses-count
          (let [[queue dp] (reduce (fn [[queue dp] bits]
                                     (let [current-lights (press-button current-lights bits)]
                                       (if (contains? dp current-lights)
                                         [queue dp]
                                         [(conj queue current-lights)
                                          (assoc dp current-lights (inc presses-count))])))
                                   [queue dp]
                                   buttons)]
            (recur queue dp)))))))

(defn run []
  (->> (read-instructions "src/day10/input.txt")
       (map count-min-button-presses)
       (reduce + 0)))

(run)