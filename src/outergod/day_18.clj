(ns outergod.day-18
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as set]))

(def input (slurp (io/resource "day-18")))
(def example (slurp (io/resource "day-18-example")))

(defn parse-line [s]
  (mapv #(Integer/parseInt %) (s/split s #",")))

(defn parse [input]
  (set (map parse-line (s/split-lines input))))

(def neighbors
  (for [i (range 0 3) n [-1 1]] (assoc [0 0 0] i n)))

(defn surface [droplet pos]
  (->> neighbors (map (partial mapv + pos)) (remove droplet)))

(defn dimensions [droplet]
  (reduce (fn [[mins maxs] current]
            [(mapv min mins current) (mapv max maxs current)])
          [[Integer/MAX_VALUE Integer/MAX_VALUE Integer/MAX_VALUE] [0 0 0]]
          droplet))

(defn search [droplet]
  (let [[min max] (dimensions droplet)
        min (mapv - min [1 1 1])
        max (mapv + max [1 1 1])]
    (loop [open #{min} closed #{} found []]
      (if (empty? open)
        found
        (let [neighbors (filter (fn [pos] (every? identity (map <= min pos max)))
                                (mapcat #(map (partial mapv + %) neighbors) open))]
          (recur
           (set/difference (set neighbors) droplet closed)
           (into closed open)
           (into found (filter droplet neighbors))))))))

(defn solve-1 [input]
  (let [droplet (parse input)]
    (reduce + (map (partial (comp count surface) droplet) droplet))))

(defn solve-2 [input]
  (count (search (parse input))))
