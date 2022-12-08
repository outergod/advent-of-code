(ns outergod.day-8
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input (slurp (io/resource "day-8")))

(defn grid-table [grid]
  (let [len (count (first grid))]
    {:len len
     :grid (into {} (for [x (range 0 len) y (range 0 len)]
                      [[x y] (-> grid (nth y) (nth x) str Integer/parseInt)]))}))

(defn views [len]
  (concat
     (for [y (range 0 len)]
       (for [x (range 0 len)]
         [x y]))
     (for [x (range 0 len)]
       (for [y (range 0 len)]
         [x y]))
     (for [y (reverse (range 0 len))]
       (for [x (reverse (range 0 len))]
         [x y]))
     (for [x (reverse (range 0 len))]
       (for [y (reverse (range 0 len))]
         [x y]))))

(defn solve-1 [input]
  (let [{:keys [len grid]} (-> input s/split-lines grid-table)
        visible (into {} (for [x (range 0 len) y (range 0 len)] [[x y] false]))
        views (views len)]
    (->> (reduce (fn [visible view]
                  (first (reduce (fn [[visible max] pos]
                                   (let [height (grid pos)]
                                     (if (> height max)
                                       [(assoc visible pos true) height]
                                       [visible max])))
                                 [visible -1] view)))
                visible views)
        vals (filter identity) count)))

(defn solve-2 [input]
  (let [{:keys [len grid]} (-> input s/split-lines grid-table)
        scores (into {} (for [x (range 0 len) y (range 0 len)] [[x y] 1]))
        views (views len)]
    (->> (reduce (fn [scores view]
                  (first (reduce (fn [[scores seen] pos]
                                   (let [height (grid pos)]
                                     [(update scores pos (partial * (nth seen height)))
                                      (concat (repeat (inc height) 1) (map inc (drop (inc height) seen)))]))
                                 [scores (repeat 10 0)] view)))
                scores views)
         vals (apply max))))
