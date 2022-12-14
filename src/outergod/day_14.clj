(ns outergod.day-14
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as set]))

(def input (slurp (io/resource "day-14")))
(def example (slurp (io/resource "day-14-example")))

(defn parse-tuple [s]
  (map #(Integer/parseInt %) (s/split s #",")))

(defn parse-line [line]
  (map parse-tuple (s/split line #" -> ")))

(defn parse [input]
  (->> input s/split-lines (map parse-line)))

(defn line-range [a b]
  (case (compare a b)
    -1 (range a (inc b))
    1 (range a (dec b) -1)
    0 (repeat a)))

(defn interpolate-path [path]
  (->> path
       (partition 2 1)
       (mapcat (fn [[[x0 y0] [x1 y1]]]
              (map vector (line-range x0 x1) (line-range y0 y1))))
       set))

(def fall [[0 1] [-1 1] [1 1]])
(def source [500 0])

(defn paths-obstacles [paths]
  (->> paths (map interpolate-path) (reduce set/union)))

(defn rock-bottom [rocks]
  (->> rocks (map second) (apply max)))

(defn solve-1 [input]
  (let [rocks (paths-obstacles (parse input))
        bottom (rock-bottom rocks)]
    (count
     (loop [active source sand #{}]
       (let [fall (map (partial mapv + active) fall)
             target (->> fall (remove sand) (remove rocks) first)]
         (cond
           (nil? target) (recur source (conj sand active))
           (>= (second target) bottom) sand
           :default (recur target sand)))))))

(defn solve-2 [input]
  (let [rocks (paths-obstacles (parse input))
        bottom (+ 2 (rock-bottom rocks))]
    (count
     (loop [active source sand #{}]
       (let [fall (map (partial mapv + active) fall)
             target (->> fall (remove sand) (remove rocks) first)]
         (cond
           (nil? target)
           (if (= active source)
             (conj sand active)
             (recur source (conj sand active)))

           (>= (second target) bottom)
           (recur source (conj sand active))

           :default (recur target sand)))))))
