(ns outergod.day-23
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as set]))

(def input (slurp (io/resource "day-23")))
(def example (slurp (io/resource "day-23-example")))
(def simple (slurp (io/resource "day-23-simple")))

(defn parse [s]
  (reduce into #{}
          (map-indexed
           (fn [y line] (keep-indexed (fn [x c] (when (= c \#) [x y])) line))
           (s/split-lines s))))

(def neighbors (remove #{[0 0]} (for [x (range -1 2) y (range -1 2)] [x y])))
(def north (filter (fn [[_ y]] (= y -1)) neighbors))
(def east (filter (fn [[x _]] (= x 1)) neighbors))
(def south (filter (fn [[_ y]] (= y 1)) neighbors))
(def west (filter (fn [[x _]] (= x -1)) neighbors))

(defn direction-filter [dir] (case dir :north north :east east :south south :west west))
(defn direction-vec [dir] (case dir :north [0 -1] :east [1 0] :south [0 1] :west [-1 0]))
(def init-directions [:north :south :west :east])

(defn propose [state directions]
  (map (fn [pos]
         [pos (if (not-any? state (map (partial mapv + pos) neighbors))
                pos
                (if-let [pos (some (fn [dir] (when (not-any? state (map (partial mapv + pos) (direction-filter dir)))
                                               (mapv + pos (direction-vec dir))))
                                   directions)]
                  pos pos))])
       state))

(defn move [proposal]
  (set (mapcat (fn [[pos origins]] (if (= 1 (count origins)) [pos] (map first origins)))
               (group-by second proposal))))

(defn iter [state directions]
  [(move (propose state directions)) (conj (subvec directions 1) (first directions))])

(defn state-seq [state directions]
  (cons [state directions]
        (lazy-seq (apply state-seq (iter state directions)))))

(defn empty-area [state]
  (let [[x0 y0 x1 y1] (reduce (fn [[x0 y0 x1 y1] [x y]] [(min x0 x) (min y0 y) (max x1 x) (max y1 y)])
                              [Integer/MAX_VALUE Integer/MAX_VALUE 0 0] state)]
    (- (* (inc (- x1 x0)) (inc (- y1 y0))) (count state))))

(defn solve-1 [s]
  (empty-area (first (nth (state-seq (parse s) init-directions) 10))))

(defn solve-2 [s]
  (loop [[current & seq] (state-seq (parse s) init-directions) i 0 previous nil]
    (let [current (first current)]
      (if (= current previous)
        i
        (recur seq (inc i) current)))))

(defn visualize [state]
  (let [[x0 y0 x1 y1] (reduce (fn [[x0 y0 x1 y1] [x y]] [(min x0 x) (min y0 y) (max x1 x) (max y1 y)])
                              [Integer/MAX_VALUE Integer/MAX_VALUE 0 0] state)]
    (s/join \newline (map (fn [y] (s/join (map (fn [x] (if (state [x y]) \# \.)) (range x0 (inc x1))))) (range y0 (inc y1))))))
