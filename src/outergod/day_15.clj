(ns outergod.day-15
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as set]))

(def input (slurp (io/resource "day-15")))
(def example (slurp (io/resource "day-15-example")))

(defn sensor-distance [sensor beacon]
  (let [[x0 y0] sensor
        [x1 y1] beacon]
    (+ (abs (- x1 x0)) (abs (- y1 y0)))))

(defn parse-line [line]
  (let [[_ & nums] (re-matches #"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)" line)
        [x0 y0 x1 y1] (map #(Integer/parseInt %) nums)]
    {:sensor [x0 y0] :beacon [x1 y1] :signal (sensor-distance [x0 y0] [x1 y1])}))

(defn sensor-row-coverage [y sensor signal]
  (let [[x0 y0] sensor
        n (- signal (abs (- y y0)))]
    (map #(vector (+ % x0) y) (range (- n) (inc n)))))

(defn parse [input]
  (->> input s/split-lines (map parse-line)))

(defn manhattan-outline [n]
  (mapcat
   (fn [y]
     (let [width (- n (abs y))]
       [[(- width) y] [width y]]))
   (range (- n) (inc n))))

(defn solve-1 [input y]
  (let [coll (parse input)
        beacons (set (map :beacon coll))]
    (as-> coll v
         (mapcat (fn [{:keys [sensor signal]}] (sensor-row-coverage y sensor signal)) v)
         (set v)
         (set/difference v beacons)
         (count v))))

(defn solve-2 [input n]
  (let [coll (parse input)
        outlines (mapcat (fn [{:keys [sensor signal]}]
                           (filter (fn [[x y]]
                                     (and (>= x 0) (>= y 0)
                                          (<= x n) (<= y n)))
                                   (map (partial mapv + sensor)
                                        (manhattan-outline (inc signal)))))
                         coll)
        [x y] (some
               (fn [beacon] (when (not-any?
                                   (fn [{:keys [sensor signal]}]
                                     (<= (sensor-distance sensor beacon) signal))
                                   coll)
                              beacon))
               outlines)]
    (+ (* x n) y)))

(comment
  "Too resource hungry"
  (defn manhattan-area [n]
    (mapcat
     (fn [y]
       (map
        (fn [x] [x y])
        (let [width (- n (abs y))]
          (range (- width) (inc width)))))
     (range (- n) (inc n))))

  (defn manhattan-areas
    ([n] (lazy-seq (cons (manhattan-area n) (manhattan-areas (inc n)))))
    ([] (manhattan-areas 0)))

  (def areas (manhattan-areas))

  (defn sensor-area [sensor beacon]
    (map (partial mapv + sensor)
         (nth areas (sensor-distance sensor beacon))))

  (defn sensor-areas [coll]
    (set (mapcat (fn [{:keys [sensor beacon]}] (sensor-area sensor beacon)) coll)))

  (defn solve-1 [input y]
    (let [coll (parse input)
          beacons (set (map :beacon coll))]
      (->> coll sensor-areas (filter (fn [[_ y1]] (= y y1))) (remove beacons))))
  
  (defn solve-2 [input n]
    (let [coll (parse input)]
      (some
       (fn [beacon] (when (not-any?
                           (fn [{:keys [sensor signal]}]
                             (<= (sensor-distance sensor beacon) signal))
                           coll)
                      beacon))
       (for [x (range 0 (inc n)) y (range 0 (inc n))] [x y])))))
