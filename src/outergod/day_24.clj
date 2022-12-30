(ns outergod.day-24
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as set]
            [clojure.data.priority-map :refer [priority-map-keyfn]]))

(def input (slurp (io/resource "day-24")))
(def example (slurp (io/resource "day-24-example")))
(def complex (slurp (io/resource "day-24-complex")))

(defn parse [s]
  (let [lines (s/split-lines s)
        width (count (first lines))
        height (count lines)
        blizzards (mapcat (fn [y line]
                            (keep-indexed (fn [x c]
                                            (when (#{\<\>\^\v} c)
                                              [[x y] (case c \< :left \> :right \^ :up \v :down)]))
                                          line))
                          (range 0 height) lines)
        walls (set (mapcat (fn [y line]
                             (keep-indexed (fn [x c] (when (= \# c) [x y])) line))
                           (range 0 height) lines))]
    {:width width :height height :start [1 0] :goal [(- width 2) (- height 1)] :blizzards blizzards :walls walls}))

(defn delta [dir] (case dir :up [0 -1] :right [1 0] :down [0 1] :left [-1 0]))

(defn blizzard-iterate [blizzards width height]
  (let [width (- width 2)
        height (- height 2)]
    (map (fn [[pos dir]]
           (let [[x y] (mapv + pos (delta dir))
                 x (inc (mod (+ (dec x) width) width))
                 y (inc (mod (+ (dec y) height) height))]
             [[x y] dir]))
         blizzards)))

(defn blizzard-seq [blizzards width height]
  (cons blizzards (lazy-seq (blizzard-seq (blizzard-iterate blizzards width height) width height))))

(defn euclidian-distance
  ([[x y]] (Math/sqrt (+ (Math/pow x 2) (Math/pow y 2))))
  ([[x0 y0] [x1 y1]] (euclidian-distance [(- x1 x0) (- y1 y0)])))

(defn neighbors [pos] (mapv (partial mapv + pos) [[0 1] [1 0] [0 -1] [-1 0]]))

(defn in-limits? [[x y] [w h]] (and (>= x 0) (>= y 0) (< x w) (< y h)))

(defn a*-seq [[x0 y0] [x1 y1] [w h] walls blizzards offset]
  (let [in-limits? #(in-limits? % [w h])]
    (letfn [(h [[x y]] (euclidian-distance [x y] [x1 y1]))
            (f [[x y] g] (+ g (h [x y])))
            (n [[x y] g parents]
              (let [parents (conj parents [x y])
                    g (inc g)
                    os (into walls (map first (nth blizzards g)))]
                (map (fn [[x y]] (let [cost (f [x y] g)] [[x y g] [cost g parents]]))
                     (filter in-limits? (remove os (conj (neighbors [x y]) [x y]))))))]
      (iterate
       (fn [[state fringe goal]]
         (if (= state :stop)
           [state fringe goal]
           (let [[[x y i] [cost distance parents] :as node] (peek fringe)
                 [[x-goal y-goal] [cost-goal _ parents-goal] :as node-goal] (peek goal)]
             (cond (= [x y] [x1 y1]) [:continue (pop fringe) (conj goal node)]
                   (and (not-empty node-goal)
                        (or (empty? node)
                            (< cost-goal cost))) [:stop (conj parents-goal [x-goal y-goal])]
                   node [:continue (into (pop fringe) (n [x y] distance parents)) goal]))))
       [:continue
        (priority-map-keyfn first [x0 y0 offset] [(h [x0 y0]) offset []])
        (priority-map-keyfn first)]))))

(defn a* [start goal walls width height blizzards limit offset]
  (let [[state path] (last (take limit (a*-seq start goal [width height] walls blizzards offset)))]
    (if (= state :stop) path nil)))

(defn solve-1 [s]
  (let [{:keys [width height start goal blizzards walls]} (parse s)]
    (dec (count (a* start goal walls width height (blizzard-seq blizzards width height) 100000 0)))))

(defn solve-2 [s]
  (let [{:keys [width height start goal blizzards walls]} (parse s)
        seq (blizzard-seq blizzards width height)
        trip-1 (dec (count (a* start goal walls width height seq 100000 0)))
        trip-2 (dec (count (a* goal start walls width height seq 100000 trip-1)))
        trip-3 (dec (count (a* start goal walls width height seq 100000 (+ trip-1 trip-2))))]
    (+ trip-1 trip-2 trip-3)))
