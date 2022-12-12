(ns outergod.day-12
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.data.priority-map :refer [priority-map-keyfn]]))

(def input (slurp (io/resource "day-12")))
(def example (slurp (io/resource "day-12-example")))

(defn grid [lines]
  (into {}
        (apply concat
               (map-indexed (fn [y line]
                              (map-indexed (fn [x c] [[x y] c]) line))
                            lines))))

(defn heightmap [grid]
  (reduce-kv (fn [m k v] (assoc m k (case v
                                      \S 0
                                      \E 25
                                      (- (int v) 97))))
             {} grid))

(defn grid-ends [grid]
  (reduce-kv (fn [m k v] (case v
                           \S (assoc m :start k)
                           \E (assoc m :end k)
                           m))
             {} grid))

(defn grid-starts [grid]
  (reduce-kv (fn [set k v] (if (#{\a \S} v) (conj set k) set))
             #{} grid))

(defn parse [input]
  (let [lines (s/split-lines input)
        grid (grid lines)]
    (merge (grid-ends grid) {:heightmap (heightmap grid)
                             :starts (grid-starts grid)
                             :width (count (first lines))
                             :height (count lines)})))

(defn euclidian-distance
  ([[x y]] (Math/sqrt (+ (Math/pow x 2) (Math/pow y 2))))
  ([[x0 y0] [x1 y1]] (euclidian-distance [(- x1 x0) (- y1 y0)])))

(defn neighbors [[x y]]
  (map (fn [[a b]] [(+ x a) (+ y b)]) [[0 1] [1 0] [0 -1] [-1 0]]))

(defn in-limits? [[x y] [w h]]
  (and (>= x 0)
       (>= y 0)
       (< x w)
       (< y h)))

(defn a*-seq [[x0 y0] [x1 y1] [w h] heightmap]
  (let [in-limits? #(in-limits? % [w h])]
    (letfn [(h [[x y]] (euclidian-distance [x y] [x1 y1]))
            (f [[x y] g] (+ g (h [x y])))
            (n [[x y] g parents fringe closed]
              (let [parents (conj parents [x y])
                    h0 (heightmap [x y])]
                (keep (fn [[x y]]
                        (let [g (inc g)
                              cost (f [x y] g)
                              [prev-cost] (fringe [x y])]
                          (when-not (and prev-cost (<= prev-cost cost))
                            [[x y] [cost g parents]])))
                      (filter (fn [[x y]]
                                (let [h1 (heightmap [x y])]
                                  (and (in-limits? [x y]) (<= (- h1 h0) 1))))
                              (remove closed (neighbors [x y]))))))]
      (iterate
       (fn [[state fringe closed goal]]
         (if (= state :stop)
           [state fringe closed goal]
           (let [[[x y] [cost distance parents] :as node] (peek fringe)
                 [[x-goal y-goal] [cost-goal _ parents-goal] :as node-goal] (peek goal)]
             (cond (= [x y] [x1 y1]) [:continue (pop fringe) closed (conj goal node)]
                   (and (not-empty node-goal)
                        (or (empty? node)
                            (< cost-goal cost))) [:stop (conj parents-goal [x-goal y-goal])]
                   node [:continue (into (pop fringe) (n [x y] distance parents fringe closed)) (conj closed [x y]) goal]))))
       [:continue
        (priority-map-keyfn first [x0 y0] [(h [x0 y0]) 0 []])
        #{}
        (priority-map-keyfn first)]))))

(defn a*
  [[x0 y0] [x1 y1] [w h] heightmap limit]
  (let [[state path] (last (take limit (a*-seq [x0 y0] [x1 y1] [w h] heightmap)))]
    (if (= state :stop) path nil)))

(defn solve-1 [input]
  (let [{:keys [start end heightmap width height]} (parse input)]
    (dec (count (a* start end [width height] heightmap 10000)))))

(defn solve-2 [input]
  (let [{:keys [start end starts heightmap width height]} (parse input)]
    (->> starts
         (map (fn [start] (a* start end [width height] heightmap 10000)))
         (filter identity)
         (map (comp dec count))
         sort first)))
