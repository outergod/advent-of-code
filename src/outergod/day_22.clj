(ns outergod.day-22
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as set]))

(def input (slurp (io/resource "day-22")))
(def example (slurp (io/resource "day-22-example")))

(defn split-inputs [s]
  (let [lines (s/split-lines s)]
    [(take (- (count lines) 2) lines) (last lines)]))

(defn parse-map [lines]
  (into {} (mapcat (fn [y line]
                     (keep-indexed (fn [x c]
                                     (let [x (inc x)]
                                       (case c \. [[x y] :floor] \# [[x y] :obstacle] \space nil))) line))
                   (range 1 (inc (count lines))) lines)))

(defn parse-instructions [s]
  (map #(case % "L" :left "R" :right (Integer/parseInt %)) (re-seq #"(?:\d+|R|L)" s)))

(defn parse [s]
  (let [[m ins] (split-inputs s)]
    [(parse-map m) (parse-instructions ins)]))

(defn init [s]
  (let [[m ins] (parse s)
        left (reduce (fn [acc [[x y] type]] (if (and (= y 1) (= type :floor)) (min acc x) acc)) Integer/MAX_VALUE m)]
    {:m m :instructions ins :position [left 1] :direction :right}))

(defn delta [dir] (case dir :up [0 -1] :right [1 0] :down [0 1] :left [-1 0]))
(def directions [:up :right :down :left])

(defn turn [x y]
  (nth directions (mod (+ (.indexOf directions x) (case y :left -1 :right 1) (count directions)) (count directions))))

(defn move [m pos dir]
  (let [[x y] (mapv + pos (delta dir))
        [x y] (if (contains? m [x y]) [x y]
                  (case dir
                    :up [x (reduce (fn [acc [x2 y2]] (if (= x x2) (max acc y2) acc)) y (keys m))]
                    :right [(reduce (fn [acc [x2 y2]] (if (= y y2) (min acc x2) acc)) x (keys m)) y]
                    :down [x (reduce (fn [acc [x2 y2]] (if (= x x2) (min acc y2) acc)) y (keys m))]
                    :left [(reduce (fn [acc [x2 y2]] (if (= y y2) (max acc x2) acc)) x (keys m)) y]))]
    (case (m [x y])
      :floor [x y]
      :obstacle pos)))

(def facing-values [:right :down :left :up])

(defn play [setup]
  (let [{:keys [m instructions position direction]} setup]
    (loop [[ins & instructions] instructions position position direction direction]
      (if (nil? ins)
        [position direction]
        (case ins
          (:right :left) (recur instructions position (turn direction ins))
          (recur instructions (reduce (fn [pos _] (move m pos direction)) position (range 0 ins)) direction))))))

(defn solve-1 [s]
  (let [[[x y] dir] (play (init s))]
    (+ (* 1000 y) (* 4 x) (.indexOf facing-values dir))))

;; Learned and adapted from https://www.reddit.com/user/maneatingape/

(defn cross* [v1 v2]
  (let [[x1 y1 z1] v1
        [x2 y2 z2] v2]
    [(- (* y1 z2) (* z1 y2)) (- (* z1 x2) (* x1 z2)) (- (* x1 y2) (* y1 x2))]))

(defn vec* [v n]
  (mapv (partial * n) v))

(defn vec+ [v & vs]
  (reduce (partial mapv +) v vs))

(defn vec- [v & vs]
  (reduce (partial mapv -) v vs))

(defn parse-cube [s]
  (let [{:keys [m instructions position direction]} (init s)
        [width height] (reduce (fn [[x1 y1] [x2 y2]] [(max x1 x2) (max y1 y2)]) [0 0] (keys m))
        size (int (/ (min width height) 3))
        scale-xy (dec size)
        scale-z (inc size)
        left (reduce (fn [acc [x y]] (if (= y 1) (min acc x) acc)) Integer/MAX_VALUE (keys m))
        start {:pos-2d [left 1] :axis-x [1 0 0] :axis-y [0 1 0] :axis-z [0 0 1]}]
    (loop [[position & queue] [start] closed #{} cube {}]
      (if (nil? position)
        {:cube cube :m m :instructions instructions :position [(- scale-xy) (- scale-xy) (- scale-z)] :direction [2 0 0]}
        (let [{:keys [pos-2d axis-x axis-y axis-z]} position
              area (for [x (range 0 size) y (range 0 size)
                         :let [pos (vec+ pos-2d [x y])
                               key (vec+ (vec* axis-x (- (* x 2) scale-xy)) (vec* axis-y (- (* y 2) scale-xy)) (vec* axis-z (- scale-z)))]]
                     [key {:pos pos :axis-x axis-x :axis-y axis-y :axis-z axis-z}])
              neighbors (remove closed (filter m (map (partial vec+ pos-2d) [[size 0] [(- size) 0] [0 size] [0 (- size)]])))]
          (recur
           (into queue
                 (map (fn [pos]
                        (condp = (vec- pos pos-2d)
                          [0 (- size)] {:pos-2d pos :axis-x axis-x :axis-y (cross* axis-y axis-x) :axis-z (cross* axis-z axis-x)}
                          [size 0] {:pos-2d pos :axis-x (cross* axis-x axis-y) :axis-y axis-y :axis-z (cross* axis-z axis-y)}
                          [0 size] {:pos-2d pos :axis-x axis-x :axis-y (cross* axis-x axis-y) :axis-z (cross* axis-x axis-z)}
                          [(- size) 0] {:pos-2d pos :axis-x (cross* axis-y axis-x) :axis-y axis-y :axis-z (cross* axis-y axis-z)}))
                      neighbors))
           (conj closed pos-2d)
           (into cube area)))))))

(defn turn-cube [direction axis-z ins]
  (case ins
    :left (cross* direction axis-z)
    :right (cross* axis-z direction)))

(defn move-cube [m pos dir]
  (let [[x y] (mapv + pos (delta dir))
        [x y] (if (contains? m [x y]) [x y]
                  (case dir
                    :up [x (reduce (fn [acc [x2 y2]] (if (= x x2) (max acc y2) acc)) y (keys m))]
                    :right [(reduce (fn [acc [x2 y2]] (if (= y y2) (min acc x2) acc)) x (keys m)) y]
                    :down [x (reduce (fn [acc [x2 y2]] (if (= x x2) (min acc y2) acc)) y (keys m))]
                    :left [(reduce (fn [acc [x2 y2]] (if (= y y2) (max acc x2) acc)) x (keys m)) y]))]
    (case (m [x y])
      :floor [x y]
      :obstacle pos)))

(defn play-cube [setup]
  (let [{:keys [cube m instructions position direction]} setup]
    (loop [[ins & instructions] instructions position position direction direction]
      (let [{:keys [pos axis-x axis-y axis-z]} (cube position)]
        (if (nil? ins)
          [pos (condp = direction
                 (vec* axis-x 2) :right
                 (vec* axis-y 2) :down
                 (vec* axis-x -2) :left
                 (vec* axis-y -2) :up)]
          (case ins
            (:right :left) (recur instructions position (turn-cube direction axis-z ins))
            (let [[position direction]
                  (reduce (fn [[position direction] _]
                            (let [next (vec+ position direction)]
                              (if-let [{:keys [pos]} (cube next)]
                                (if (= (m pos) :floor) [next direction] [position direction])
                                (let [wrap-direction (vec* (get-in cube [position :axis-z]) 2)
                                      wrap-position (vec+ wrap-direction next)
                                      {:keys [pos]} (cube wrap-position)]
                                  (if (= (m pos) :floor) [wrap-position wrap-direction] [position direction])))))
                          [position direction] (range 0 ins))]
              (recur instructions position direction))))))))

(defn solve-2 [s]
  (let [[[x y] dir] (play-cube (parse-cube s))]
    (+ (* 1000 y) (* 4 x) (.indexOf facing-values dir))))
