(ns outergod.day-9
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input (slurp (io/resource "day-9")))

(defn move [[x y] motion]
  (case motion
    :up [x (inc y)]
    :right [(inc x) y]
    :down [x (dec y)]
    :left [(dec x) y]))

(defn distant? [[x1 y1] [x2 y2]]
  (or (> (abs (- x1 x2)) 1) (> (abs (- y1 y2)) 1)))

(defn offset-direction [[x1 y1] [x2 y2]]
  (mapv (fn [d]
          (if (zero? d)
            0
            (/ d (abs d))))
        [(- x2 x1) (- y2 y1)]))

(defn parse-instruction [s]
  (let [[_ direction amount] (re-matches #"(\w) (\d+)" s)]
    (repeat (Integer/parseInt amount) (case direction "U" :up "R" :right "D" :down "L" :left))))

(defn parse [input]
  (mapcat parse-instruction (s/split-lines input)))

(defn iterate [{:keys [head tail visited path]} position]
  (if (distant? tail position)
    (let [tail (mapv + tail (offset-direction tail position))]
      {:head position :tail tail :visited (conj visited tail) :path (conj path tail)})
    {:head position :tail tail :visited visited :path path}))

(def init {:head [0 0] :tail [0 0] :visited #{[0 0]} :path [[0 0]]})

(defn solve-1 [input]
  (->> input parse
       (reduce (fn [acc step] (conj acc (move (peek acc) step))) [(:head init)])
       (reduce iterate init)
       :visited count))

(defn solve-2 [input]
  (let [seq (->> input parse
                 (reduce (fn [acc step] (conj acc (move (peek acc) step))) [(:head init)])
                 (reduce iterate init)
                 (clojure.core/iterate (fn [state] (reduce iterate init (:path state)))))]
    (-> seq (nth 8) :visited count)))
