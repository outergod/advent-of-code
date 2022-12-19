(ns outergod.day-17
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as set]))

(def input (slurp (io/resource "day-17")))
(def example (slurp (io/resource "day-17-example")))

(def --shape "####")
(def +-shape "·#·
###
·#·")
(def l-shape "··#
··#
###")
(def |-shape "#
#
#
#")
(def o-shape "##
##")

(defn load-shape [s]
  (let [tiles
        (apply concat
               (map-indexed
                (fn [y line]
                  (filter identity (map-indexed (fn [x c] (when (= c \#) [x y])) line)))
                (reverse (s/split-lines s))))]
    {:tiles tiles
     :width (inc (reduce max (map first tiles)))
     :height (inc (reduce max (map second tiles))) }))

(def spawn-x 2)
(def space 7)

(def rocks (cycle (map load-shape [--shape +-shape l-shape |-shape o-shape])))

(defn input-seq [s]
  (cycle (concat (interpose :down (map #(case % \< :left \> :right) (s/trim-newline s))) [:down])))

(def input-diff {:left [-1 0] :right [1 0] :down [0 -1]})

(defn render-state [tiles blocked]
  (let [top (reduce max (map second tiles))]
    (s/join
     \newline
     (map
      (fn [y]
        (s/join
         (map
          (fn [x]
            (cond
              (and (zero? y) (#{-1 space} x)) \+
              (zero? y) \-
              (#{-1 space} x) \|
              (tiles [x y]) \@
              (blocked [x y]) \#
              :default \·))
          (range -1 (inc space)))))
      (range top -1 -1)))))

(defn solve [input]
  (letfn [(solve-seq [i stopped level pos blocked rock [input & inputs]]
            (let [{:keys [tiles width height]} rock
                  tiles (map (partial mapv + pos) tiles)
                  diff (input-diff input)
                  pos' (mapv + pos diff)
                  tiles' (map (partial mapv + diff) tiles)]
              (if (every? (fn [[x y]] (and (>= x 0) (< x space) (pos? y) (nil? (blocked [x y])))) tiles')
                (recur (inc i) stopped level pos' blocked rock inputs)
                (if (= input :down)
                  (let [blocked (apply conj blocked tiles)
                        level (reduce max (map second blocked))
                        stopped (inc stopped)
                        rock (nth rocks stopped)]
                    (cons level (lazy-seq (solve-seq (inc i) stopped level [spawn-x (+ level 4)] blocked rock inputs))))
                  (recur (inc i) stopped level pos blocked rock inputs)))))]
    (cons 0 (lazy-seq (solve-seq 0 0 0 [spawn-x 4] #{} (first rocks) (input-seq input))))))

(defn solve-cached [s n]
  (let [len (* 2 (count (s/trim-newline s)))]
    (loop [i 0 stopped 0 level 0 pos [spawn-x 4] blocked #{} rock (first rocks) [input & inputs] (input-seq s) cache {}]
      (if (>= stopped n)
        level
        (let [pattern (set (keep (fn [[x y]] (let [y (- level y)] (when (<= y 20) [x y]))) blocked))]
          (if-let [[i' stopped' level'] (cache [rock (mod i len) pattern])]
            (let [remain (- n stopped)
                  stopped' (- stopped stopped')
                  level' (- level level')
                  m (long (/ remain stopped'))]
              (+ (* m level') (- (nth (solve s) (+ stopped' (- n (* m stopped')))) level')))
            (let [{:keys [tiles width height]} rock
                  tiles (map (partial mapv + pos) tiles)
                  diff (input-diff input)
                  pos' (mapv + pos diff)
                  tiles' (map (partial mapv + diff) tiles)
                  cache (assoc cache [rock (mod i len) pattern] [i stopped level])]
              (if (every? (fn [[x y]] (and (>= x 0) (< x space) (pos? y) (nil? (blocked [x y])))) tiles')
                (recur (inc i) stopped level pos' blocked rock inputs cache)
                (if (= input :down)
                  (let [blocked (apply conj blocked tiles)
                        level (reduce max (map second blocked))
                        stopped (inc stopped)
                        rock (nth rocks stopped)]
                    (recur (inc i) stopped level [spawn-x (+ level 4)] blocked rock inputs cache))
                  (recur (inc i) stopped level pos blocked rock inputs cache))))))))))

(defn solve-1 [input]
  (solve input 2022))

(defn solve-2 [input]
  (solve-cached input 1000000000000))
