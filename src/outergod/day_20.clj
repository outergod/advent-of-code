(ns outergod.day-20
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as set]))

(def input (slurp (io/resource "day-20")))
(def example (slurp (io/resource "day-20-example")))

(defn parse [s]
  (->> s s/split-lines (mapv #(Integer/parseInt %))))

(defn move [coll index]
  (let [n (count coll)
        a (nth coll index)
        x (mod (+ index a) (dec n))
        x (if (and (neg? a) (zero? x)) (dec n) x)]
    (cond
      (> x index)
      [(reduce into [(subvec coll 0 index)
                     (subvec coll (inc index) (inc x))
                     [a]
                     (subvec coll (inc x))])
       x (inc index) x -1]

      (< x index)
      [(reduce into [(subvec coll 0 x)
                     [a]
                     (subvec coll x index)
                     (subvec coll (inc index))])
       x x (dec index) 1]

      (= x index)
      [coll x x index 1])))

(defn move [coll index]
  (let [n (count coll)
        el (nth coll index)
        [_ a] el
        x (mod (+ index a) (dec n))
        x (if (and (neg? a) (zero? x)) (dec n) x)]
    (cond
      (> x index)
      (reduce into [(subvec coll 0 index)
                    (subvec coll (inc index) (inc x))
                    [el]
                    (subvec coll (inc x))])

      (< x index)
      (reduce into [(subvec coll 0 x)
                    [el]
                    (subvec coll x index)
                    (subvec coll (inc index))])

      (= x index) coll)))

(defn mix [coll]
  (let [n (count coll)]
    (loop [index 0 coll coll]
      (if (= index n)
        coll
        (let [[pos x] (some (fn [[pos [i x]]] (when (= i index) [pos x])) (map-indexed vector coll))]
          (let [coll (move coll pos)]
            (recur (inc index) coll)))))))

(defn solve [coll n]
  (let [coll (map second (nth (iterate mix (vec (map-indexed vector coll))) n))
        n (count coll)
        pos (.indexOf coll 0)]
    (reduce (fn [acc x] (+ acc (nth coll (mod (+ x pos) n)))) 0 [1000 2000 3000])))

(defn solve-1 [s]
  (solve (parse s) 1))

(defn solve-2 [s]
  (solve (mapv (partial * 811589153) (parse s)) 10))
