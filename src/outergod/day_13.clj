(ns outergod.day-13
  (:require [clojure.java.io :as io]
            [clojure.string :as s])
  (:import java.lang.Long
           clojure.lang.PersistentVector))

(def input (slurp (io/resource "day-13")))
(def example (slurp (io/resource "day-13-example")))

(defn parse-1 [input]
  (->> input s/split-lines
       (partition-by (partial = "")) (take-nth 2)
       (map (partial map read-string))))

(defn parse-2 [input]
  (->> input s/split-lines
       (remove (partial = ""))
       (map read-string)))

(defn padded [left right]
  (let [len (max (count left) (count right))]
    [(concat left (repeat (- len (count left)) nil))
     (concat right (repeat (- len (count right)) nil))]))

(defn check [left right]
  (condp = [(type left) (type right)]
    [java.lang.Long java.lang.Long]
    (cond (< left right) -1
          (> left right) 1
          :default 0)

    [Long PersistentVector]
    (check [left] right)

    [PersistentVector Long]
    (check left [right])

    [nil Long] -1
    [nil PersistentVector] -1
    [Long nil] 1
    [PersistentVector nil] 1

    [PersistentVector PersistentVector]
    (or
     (first (drop-while zero? (apply map check (padded left right))))
     0)))

(defn solve-1 [input]
  (->> input parse-1
       (map (partial apply check))
       (keep-indexed (fn [i r] (when (neg? r) (inc i))))
       (reduce +)))

(defn solve-2 [input]
  (let [packets (->> input parse-2
                     (concat [[[2]] [[6]]])
                     (sort check))]
    (* (inc (.indexOf packets [[2]])) (inc (.indexOf packets [[6]])))))
