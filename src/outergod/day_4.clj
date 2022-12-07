(ns outergod.day-4
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :use [intersection subset? superset?]]))

(def input (slurp (io/resource "day-4")))

(defn solve-1 [input]
  (count
   (filter identity
           (map (fn [line]
                  (let [[a1 b1 a2 b2] (->> line (re-matches #"(\d+)-(\d+),(\d+)-(\d+)") (drop 1) (map #(Integer/parseInt %)))
                        range1 (set (range a1 (inc b1)))
                        range2 (set (range a2 (inc b2)))]
                    (or (superset? range1 range2) (subset? range1 range2))))
                (s/split-lines input)))))

(defn solve-2 [input]
  (count
   (filter identity
           (map (fn [line]
                  (let [[a1 b1 a2 b2] (->> line (re-matches #"(\d+)-(\d+),(\d+)-(\d+)") (drop 1) (map #(Integer/parseInt %)))
                        range1 (set (range a1 (inc b1)))
                        range2 (set (range a2 (inc b2)))]
                    (not (empty? (intersection range1 range2)))))
                (s/split-lines input)))))
