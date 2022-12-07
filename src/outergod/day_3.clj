(ns outergod.day-3
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :use [intersection subset? superset?]]))

(def input (slurp (io/resource "day-3")))

(def priorities
  (concat (map char (range (int \a) (inc (int \z))))
          (map char (range (int \A) (inc (int \Z))))))

(defn solve-1 [input]
  (reduce + (map (fn [line]
                   (inc (.indexOf priorities (first (apply intersection (map set (split-at (/ (count line) 2) line)))))))
                 (s/split-lines input))))

(defn solve-2 [input]
  (reduce + (map (fn [lines]
                   (inc (.indexOf priorities (first (apply intersection (map set lines))))))
                 (partition 3 (s/split-lines input)))))
