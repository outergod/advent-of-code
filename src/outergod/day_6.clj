(ns outergod.day-6
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :use [intersection subset? superset?]]))

(def input (slurp (io/resource "day-6")))

(defn solve [input]
  (->> input
       (iterate (partial drop 1))
       (take-while #(>= (count %) 4))
       (map #(->> % (take 4) sort dedupe count))
       (take-while #(< % 4))
       count))

(defn solve-1 [input]
  (->> input solve (+ 4)))

(defn solve-2 [input]
  (->> input solve (+ 14)))
