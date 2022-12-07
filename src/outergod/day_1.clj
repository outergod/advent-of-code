(ns outergod.day-1
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :use [intersection subset? superset?]]))

(def input (slurp (io/resource "day-1")))

(defn solve-1 [input]
  (apply max
         (map (comp (partial reduce +) (partial map #(Integer/parseInt %)))
              (take-nth 2 (partition-by (partial = "") (s/split-lines input))))))

(defn solve-2 [input]
  (apply + (take 3 (sort > (map (comp (partial reduce +) (partial map #(Integer/parseInt %)))
                                (take-nth 2 (partition-by (partial = "") (s/split-lines input))))))))
