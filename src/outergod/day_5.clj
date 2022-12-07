(ns outergod.day-5
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :use [intersection subset? superset?]]))

(def input (slurp (io/resource "day-5")))

(defn solve-1 [input]
  (let [[setup _ code] (->> input s/split-lines (partition-by (partial = "")))
        setup (->> setup
                   (map (fn [line]
                          (map #(nth (->> % (apply str) (re-matches #"\[(\w)\] ?")) 1)
                               (partition-all 4 line))))
                   reverse (drop 1))
        setup (->> setup
                   (apply interleave)
                   (partition (count setup))
                   (map (partial take-while identity))
                   (map reverse)
                   vec)]
    (->> (reduce (fn [setup instruction]
                   (let [[n x y] (->> instruction
                                      (re-matches #"move (\d+) from (\d) to (\d)")
                                      (drop 1)
                                      (map #(Integer/parseInt %)))
                         [crates from] (split-at n (nth setup (dec x)))]
                     (-> setup
                         (assoc (dec x) from)
                         (update (dec y) (partial concat (reverse crates))))))
                 setup code)
         (map first)
         s/join)))

(defn solve-2 [input]
  (let [[setup _ code] (->> input s/split-lines (partition-by (partial = "")))
        setup (->> setup
                   (map (fn [line]
                          (map #(nth (->> % (apply str) (re-matches #"\[(\w)\] ?")) 1)
                               (partition-all 4 line))))
                   reverse (drop 1))
        setup (->> setup
                   (apply interleave)
                   (partition (count setup))
                   (map (partial take-while identity))
                   (map reverse)
                   vec)]
    (->> (reduce (fn [setup instruction]
                   (let [[n x y] (->> instruction
                                      (re-matches #"move (\d+) from (\d) to (\d)")
                                      (drop 1)
                                      (map #(Integer/parseInt %)))
                         [crates from] (split-at n (nth setup (dec x)))]
                     (-> setup
                         (assoc (dec x) from)
                         (update (dec y) (partial concat crates)))))
                 setup code)
         (map first)
         s/join)))
