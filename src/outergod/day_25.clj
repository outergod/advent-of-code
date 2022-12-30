(ns outergod.day-25
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as set]))

(def input (slurp (io/resource "day-25")))
(def example (slurp (io/resource "day-25-example")))

(defn snafu-digit-value [c]
  (case c
    \2 2
    \1 1
    \0 0
    \- -1
    \= -2))

(defn parse-snafu [s]
  (long (reduce + (map (fn [c i] (*' (snafu-digit-value c) (Math/pow 5 i))) s (range (dec (count s)) -1 -1)))))

(defn parse [s]
  (->> s s/split-lines (map parse-snafu)))

(def snafu-powers (map #(bigint (Math/pow 5 %)) (iterate inc 0)))

(defn integer-snafu [i]
  (loop [i i cs [] [p & ps] (reverse (take-while #(>= (/ i %) 0.5) snafu-powers))]
    (if (nil? p) (s/join cs)
        (let [x (min 2 (bigint (+ (/ (abs i) p) 0.5)))
              y (* x p)]
          (cond
            (pos? i) (recur (- i y) (conj cs (case x 0 \0 1 \1 2 \2)) ps)
            (neg? i) (recur (+ i y) (conj cs (case x 0 \0 1 \- 2 \=)) ps)
            (zero? i) (recur i (conj cs \0) ps))))))

(defn solve-1 [s]
  (integer-snafu (reduce + (parse s))))
