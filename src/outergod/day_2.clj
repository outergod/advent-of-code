(ns outergod.day-2
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :use [intersection subset? superset?]]))

(def input (slurp (io/resource "day-2")))

(defn letter-shape [c]
  (case c
    ("A" "X") :rock
    ("B" "Y") :paper
    ("C" "Z") :scissors))

(defn letter-result [c]
  (case c
    "X" :lose
    "Y" :draw
    "Z" :win))

(defn game-result [x y]
  (case [y x]
    [:rock :scissors] :win
    [:scissors :rock] :lose
    [:scissors :paper] :win
    [:paper :scissors] :lose
    [:paper :rock] :win
    [:rock :paper] :lose
    :draw))

(defn result-score [result]
  (case result
    :win 6
    :lose 0
    :draw 3))

(defn shape-score [shape]
  (case shape
    :rock 1
    :paper 2
    :scissors 3))

(defn result-shape [shape result]
  (case [shape result]
    [:rock :lose] :scissors
    [:rock :win] :paper
    [:paper :lose] :rock
    [:paper :win] :scissors
    [:scissors :lose] :paper
    [:scissors :win] :rock
    shape))

(defn solve-1 [input]
  (reduce + (map (fn [c]
                   (let [[x y] (map letter-shape c)]
                     (+ (shape-score y) (result-score (game-result x y)))))
                 (map #(s/split % #" ") (s/split-lines input)))))

(defn solve-2 []
  (reduce + (map (fn [[x y]]
                   (let [x (letter-shape x)
                         result (letter-result y)
                         y (result-shape x result)]
                     (+ (shape-score y) (result-score result))))
                 (map #(s/split % #" ") (s/split-lines input)))))
