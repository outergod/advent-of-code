(ns outergod.day-10
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input (slurp (io/resource "day-10")))

(defn parse-instruction [s]
  (let [[full instruction value] (re-matches #"(\w+) ?(-?\d+)?" s)]
    (case instruction
      "addx" {:instruction :addx :value (Integer/parseInt value)}
      "noop" {:instruction :noop})))

(defn step [[cycle register addx instructions]]
  (cond
    addx [(inc cycle) (+ register addx) nil instructions]
    instructions (let [[instruction & instructions] instructions
                       {:keys [instruction value]} (parse-instruction instruction)]
                   (case instruction
                     :addx [(inc cycle) register value instructions]
                     :noop [(inc cycle) register nil instructions]))
    :default nil))

(defn input-seq [input]
  (iterate step [1 1 nil (s/split-lines input)]))

(defn solve-1 [input]
  (let [seq (iterate step [1 1 nil (s/split-lines input)])
        cycles [20 60 100 140 180 220]]
    (reduce + (map #(let [[cycle register & _] (nth seq (dec %))]
                      (* cycle register))
                   cycles))))

(defn solve-2 [input]
  (let [seq (iterate step [1 1 nil (s/split-lines input)])]
    (println
     (s/join \newline
             (map (partial apply str)
                  (partition 40
                             (map (fn [[cycle register & _]]
                                    (let [pos (mod (dec cycle) 40)]
                                     (if (< (abs (- register pos)) 2) \# \.)))
                                  (take-while some? seq))))))))
