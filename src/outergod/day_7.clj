(ns outergod.day-7
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input (slurp (io/resource "day-7")))

(def total-space 70000000)
(def required-space 30000000)

(defn parse-cd [input state arg]
  {:status :accept :input input
   :state (case arg
            "/" (assoc state :cwd [])
            ".." (update state :cwd pop)
            (update state :cwd conj arg))})

(defn parse-command [input state]
  (let [[line & input] input
        [match command arg] (re-matches #"\$ (\w+) ?(.+)?" line)]
    (if match
      (case command
        "cd" (parse-cd input state arg)
        "ls" (parse-ls input state)
        {:status :reject :input input :state state})
      {:status :reject :input input :state state})))

(defn parse-ls [input state]
  (let [[output input] (split-with (partial re-find #"^(?!\$)") input)]
    (reduce (fn [result line]
              (let [[base name] (s/split line #" ")]
                (update-in result (concat [:state :tree] (interpose :children (:cwd state)) [:children])
                           assoc name (if (= base "dir")
                                        {:type :dir :children {}}
                                        {:type :file :size (Integer/parseInt base)}))))
            {:status :accept :input input :state state} output)))

(defn traverse [path node]
  (if (= (:type node) :file)
    {:size (:size node) :files {}}
    (let [r (reduce (fn [{:keys [size files]} [name node]]
                      (let [r (traverse (conj path name) node)]
                        {:size (+ size (:size r)) :files (merge files (:files r))}))
                    {:size 0 :files {}}
                    (:children node))]
      {:size (:size r) :files (assoc (:files r) (s/join "/" path) (:size r))})))

(defn solve [input]
  (let [{:keys [state]}
        (loop [{:keys [status input state] :as result}
               {:status :accept :input (s/split-lines input) :state {:cwd [] :tree {}}}]
          (if (and input (not (s/blank? (first input))) (= status :accept))
            (recur (parse-command input state))
            result))]
    (traverse [] {:type :dir :children (:tree state)})))

(defn solve-1 [input]
  (->> input solve :files vals (filter (partial >= 100000)) (reduce +)))

(defn solve-2 [input]
  (let [files (->> input solve :files)
        used-space (files "")
        free-space (- total-space used-space)
        to-free (- required-space free-space)]
    (->> files vals sort (filter (partial < to-free)) first)))
