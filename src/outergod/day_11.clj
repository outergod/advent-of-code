(ns outergod.day-11
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input (slurp (io/resource "day-11")))
(def example (slurp (io/resource "day-11-example")))

(defn parse-starting-items [lines]
  (let [[line & lines] lines
        [_ items] (re-matches #"  Starting items: (.+)" line)]
    [{:items (mapv #(Integer/parseInt %) (s/split items #", "))} lines]))

(defn parse-operation [lines]
  (let [[line & lines] lines
        [_ x op y] (re-matches #"  Operation: new = (\w+) ([+*]) (\w+)" line)
        op (case op "+" '+ "*" '*)
        [x y] (map #(if (= % "old") 'old (Integer/parseInt %)) [x y])]
    [{:operation (eval `(fn [~'old] ~(list op x y)))} lines]))

(defn parse-test [lines]
  (let [[test if-true if-false & lines] lines
        [_ divisor] (re-matches #"  Test: divisible by (\d+)" test)
        [_ if-true] (re-matches #"    If true: throw to monkey (\d+)" if-true)
        [_ if-false] (re-matches #"    If false: throw to monkey (\d+)" if-false)]
    [{:test {:divisor (Integer/parseInt divisor) :if-true (Integer/parseInt if-true) :if-false (Integer/parseInt if-false)}} lines]))

(defn parse-monkey [lines]
  (let [[line & lines] lines
        [_ index] (re-matches #"Monkey (\d+):" line)]
    (reduce (fn [[monkey lines] f]
              (let [[data lines] (f lines)]
                [(merge monkey data) lines]))
            [{:index (Integer/parseInt index)} lines]
            [parse-starting-items parse-operation parse-test])))

(defn parse [input]
  (->> input s/split-lines
       (partition-by (partial = ""))
       (take-nth 2)
       (mapv (comp first parse-monkey))))

(defn monkeys-swap [monkeys]
  (->> monkeys (map (comp :divisor :test)) (reduce *)))

(defn turn [relief? swap monkeys index]
  (let [reliefn (if relief? #(int (/ % 3)) identity)
        monkey (nth monkeys index)
        {:keys [items operation test inspections]} monkey
        {:keys [divisor if-true if-false]} test
        monkeys (-> monkeys
                    (assoc-in [index :items] [])
                    (update-in [index :inspections] + (count items)))]
    (reduce (fn [monkeys item]
              (let [item (-> item operation reliefn)
                    target (if (zero? (mod item divisor)) if-true if-false)]
                (update-in monkeys [target :items] conj (mod item swap))))
            monkeys items)))

(defn round [relief? swap monkeys]
  (reduce (partial turn relief? swap) monkeys (range 0 (count monkeys))))

(defn monkey-business [monkeys]
  (->> monkeys (map :inspections) (sort >) (take 2) (reduce *)))

(defn solve [input index relief?]
  (let [monkeys (mapv #(assoc % :inspections 0) (parse input))
        swap (monkeys-swap monkeys)]
    (monkey-business
     (nth (iterate (partial round relief? swap) monkeys)
          index))))

(defn solve-1 [input] (solve input 20 true))
(defn solve-2 [input] (solve input 10000 false))
