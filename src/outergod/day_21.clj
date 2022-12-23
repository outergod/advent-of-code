(ns outergod.day-21
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as set]
            [clojure.zip :as zip]))

(def input (slurp (io/resource "day-21")))
(def example (slurp (io/resource "day-21-example")))

(defn parse [s]
  (->> s s/split-lines
       (reduce
        (fn [r s]
          (let [[_ k v] (re-matches #"(\w+): (.+)" s)
                [form? v1 op v2] (re-matches #"(\w+) ([+\-\*/]) (\w+)" v)
                [num? n] (re-matches #"(-?\d+)" v)]
            (-> r
                (assoc-in [:tree k]
                       (if form?
                         (let [op (case op "+" +' "-" -' "*" *' "/" /)]
                           (eval `(fn [~'r] (~op ((~'r ~v1) ~'r) ((~'r ~v2) ~'r)))))
                         (constantly (Long/parseLong n))))
                (assoc-in [:deps k] (if form? [v1 v2] [])))))
        {:tree {} :deps {}})))

(defn deps-zipper [r root]
  (let [deps (:deps r)]
    (zip/zipper seq (partial map deps) nil [root])))

(defn humn-parent [deps]
  (let [zipper (zip/zipper seq (partial map deps) nil ["root"])
        [[left right]] (zip/children zipper)
        left-nodes (mapcat zip/node (take-while #(not (zip/end? %)) (iterate zip/next (zip/down zipper))))]
    (if (some #{"humn"} left-nodes) left right)))

(defn solve [tree root] ((tree root) tree))

(defn solve-1 [s] (solve (:tree (parse s)) "root"))

(defn gen-chromosome [] (vec (Long/toString (long (rand Long/MAX_VALUE)) 2)))

(defn splice-chromosomes [left right]
  (let [len (max (count left) (count right))
        [left right] (map #(vec (concat (s/join (repeat (- len (count %)) \0)) %)) [left right])
        i (rand-int len)]
    (into (subvec left 0 i) (subvec right i))))

(defn mutate-chromosome [cx]
  (let [p 0.05]
    (mapv (fn [x] (if (< (rand) p) (if (= \0 x) \1 \0) x)) cx)))

(defn chromosome-value [cx]
  (Long/parseLong (s/join cx) 2))

(defn solve-2 [s]
  (let [{:keys [tree deps] :as r} (parse s)
        search (humn-parent deps)
        x (->> (deps "root") (remove #{search}) first (solve tree))
        sample (vec (repeatedly 100 gen-chromosome))
        f #(let [y (solve (assoc tree "humn" (constantly %)) search)
                 d (abs (- x y))]
             (if (zero? d) 1N (/ 1N d)))]
    (println "searching" x)
    (loop [i 0 population sample]
      (let [fitness (mapv (comp f chromosome-value) population)
            [i best] (first (sort-by second > (map vector (range 0 (count fitness)) fitness)))]
        (println "iteration" i "best" best "value" (nth population i))
        (if-let [y (first (keep-indexed (fn [i f] (when (= 1N f) (nth population i))) fitness))]
          (chromosome-value y)
          (let [total (reduce +' fitness)
                ps (reduce (fn [acc f] (conj acc (+ (or (peek acc) 0N) (/ f total)))) [] fitness)
                select (fn [p] (nth population (count (take-while #(< % p) ps))))
                population (pmap (fn [_] (mutate-chromosome (splice-chromosomes (select (rand)) (select (rand))))) (range 0 100))]
            (recur (inc i) population)))))))
