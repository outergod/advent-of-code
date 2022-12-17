(ns outergod.day-16
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as set]))

(def input (slurp (io/resource "day-16")))
(def example (slurp (io/resource "day-16-example")))

(defn parse-line [line]
  (let [[_ valve flow valves] (re-matches #"Valve (\w{2}) has flow rate=(\d+); tunnels? leads? to valves? (.+)" line)]
    {:valve valve :flow (Integer/parseInt flow) :valves (s/split valves #", ")}))

(defn parse [input]
  (->> input s/split-lines
       (map parse-line)
       (reduce (fn [acc v] (assoc acc (:valve v) v)) {})))

(defn distances
  ([g] (reduce (fn [g k] (assoc-in g [k :distances] (distances g k))) g (keys g)))
  ([g valve]
   (let [valves (keys g)]
     (loop [closed {valve 0} distance 1 current (get-in g [valve :valves])]
       (if (= (count closed) (count valves))
         closed
         (let [closed (reduce (fn [closed valve] (assoc closed valve distance)) closed current)]
           (recur
            closed
            (inc distance)
            (->> current (mapcat #(get-in g [% :valves])) (remove closed)))))))))

(defn fitness [g root time valves]
  (loop [time (inc time) current root valves valves closed #{} distance 0 flow 0]
    (if (nil? current)
      flow
      (let [time (- time (inc distance))
            [next & vs] valves]
        (if (<= time 0)
          flow
          (recur time next vs (conj closed current)
                (get-in g [current :distances next])
                (if (closed current)
                  flow
                  (+ flow (* time (get-in g [current :flow]))))))))))

(defn perm [g v0 vs d max]
  (if (seq (rest vs))
    (apply concat (map (fn [v1]
                         (let [d (+ d (get-in g [v0 :distances v1]) 1)]
                           (if (>= d max)
                             [nil]
                             (map (partial cons v1) (perm g v1 (remove #{v1} vs) d max)))))
                       vs))
    [vs]))

(defn flow-valves [g] (keys (filter (fn [[_ v]] (pos? (v :flow))) g)))

(defn optimize-valves [g v0 time]
  (let [vs (apply concat (take-while seq (iterate
                                          (fn [vs] (dedupe (filter seq (map butlast vs))))
                                          (perm g v0 (flow-valves g) 0 time))))]
    (reduce
     (fn [acc [vset f vs]] (update acc vset (fn [old new] (if (> (first new) (or (first old) 0)) new old)) [f vs]))
     {}
     (pmap (fn [vs] [(set vs) (fitness g v0 time vs) vs]) vs))))

(defn find-max-pair [g time]
  (let [valves (optimize-valves g "AA" time)]
    (loop [[left & vs] valves max [0 nil nil]]
      (if (nil? left)
        max
        (let [[lset [lflow lvs]] left
              rs (filter (fn [[rset & _]] (empty? (set/intersection lset rset))) valves)]
          (recur
           vs
           (reduce (fn [max [rset [rflow rvs]]]
                     (let [sum (+ lflow rflow)]
                       (if (> sum (first max)) [sum lvs rvs] max)))
                   max rs)))))))

(defn solve-1 [input]
  (->> input parse distances (solve 30)))

(defn solve-2 [input]
  (first (find-max-pair (->> input parse distances) 26)))

(comment
  (defn chromosome-size [valves]
    (int (Math/pow 2 (Math/ceil (/ (Math/log (count valves)) (Math/log 2))))))

  (defn gen-chromosome [g]
    (repeatedly (count g) (fn [] (rand-nth (keys g)))))

  (defn splice-chromosomes [left right]
    (let [i (inc (rand-int (dec (count left))))]
      (concat (take i left) (drop i right))))

  (defn mutate-chromosome [valves cx]
    (let [p (/ 1 (count valves))]
      (map (fn [x] (if (< (rand) p) (rand-nth valves) x)) cx)))

  (defn evolve [g root]
    (let [sample (repeatedly 1000 (fn [] (gen-chromosome g)))
          f (partial fitness g root)
          mutate (partial mutate-chromosome (keys g))]
      (loop [i 0 population sample best 0]
        (let [fs (map f population)
              total (reduce +' fs)
              ps (reduce (fn [acc f] (conj acc (+ (or (peek acc) 0) (/ f total)))) [] fs)
              select (fn [p] (nth population (count (take-while #(< % p) ps))))]
          (if (< i 10000)
            (let [population (pmap (fn [_] (mutate (splice-chromosomes (select (rand)) (select (rand))))) (range 0 1000))]
              (recur (inc i) population (reduce max fs)))
            best))))))
