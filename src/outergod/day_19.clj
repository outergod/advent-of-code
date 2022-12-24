(ns outergod.day-19
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as set]))

(def input (slurp (io/resource "day-19")))
(def example (slurp (io/resource "day-19-example")))
(def example-inputs [:clay :clay :clay :obsidian :clay :obsidian :geode :geode])

(defn parse-blueprint [s]
  (let [[index ore-ore clay-ore obsidian-ore obsidian-clay geode-ore geode-obsidian]
        (map #(Integer/parseInt %) (re-seq #"\d+" s))]
    {:index index
     :costs {:ore {:ore ore-ore}
             :clay {:ore clay-ore}
             :obsidian {:ore obsidian-ore :clay obsidian-clay}
             :geode {:ore geode-ore :obsidian geode-obsidian}}}))

(defn parse [input]
  (->> input s/split-lines (map parse-blueprint)))

(def bot-types [:ore :clay :obsidian :geode])

(defn dfs [costs depth]
  (let [max-costs (reduce (partial merge-with max) {:ore 0 :clay 0 :obsidian 0} (vals costs))
        time-cost (fn [r cost bots]
                    (cond
                      (>= r cost) 1
                      (zero? bots) (inc depth)
                      :default (int (inc (Math/ceil (/ (- cost r) bots))))))]
    (letfn [(r [n path best resources bots]
              (let [{:keys [geode]} resources
                    time-left (- depth n)
                    bot-types (remove (fn [bot] (case bot
                                                  :ore (>= (bots :ore) (max-costs :ore))
                                                  :clay (>= (bots :clay) (max-costs :clay))
                                                  :obsidian (>= (bots :obsidian) (max-costs :obsidian))
                                                  false))
                                      bot-types)]
                (cond (= n depth)
                      (if (> geode (first best)) [geode path] best)

                      (> (first best) (+ geode (* (bots :geode) time-left) (reduce + (range 1 time-left))))
                      best

                      :default (reduce
                                (fn [best bot]
                                  (if (= bot :wait)
                                    (let [time (- depth n)]
                                      (r depth path best
                                         (-> resources
                                             (update :ore + (* (bots :ore) time))
                                             (update :clay + (* (bots :clay) time))
                                             (update :obsidian + (* (bots :obsidian) time))
                                             (update :geode + (* (bots :geode) time)))
                                         bots))
                                    (let [bp (costs bot)
                                          time (reduce max (map #(time-cost (resources %) (or (bp %) 0) (bots %)) [:ore :clay :obsidian]))
                                          n (+ n time)]
                                      (if (> n depth)
                                        best
                                        (r n (conj path bot) best
                                           (-> resources
                                               (update :ore + (- (* (bots :ore) time) (or (bp :ore) 0)))
                                               (update :clay + (- (* (bots :clay) time) (or (bp :clay) 0)))
                                               (update :obsidian + (- (* (bots :obsidian) time) (or (bp :obsidian) 0)))
                                               (update :geode + (* (bots :geode) time)))
                                           (update bots bot inc))))))
                                best (conj bot-types :wait)))))]
      (r 1 [] [0 nil] {:ore 1 :clay 0 :obsidian 0 :geode 0} {:ore 1 :clay 0 :obsidian 0 :geode 0}))))

(defn quality [blueprint n]
  (let [{:keys [index costs]} blueprint
        [geodes path] (dfs costs n)]
    (* index geodes)))

(defn solve-1 [input]
  (reduce (fn [acc blueprint] (+ acc (quality blueprint 24))) 0 (parse input)))

(defn solve-2 [input]
  (reduce (fn [acc blueprint] (* acc (first (dfs (blueprint :costs) 32)))) 1 (take 3 (parse input))))

(comment
  (defn random-bot [] (rand-nth bot-types))

  ;; (def chromosome-size (int (Math/pow 2 (Math/ceil (/ (Math/log (count bot-types)) (Math/log 2))))))

  (defn gen-chromosome [n]
    (vec (repeatedly n random-bot)))

  (defn splice-chromosomes [left right]
    (let [i (inc (rand-int (dec (count left))))]
      (into (subvec left 0 i) (subvec right i))))

  (defn mutate-chromosome [cx]
    (let [p (/ 1 (count bot-types))]
      (mapv (fn [x] (if (< (rand) p) (random-bot) x)) cx)))

  (defn score [resources]
    (reduce + (map #(* %1 (long (Math/pow 256 %2))) resources (range (dec (count resources)) -1 -1))))

  (defn play [blueprint n cx]
    (loop [i 1 [build & cx] cx ore 1 clay 0 obsidian 0 geode 0 ore-bot 1 clay-bot 0 obsidian-bot 0 geode-bot 0]
      (if (>= i n)
        [geode obsidian clay ore]
        (let [{ore-cost :ore clay-cost :clay obsidian-cost :obsidian} (blueprint build)
              ore-cost (or ore-cost 0)
              clay-cost (or clay-cost 0)
              obsidian-cost (or obsidian-cost 0)
              ore' (+ ore ore-bot)
              clay' (+ clay clay-bot)
              obsidian' (+ obsidian obsidian-bot)
              geode (+ geode geode-bot)]
          (if (and (>= ore ore-cost) (>= clay clay-cost) (>= obsidian obsidian-cost))
            (recur (inc i) cx
                   (- ore' ore-cost)
                   (- clay' clay-cost)
                   (- obsidian' obsidian-cost)
                   geode
                   (if (= build :ore) (inc ore-bot) ore-bot)
                   (if (= build :clay) (inc clay-bot) clay-bot)
                   (if (= build :obsidian) (inc obsidian-bot) obsidian-bot)
                   (if (= build :geode) (inc geode-bot) geode-bot))
            (recur (inc i) (cons build cx) ore' clay' obsidian' geode ore-bot clay-bot obsidian-bot geode-bot))))))

  (defn solve [blueprint n]
    (let [sample (repeatedly 1000 (fn [] (gen-chromosome n)))
          play (memoize (partial play blueprint n))]
      (loop [i 0 population sample best [0 nil nil]]
        (if (>= i 100)
          best
          (let [rs (mapv play population)
                scores (mapv score rs)
                total (reduce +' scores)
                ps (reduce (fn [acc f] (conj acc (+ (or (peek acc) 0) (/ f total)))) [] scores)
                select (fn [p] (nth population (count (take-while #(< % p) ps))))
                best (reduce-kv (fn [acc k v] (if (> v (first acc)) [v (nth rs k) (nth population k)] acc)) best scores)]
            (println i best)
            (let [population (pmap (fn [_] (mutate-chromosome (splice-chromosomes (select (rand)) (select (rand))))) (range 0 1000))]
              (recur (inc i) population best))))))))
