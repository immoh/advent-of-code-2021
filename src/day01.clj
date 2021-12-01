(ns day01
  (:require
    clojure.string))

(defn parse-input [input]
  (map parse-long (clojure.string/split-lines input)))

(defn increasing? [[x y]]
  (< x y))

(defn part1 [input]
  (->> (parse-input input)
       (partition 2 1)
       (filter increasing?)
       (count)))

(defn part2 [input]
  (->> (parse-input input)
       (partition 3 1)
       (map (partial reduce +))
       (partition 2 1)
       (filter increasing?)
       (count)))
