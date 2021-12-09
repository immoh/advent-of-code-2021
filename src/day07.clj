(ns day07
  (:require
    clojure.java.math
    clojure.string))

(defn parse-input [input]
  (map parse-long (clojure.string/split input #",")))

(defn median-choices [xs]
  (let [n (/ (inc (count xs)) 2)
        sorted-xs (sort xs)]
    (set [(nth sorted-xs (dec (int (clojure.java.math/floor n))))
          (nth sorted-xs (dec (int (clojure.java.math/ceil n))))])))

(defn distance [x y]
  (clojure.java.math/abs (- x y)))

(defn total-fuel-consumption [fuel-consumption-fn positions alignment]
  (reduce + (map (partial fuel-consumption-fn alignment) positions)))

(defn part1 [input]
  (let [positions (parse-input input)]
    (reduce min (map (partial total-fuel-consumption distance positions) (median-choices positions)))))

(defn all-choices [xs]
  (range (apply min xs) (inc (apply max xs))))

(defn arithmetic-sequence-sum [x y]
  (let [n (clojure.java.math/abs (- x y))]
    (/ (* n (inc n)) 2)))

(defn part2 [input]
  (let [positions (parse-input input)]
    (reduce min (map (partial total-fuel-consumption arithmetic-sequence-sum positions) (all-choices positions)))))
