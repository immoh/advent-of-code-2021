(ns day06
  (:require
    clojure.string))

(defn parse-input [input]
  (map parse-long (clojure.string/split input #",")))

(defn next-day [population]
  (reduce (partial merge-with +)
          (map (fn [[age n]]
                 (if (zero? age)
                   {6 n 8 n}
                   {(dec age) n}))
               population)))

(defn total-count [population]
  (reduce + (vals population)))

(defn part1 [input]
  (-> (iterate next-day (frequencies (parse-input input)))
      (nth 80)
      (total-count)))

(defn part2 [input]
  (-> (iterate next-day (frequencies (parse-input input)))
      (nth 256)
      (total-count)))
