(ns day13
  (:require
    clojure.string))

(defn parse-input [input]
  (let [[dots folds] (clojure.string/split input #"\n\n")]
    {:dots  (set (map (fn [line]
                        (zipmap [:x :y] (map parse-long (clojure.string/split line #","))))
                      (clojure.string/split-lines dots)))
     :folds (map (fn [line]
                   (let [[s n] (clojure.string/split line #"=")]
                     [(keyword (str (last s))) (parse-long n)]))
                 (clojure.string/split-lines folds))}))


(defn fold [dots [dir n]]
  (set (map (fn [dot]
              (if (> (dir dot) n)
                (update dot dir (fn [v] (- (* 2 n) v)))
                dot))
            dots)))

(defn part1 [input]
  (let [{:keys [dots folds]} (parse-input input)]
    (count (fold dots (first folds)))))

(defn print-paper [dots]
  (dotimes [y (inc (reduce max (map :y dots)))]
    (println (reduce str (map #(if (dots {:x % :y y}) \# \space) (range (inc (reduce max (map :x dots)))))))))

(defn part2 [input]
  (let [{:keys [dots folds]} (parse-input input)]
    (print-paper (reduce fold dots folds))))
