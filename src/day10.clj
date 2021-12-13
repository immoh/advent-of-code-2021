(ns day10
  (:require
    clojure.set
    clojure.string))

(defn parse-input [input]
  (clojure.string/split-lines input))

(def open->close (into {} (map vec (partition 2 "()[]{}<>"))))

(def close->open (clojure.set/map-invert open->close))

(defn status [subsystem]
  (loop [remaining subsystem
         state []]
    (if-let [c (first remaining)]
      (cond
        (open->close c)
        (recur (rest remaining) (conj state c))

        (= (last state) (close->open c))
        (recur (rest remaining) (vec (butlast state)))

        :else
        {:status :corrupted :illegal-character c})
      {:status :incomplete :incomplete state})))

(def points1
  {\) 3
   \] 57
   \} 1197
   \> 25137})

(defn part1 [input]
  (->> (parse-input input)
       (map status)
       (filter #(= (:status %) :corrupted))
       (map :illegal-character)
       (map points1)
       (reduce +)))

(defn completion [incomplete]
  (map open->close (reverse incomplete)))

(def points2
  {\) 1
   \] 2
   \} 3
   \> 4})

(defn score-completion [completion]
  (reduce
    (fn [total-score char-score]
      (+ (* 5 total-score) char-score))
    0
    (map points2 completion)))

(defn middle [xs]
  (->> (sort xs)
       (drop (/ (dec (count xs)) 2))
       (first)))

(defn part2 [input]
  (->> (parse-input input)
       (map status)
       (filter #(= (:status %) :incomplete))
       (map :incomplete)
       (map completion)
       (map score-completion)
       (middle)))

