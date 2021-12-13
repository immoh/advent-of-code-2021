(ns day11
  (:require
    clojure.string))

(defn parse-input [input]
  (let [lines (clojure.string/split-lines input)]
    (into {} (for [[i line] (map-indexed vector lines)
                   [j c] (map-indexed vector line)]
               [[i j] (parse-long (str c))]))))

(defn neighbors [pos]
  (filter (fn [[x y]]
            (and (<= 0 x 9)
                 (<= 0 y 9)))
          (for [dx [-1 0 1]
                dy [-1 0 1]
                :when (not= 0 dx dy)]
            (mapv + pos [dx dy]))))

(defn flash [grid pos]
  (reduce (fn [grid pos] (update grid pos inc)) grid (neighbors pos)))

(defn handle-flashes
  ([grid]
   (handle-flashes grid #{}))
  ([grid flashed]
   (if-let [new-flashes (seq (remove flashed (map key (filter #(> (val %) 9) grid))))]
     (recur (reduce flash grid new-flashes) (into flashed new-flashes))
     grid)))

(defn increase-energy [grid]
  (zipmap (keys grid) (map inc (vals grid))))

(defn reset-flashed [grid]
  (zipmap (keys grid) (map (fn [x] (if (> x 9) 0 x)) (vals grid))))

(defn next-step [grid]
  (-> grid
      (increase-energy)
      (handle-flashes)
      (reset-flashed)))

(defn flashed-count [grid]
  (count (filter zero? (vals grid))))

(defn part1 [input]
  (->> (parse-input input)
       (iterate next-step)
       (take 100)
       (map flashed-count)
       (reduce +)))

(defn all-flash? [grid]
  (every? zero? (vals grid)))

(defn part2 [input]
  (->> (parse-input input)
       (iterate next-step)
       (take-while (complement all-flash?))
       (count)))
