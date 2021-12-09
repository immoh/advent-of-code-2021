(ns day09
  (:require
    clojure.string))

(defn parse-input [input]
  (let [lines (clojure.string/split-lines input)]
    (into {} (for [[i line] (map-indexed vector lines)
                   [j c] (map-indexed vector line)]
               [[i j] (parse-long (str c))]))))

(defn pos+ [pos1 pos2]
  (mapv + pos1 pos2))

(defn neighbors [pos]
  (map (partial pos+ pos) [[0 1] [0 -1] [1 0] [-1 0]]))

(defn low-point? [grid pos]
  (every? (partial < (grid pos)) (keep grid (neighbors pos))))

(defn low-points [grid]
  (filter (partial low-point? grid) (keys grid)))

(defn risk-level [grid pos]
  (inc (grid pos)))

(defn part1 [input]
  (let [grid (parse-input input)]
    (->> (low-points grid)
         (map (partial risk-level grid))
         (reduce +))))

(defn extend-basin-point [grid pos]
  (concat [pos] (filter (fn [pos] (< (grid pos 9) 9)) (neighbors pos))))

(defn extend-basin [grid basin]
  (let [new-basin (set (mapcat (partial extend-basin-point grid) basin))]
    (if (= basin new-basin)
      basin
      (recur grid new-basin))))

(defn part2 [input]
  (let [grid (parse-input input)]
    (->> (low-points grid)
         (map #(extend-basin grid #{%}))
         (map count)
         (sort)
         (reverse)
         (take 3)
         (reduce *))))
