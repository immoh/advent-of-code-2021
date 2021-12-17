(ns day15
  (:require
    clojure.string))

(defn parse-input [input]
  (let [lines (clojure.string/split-lines input)]
    {:grid    (into {} (for [[i line] (map-indexed vector lines)
                             [j c] (map-indexed vector line)]
                         [[i j] (parse-long (str c))]))
     :rows    (count lines)
     :columns (count (first lines))}))

(defn pos+ [pos1 pos2]
  (mapv + pos1 pos2))

(defn neighbors [pos]
  (map (partial pos+ pos) [[0 1] [0 -1] [1 0] [-1 0]]))

(defn find-shortest-path [start end neighbors-fn dist-fn]
  (loop [wip {start 0}
         visited #{start}]
    (when-let [[node dist] (first (sort-by val wip))]
      (if (= node end)
        dist
        (let [neighbors (remove visited (neighbors-fn node))]
          (recur (merge-with min
                             (dissoc wip node)
                             (zipmap neighbors
                                     (map (fn [node2]
                                            (+ dist (dist-fn node node2)))
                                          neighbors)))
                 (conj visited node)))))))

(defn part1 [input]
  (let [{:keys [grid rows columns]} (parse-input input)]
    (find-shortest-path [0 0]
                        [(dec rows) (dec columns)]
                        (fn [node]
                          (filter (fn [[x y]]
                                    (and (<= 0 x (dec rows))
                                         (<= 0 y (dec columns))))
                                  (neighbors node)))
                        (fn [_ node]
                          (grid node)))))

(defn wrap [n]
  (inc (mod (dec n) 9)))

(defn part2 [input]
  (let [{:keys [grid rows columns]} (parse-input input)]
    (find-shortest-path [0 0]
                        [(dec (* 5 rows)) (dec (* 5 columns))]
                        (fn [node]
                          (filter (fn [[x y]]
                                    (and (<= 0 x (dec (* 5 rows)))
                                         (<= 0 y (dec (* 5 columns)))))
                                  (neighbors node)))
                        (fn [_ [x y]]
                          (wrap (+ (get grid [(mod x rows) (mod y columns)])
                                   (quot x rows)
                                   (quot y columns)))))))
