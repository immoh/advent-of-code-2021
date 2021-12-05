(ns day05
  (:require
    clojure.string))

(defn parse-input [input]
  (map
    (fn [line]
      (map (fn [coords]
             (mapv parse-long (clojure.string/split coords #",")))
           (clojure.string/split line #" -> ")))
    (clojure.string/split-lines input)))

(defn horizontal? [[[x1 _] [x2 _]]]
  (= x1 x2))

(defn vertical? [[[_ y1] [_ y2]]]
  (= y1 y2))

(defn dir [x1 x2]
  (cond
    (= x1 x2) 0
    (< x1 x2) 1
    :else -1))

(defn points-of-line [[[x1 y1 :as p1] [x2 y2 :as p2]]]
  (let [dir [(dir x1 x2) (dir y1 y2)]]
    (take-while #(not= (mapv + p2 dir) %) (iterate (partial mapv + dir) p1))))

(defn overlapping-points [lines]
  (->> lines
       (mapcat points-of-line)
       (frequencies)
       (filter (fn [[_ n]] (> n 1)))))

(defn part1 [input]
  (->> (parse-input input)
       (filter (some-fn horizontal? vertical?))
       (overlapping-points)
       (count)))

(defn part2 [input]
  (->> (parse-input input)
       (overlapping-points)
       (count)))
