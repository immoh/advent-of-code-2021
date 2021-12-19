(ns day17
  (:require
    clojure.string))

(defn parse-input [input]
  (map
    (fn [range]
      (map parse-long
           (-> range
               (clojure.string/split #"=")
               (second)
               (clojure.string/split #"\.\."))))
    (-> input
        (clojure.string/replace "target area: " "")
        (clojure.string/split #", "))))

(defn move-probe [[[x y] [dx dy]]]
  [[(+ x dx)
    (+ y dy)]
   [(cond
      (zero? dx) 0
      (pos? dx) (dec dx)
      (neg? dx) (inc dx))
    (dec dy)]])

(defn above-area? [[_ [min-y _]] [_ y]]
  (>= y min-y))

(defn probe-route [area start dir]
  (->> [start dir]
       (iterate move-probe)
       (map first)
       (take-while (partial above-area? area))))

(defn visits-area? [[[min-x max-x] [min-y max-y]] route]
  (some (fn [[x y]]
          (and (<= min-x x max-x)
               (<= min-y y max-y)))
        route))

(defn max-height [route]
  (reduce max (map second route)))

(defn part1 [input]
  (let [[[min-x _] [min-y _] :as area] (parse-input input)]
    (->> (for [dx (range 1 min-x)
               dy (range 1 (- min-y))]
           [dx dy])
         (map (partial probe-route area [0 0]))
         (filter (partial visits-area? area))
         (map max-height)
         (reduce max))))

(defn part2 [input]
  (let [[[min-x _] [min-y _] :as area] (parse-input input)]
    (->> (for [dx (range (* 2 (- min-x)) (* 2 min-x))
               dy (range (* 2 min-y) (* 2 (- min-y)))]
           [dx dy])
         (map (partial probe-route area [0 0]))
         (filter (partial visits-area? area))
         (count))))
