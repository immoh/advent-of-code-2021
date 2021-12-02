(ns day02
  (:require
    clojure.string))

(defn parse-input [input]
  (map (fn [s]
         (let [[a b] (clojure.string/split s #" ")]
           [a (parse-long b)]))
       (clojure.string/split-lines input)))

(defn move [[h d] [c x]]
  (case c
    "forward" [(+ h x) d]
    "down" [h (+ d x)]
    "up" [h (- d x)]))

(defn part1 [input]
  (reduce * (reduce move [0 0] (parse-input input))))

(defn move2 [[h d a] [c x]]
  (case c
    "down" [h d (+ a x)]
    "up" [h d (- a x)]
    "forward" [(+ h x) (+ d (* a x)) a]))

(defn part2 [input]
  (reduce * (take 2 (reduce move2 [0 0 0] (parse-input input)))))
