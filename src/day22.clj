(ns day22
  (:require
    clojure.string))

(defn parse-range [r]
  (mapv parse-long (-> r
                       (clojure.string/split #"=")
                       (second)
                       (clojure.string/split #"\.\."))))

(defn parse-input [input]
  (map
    (fn [line]
      (let [[op & ranges] (clojure.string/split line #"[, ]")]
        [op (mapv parse-range ranges)]))
    (clojure.string/split-lines input)))

(defn limit-range [[x y]]
  (when (or (<= -50 x 50)
            (<= -50 y 50))
    [(max x -50) (min y 50)]))

(defn limit-instruction [[op ranges]]
  (let [new-ranges (map limit-range ranges)]
    (when-not (some nil? new-ranges)
      [op (mapv limit-range ranges)])))

(defn within? [[[x1 x2] [y1 y2] [z1 z2]]
               [[x'1 x'2] [y'1 y'2] [z'1 z'2]]]
  (and (<= x1 x'1)
       (<= x'2 x2)
       (<= y1 y'1)
       (<= y'2 y2)
       (<= z1 z'1)
       (<= z'2 z2)))

(defn overlaps? [[[x1 x2] [y1 y2] [z1 z2]]
                 [[x'1 x'2] [y'1 y'2] [z'1 z'2]]]
  (and (or (<= x1 x'1 x2)
           (<= x1 x'2 x2)
           (and (< x'1 x1)
                (> x'2 x2)))
       (or (<= y1 y'1 y2)
           (<= y1 y'2 y2)
           (and (< y'1 y1)
                (> y'2 y2)))
       (or (<= z1 z'1 z2)
           (<= z1 z'2 z2)
           (and (< z'1 z1)
                (> z'2 z2)))))

(defn remove-cuboid [[[x1 x2] [y1 y2] [z1 z2] :as cuboid1]
                     [[x'1 x'2] [y'1 y'2] [z'1 z'2] :as cuboid2]]
  (cond
    (within? cuboid2 cuboid1)
    #{}

    (overlaps? cuboid1 cuboid2)
    (set (for [xr [[x1 (dec (max x1 x'1))] [(max x1 x'1) (min x2 x'2)] [(inc (min x2 x'2)) x2]]
               yr [[y1 (dec (max y1 y'1))] [(max y1 y'1) (min y2 y'2)] [(inc (min y2 y'2)) y2]]
               zr [[z1 (dec (max z1 z'1))] [(max z1 z'1) (min z2 z'2)] [(inc (min z2 z'2)) z2]]
               :when (not (within? cuboid2 [xr yr zr]))]
           [xr yr zr]))

    :else
    #{cuboid1}))

(defn apply-instruction [ons [op r]]
  (case op
    "on" (into #{r} (mapcat #(remove-cuboid % r) ons))
    "off" (set (mapcat #(remove-cuboid % r) ons))))

(defn apply-instructions [instructions]
  (reduce apply-instruction #{} instructions))

(defn count-points [[[x1 x2] [y1 y2] [z1 z2]]]
  (* (- x2 x1 -1) (- y2 y1 -1) (- z2 z1 -1)))

(defn part1 [input]
  (->> (parse-input input)
       (keep limit-instruction)
       (apply-instructions)
       (map count-points)
       (reduce +)))

(defn part2 [input]
  (->> (parse-input input)
       (apply-instructions)
       (map count-points)
       (reduce +)))
