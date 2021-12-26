(ns day24
  (:require
    clojure.string))

;; Solution based on https://github.com/dphilipson/advent-of-code-2021/blob/d3ccbe8/src/days/day24.rs

(defn parse-input [input]
  (map #(clojure.string/split % #" ")
       (clojure.string/split-lines input)))

(defn to-constraints [stack-ops]
  (:constraints (reduce
                  (fn [{:keys [stack] :as acc} [i [op param]]]
                    (case op
                      :push (update acc :stack conj [i param])
                      :pop (let [[i2 param2] (last stack)]
                             (-> acc
                                 (update :stack #(vec (butlast %)))
                                 (update :constraints conj [i i2 (+ param2 param)])))))
                  {:stack [] :constraints []}
                  (map-indexed vector stack-ops))))

(defn to-stack-op [[check offset]]
  (if (pos? check)
    [:push offset]
    [:pop check]))

(defn parameters [instructions]
  [(parse-long (last (nth instructions 5)))
   (parse-long (last (nth instructions 15)))])

(defn extract-constraints [input]
  (->> (parse-input input)
       (partition 18)
       (map parameters)
       (map to-stack-op)
       (to-constraints)))

(defn max-digits [[i1 i2 diff]]
  (if (pos? diff)
    {i1 9 i2 (- 9 diff)}
    {i2 9 i1 (+ 9 diff)}))

(defn min-digits [[i1 i2 diff]]
  (if (pos? diff)
    {i2 1 i1 (+ 1 diff)}
    {i1 1 i2 (- 1 diff)}))

(defn to-number [digit-map]
  (apply str (map val (sort digit-map))))

(defn part1 [input]
  (->> (extract-constraints input)
       (map max-digits)
       (reduce merge)
       (to-number)))

(defn part2 [input]
  (->> (extract-constraints input)
       (map min-digits)
       (reduce merge)
       (to-number)))
