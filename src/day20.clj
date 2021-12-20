(ns day20
  (:require
    clojure.string))

(defn parse-grid [input]
  (let [lines (clojure.string/split-lines input)]
    (set (for [[i line] (map-indexed vector lines)
               [j c] (map-indexed vector line)
               :when (= c \#)]
           [i j]))))

(defn parse-input [input]
  (let [[algo image] (clojure.string/split input #"\n\n")]
    {:algo  (set (for [[i c] (map-indexed vector (reduce concat (clojure.string/split-lines algo)))
                       :when (= c \#)]
                   i))
     :image (let [grid (parse-grid image)]
              {:lit          grid
               :min-x        (reduce min (map first grid))
               :max-x        (reduce max (map first grid))
               :min-y        (reduce min (map second grid))
               :max-y        (reduce max (map second grid))
               :outside-lit? false})}))

(defn binary-to-dec [s]
  (Integer/parseInt s 2))

(defn input-pixels [[x y]]
  (for [i [-1 0 1]
        j [-1 0 1]]
    [(+ x i) (+ y j)]))

(defn algo-index [{:keys [lit min-x max-x min-y max-y outside-lit?]} p]
  (binary-to-dec (apply str (map (fn [[x y :as p]]
                                   (if (and (<= min-x x max-x)
                                            (<= min-y y max-y))
                                     (if (lit p) "1" "0")
                                     (if outside-lit? "1" "0")))
                                 (input-pixels p)))))

(defn pixel-lit? [algo image p]
  (algo (algo-index image p)))

(defn enhance-lit [algo {:keys [min-x max-x min-y max-y] :as image}]
  (set (for [i (range (dec min-x) (+ 2 max-x))
             j (range (dec min-y) (+ 2 max-y))
             :when (pixel-lit? algo image [i j])]
         [i j])))

(defn enhance [algo {:keys [min-x max-x min-y max-y outside-lit?] :as image}]
  {:lit          (enhance-lit algo image)
   :min-x        (dec min-x)
   :max-x        (inc max-x)
   :min-y        (dec min-y)
   :max-y        (inc max-y)
   :outside-lit? (if (algo 0) (not outside-lit?) outside-lit?)})

(defn part1 [input]
  (let [{:keys [algo image]} (parse-input input)]
    (count (:lit (first (drop 2 (iterate (partial enhance algo) image)))))))

(defn part2 [input]
  (let [{:keys [algo image]} (parse-input input)]
    (count (:lit (first (drop 50 (iterate (partial enhance algo) image)))))))
