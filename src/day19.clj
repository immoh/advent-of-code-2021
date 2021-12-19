(ns day19
  (:require
    clojure.java.math
    clojure.string))

(defn parse-input [input]
  (map (fn [scanner]
         (let [[_ & beacons] (clojure.string/split-lines scanner)]
           (map
             (fn [beacon]
               (mapv parse-long (clojure.string/split beacon #",")))
             beacons)))
       (clojure.string/split input #"\n\n")))

(def all-rotations
  [[]
   [:x]
   [:y]
   [:x :x]
   [:x :y]
   [:y :x]
   [:y :y]
   [:x :x :x]
   [:x :x :y]
   [:x :y :x]
   [:x :y :y]
   [:y :x :x]
   [:y :y :x]
   [:y :y :y]
   [:x :x :x :y]
   [:x :x :y :x]
   [:x :x :y :y]
   [:x :y :x :x]
   [:x :y :y :y]
   [:y :x :x :x]
   [:y :y :y :x]
   [:x :x :x :y :x]
   [:x :y :x :x :x]
   [:x :y :y :y :x]])

(defn rotate-point [[x y z] rotation]
  (case rotation
    :x [x z (- y)]
    :y [(- z) y x]))

(defn apply-rotations [p rotations]
  (reduce rotate-point p rotations))

(defn scanner-rotations [scanner]
  (map
    (fn [rotations]
      (map
        (fn [p]
          (apply-rotations p rotations))
        scanner))
    all-rotations))

(defn adjust [scanner-rotation point beacon]
  (let [adjustment (mapv - beacon point)]
    [(map
       #(mapv + adjustment %)
       scanner-rotation)
     adjustment]))

(defn aligns? [adjusted fixed-scanner]
  (>= (count (filter (set fixed-scanner) adjusted))
      12))

(defn find-pair-and-adjust [scanners fixed-scanners]
  (first (for [scanner scanners
               scanner-rotation (scanner-rotations scanner)
               scanner-point scanner-rotation
               fixed-scanner fixed-scanners
               beacon fixed-scanner
               :let [[adjusted adjustment] (adjust scanner-rotation scanner-point beacon)]
             :when (aligns? adjusted fixed-scanner)]
         [scanner adjusted adjustment])))

(defn find-beacons [scanners]
  (loop [remaining (rest scanners)
         found (take 1 scanners)]
    (if (seq remaining)
      (let [[scanner adjusted] (find-pair-and-adjust remaining found)]
        (recur (remove #{scanner} remaining)
               (conj found adjusted)))
      (distinct (mapcat identity found)))))

(defn part1 [input]
  (count (find-beacons (parse-input input))))

(defn find-scanner-locations [scanners]
  (loop [remaining (rest scanners)
         found (take 1 scanners)
         locations [[0 0 0]]]
    (if (seq remaining)
      (let [[scanner adjusted adjustment] (find-pair-and-adjust remaining found)]
        (recur (remove #{scanner} remaining)
               (conj found adjusted)
               (conj locations adjustment)))
      locations)))

(defn manhattan-distance [p1 p2]
  (reduce + (map #(clojure.java.math/abs (- % %2)) p2 p1)))

(defn largest-manhattan-distance [positions]
  (reduce max (for [i (range 0 (count positions))
                    j (range (inc i) (count positions))]
                (manhattan-distance (positions i) (positions j)))))

(defn part2 [input]
  (largest-manhattan-distance (find-scanner-locations (parse-input input))))
