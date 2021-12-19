(ns day18
  (:require
    clojure.java.math
    clojure.string))

(defn parse-line [line]
  (:vals (reduce
           (fn [{:keys [parent pos vals]} c]
             (case c
               \[ {:parent (str parent pos) :pos "0" :vals vals}
               \] {:parent (subs parent 0 (dec (count parent))) :vals vals}
               \, {:parent parent :pos "1" :vals vals}
               {:parent parent :vals (assoc vals (str (subs parent 1) pos) (parse-long (str c)))}))
           {:parent "" :pos "0"}
           line)))

(defn parse-input [input]
  (map parse-line (clojure.string/split-lines input)))

(defn second-key [k]
  (str (apply str (butlast k)) "1"))

(defn find-explode-pos [s]
  (->> (keys s)
       (sort)
       (filter (fn [k]
                 (and (> (count k) 4)
                      (= (last k) \0)
                      (contains? s (second-key k)))))
       (first)))

(defn add-to-rightmost [s k v]
  (if-let [k2 (->> (keys s)
                   (sort)
                   (take-while #(not= % k))
                   (last))]
    (update s k2 + v)
    s))

(defn add-to-leftmost [s k v]
  (if-let [k2 (->> (keys s)
                   (sort)
                   (drop-while #(not= % k))
                   (rest)
                   (first))]
    (update s k2 + v)
    s))

(defn explode-one [s]
  (if-let [explode-pos (find-explode-pos s)]
    (let [explode-pos-2 (second-key explode-pos)
          x (s explode-pos)
          y (s explode-pos-2)]
      (-> s
          (add-to-rightmost explode-pos x)
          (add-to-leftmost explode-pos-2 y)
          (dissoc explode-pos explode-pos-2)
          (assoc (apply str (butlast explode-pos)) 0)))
    s))

(defn split-one [s]
  (if-let [split-pos (->> s
                          (filter #(> (val %) 9))
                          (keys)
                          (sort)
                          (first))]
    (let [x (int (clojure.java.math/floor (/ (s split-pos) 2)))]
      (-> s
          (assoc (str split-pos "0") x)
          (assoc (str split-pos "1") (- (s split-pos) x))
          (dissoc split-pos)))
    s))

(defn reduce-number [s]
  (let [s2 (explode-one s)]
    (if (= s s2)
      (let [s3 (split-one s2)]
        (if (= s s3)
          s
          (recur s3)))
      (recur s2))))

(defn tally-2 [s1 s2]
  (merge
    (zipmap (map #(str "0" %) (keys s1)) (vals s1))
    (zipmap (map #(str "1" %) (keys s2)) (vals s2))))

(defn tally [ss]
  (reduce (comp reduce-number tally-2) ss))

(defn magnitude [s]
  (if (= 1 (count s))
    (first (vals s))
    (let [k (->> (keys s)
                 (sort)
                 (filter (fn [k]
                           (and (= (last k) \0)
                                (contains? s (second-key k)))))
                 (first))
          k2 (second-key k)]
      (recur (-> s
                 (dissoc k k2)
                 (assoc (apply str (butlast k)) (+ (* 3 (s k)) (* 2 (s k2)))))))))


(defn part1 [input]
  (magnitude (tally (parse-input input))))

(defn part2 [input]
  (let [nums (parse-input input)]
    (reduce max (for [s1 nums
                      s2 nums
                      :when (not= s1 s2)]
                  (magnitude (tally [s1 s2]))))))
