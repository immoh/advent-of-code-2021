(ns day03)

(defn parse-input [input]
  (clojure.string/split-lines input))

(defn most-common-bit [bits]
  (let [freqs (frequencies bits)]
    (if (< (get freqs \1 0) (get freqs \0 0)) \0 \1)))

(defn least-common-bit [bits]
  (let [freqs (frequencies bits)]
    (if (< (get freqs \1 0) (get freqs \0 0)) \1 \0)))

(defn gamma-rate [nums]
  (reduce str (map
                (fn [i]
                  (most-common-bit (map #(nth % i) nums)))
                (range 0 (count (first nums))))))

(defn epsilon-rate [nums]
  (reduce str (map
                (fn [i]
                  (least-common-bit (map #(nth % i) nums)))
                (range 0 (count (first nums))))))


(defn to-decimal [s]
  (Long/parseLong s 2))

(defn part1 [input]
  (let [nums (parse-input input)]
    (* (to-decimal (gamma-rate nums)) (to-decimal (epsilon-rate nums)))))

(defn find-with-criteria [nums bit-criteria]
  (first (reduce
           (fn [nums i]
             (if (= (count nums) 1)
               nums
               (filter
                 (fn [num]
                   (= (nth num i) (bit-criteria (map #(nth % i) nums))))
                 nums)))
           nums
           (range 0 (count (first nums))))))

(defn oxygen-generator-rating [nums]
  (find-with-criteria nums most-common-bit))

(defn co2-scrubber-rating [nums]
  (find-with-criteria nums least-common-bit))

(defn part2 [input]
  (let [nums (parse-input input)]
    (* (to-decimal (oxygen-generator-rating nums))
       (to-decimal (co2-scrubber-rating nums)))))
