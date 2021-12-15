(ns day14
  (:require
    clojure.string))

(defn parse-input [input]
  (let [[template rules] (clojure.string/split input #"\n\n")]
    {:template template
     :rules    (into {} (map (fn [rule]
                               (let [[a b] (clojure.string/split rule #" -> ")]
                                 [a [(str (first a) b)
                                     (str b (second a))]]))
                             (clojure.string/split-lines rules)))}))

(defn apply-rules [rules polymer-freqs]
  (reduce
    (partial merge-with +)
    (map (fn [[pair n]]
           (zipmap (rules pair) (repeat n)))
         polymer-freqs)))

(defn element-freqs [template pair-freqs]
  (->> pair-freqs
       (mapcat (fn [[pair n]]
                 [{(first pair) n} {(second pair) n}]))
       (reduce (partial merge-with +))
       (map (fn [[c n]]
              [c
               (/ (if (#{(first template) (last template)} c)
                    (inc n)
                    n)
                  2)]))
       (into {})))

(defn checksum [template pair-freqs]
  (let [freq-vals (vals (element-freqs template pair-freqs))]
    (- (apply max freq-vals) (apply min freq-vals))))

(defn simulate [rules template n]
  (->> template
       (partition 2 1)
       (map (partial apply str))
       (frequencies)
       (iterate (partial apply-rules rules))
       (drop n)
       (first)
       (checksum template)))

(defn part1 [input]
  (let [{:keys [rules template]} (parse-input input)]
    (simulate rules template 10)))

(defn part2 [input]
  (let [{:keys [rules template]} (parse-input input)]
    (simulate rules template 40)))
