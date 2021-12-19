(ns day16
  (:require
    clojure.string))

(defn binary-to-dec [bs]
  (Long/parseLong bs 2))

(defn parse-literal-value [b]
  (loop [b b
         value ""]
    (if (= "1" (subs b 0 1))
      (recur (subs b 5) (str value (subs b 1 5)))
      {:value     (binary-to-dec (str value (subs b 1 5)))
       :remaining (subs b 5)})))

(declare parse-transmission)

(defn parse-transmissions-with-length [b]
  (let [length (binary-to-dec (subs b 0 15))]
    {:children  (->> {:remaining (subs b 15 (+ 15 length))}
                     (iterate (comp parse-transmission :remaining))
                     (rest)
                     (take-while identity)
                     (map #(dissoc % :remaining)))
     :remaining (subs b (+ 15 length))}))

(defn parse-transmissions-with-count [b]
  (let [children (->> {:remaining (subs b 11)}
                      (iterate (comp parse-transmission :remaining))
                      (rest)
                      (take (binary-to-dec (subs b 0 11))))]
    {:children  (map #(dissoc % :remaining) children)
     :remaining (:remaining (last children))}))

(defn parse-transmission [b]
  (when (seq b)
    (let [version (binary-to-dec (subs b 0 3))
          type-id (binary-to-dec (subs b 3 6))]
      (merge {:version version
              :type-id type-id}
             (if (= type-id 4)
               (parse-literal-value (subs b 6))
               (case (subs b 6 7)
                 "0" (parse-transmissions-with-length (subs b 7))
                 "1" (parse-transmissions-with-count (subs b 7))))))))

(defn collect-versions [{:keys [version children]}]
  (into [version] (mapcat collect-versions children)))

(defn pad-with-zeros [b]
  (apply str (take-last 4 (str "000" b))))

(defn hex-char-to-binary-4 [c]
  (-> (str c)
      (Integer/parseInt 16)
      (Integer/toString 2)
      (pad-with-zeros)))

(defn hex-to-binary [s]
  (apply str (mapcat hex-char-to-binary-4 s)))

(defn part1 [input]
  (->> (hex-to-binary input)
       (parse-transmission)
       (collect-versions)
       (reduce +)))

(defn calculate-value [{:keys [type-id children value]}]
  (case type-id
    0 (reduce + (map calculate-value children))
    1 (reduce * (map calculate-value children))
    2 (reduce min (map calculate-value children))
    3 (reduce max (map calculate-value children))
    4 value
    5 (if (> (calculate-value (first children)) (calculate-value (second children))) 1 0)
    6 (if (< (calculate-value (first children)) (calculate-value (second children))) 1 0)
    7 (if (= (calculate-value (first children)) (calculate-value (second children))) 1 0)))

(defn part2 [input]
  (->> (hex-to-binary input)
       (parse-transmission)
       (calculate-value)))
