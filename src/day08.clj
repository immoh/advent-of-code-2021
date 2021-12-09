(ns day08
  (:require
    clojure.set))

(defn parse-input [input]
  (map (fn [line]
         (map (fn [io]
                (clojure.string/split io #" "))
              (clojure.string/split line #" \| ")))
       (clojure.string/split-lines input)))

(def digits-from-counts
  {2 1
   4 4
   3 7
   7 8})

(defn part1 [input]
  (->> (parse-input input)
       (mapcat second)
       (map count)
       (keep digits-from-counts)
       (count)))

(defn permutations [coll]
  (if (= 1 (count coll))
    [coll]
    (for [head coll
          tail (permutations (remove #{head} coll))]
      (concat [head] tail))))

(defn digits-in-configuration [configuration]
  (zipmap
    (map #(set (map (partial nth configuration) %))
         [#{0 1 2 4 5 6}
          #{2 5}
          #{0 2 3 4 6}
          #{0 2 3 5 6}
          #{1 2 3 5}
          #{0 1 3 5 6}
          #{0 1 3 4 5 6}
          #{0 2 5}
          #{0 1 2 3 4 5 6}
          #{0 1 2 3 5 6}])
    (range)))

(def configurations (map digits-in-configuration (permutations "abcdefg")))

(defn find-configuration [patterns]
  (first (drop-while
           (fn [pattern->digit]
             (some nil? (map pattern->digit (map set patterns))))
           configurations)))

(defn digits->n [digits]
  (reduce + (map * (iterate (partial * 10) 1) (reverse digits))))

(defn part2 [input]
  (reduce + (map (fn [[input output]]
                   (digits->n (map (find-configuration input) (map set output))))
                 (parse-input input))))
