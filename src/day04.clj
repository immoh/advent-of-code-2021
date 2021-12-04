(ns day04
  (:require
    clojure.string))

(defn parse-board [s]
  (let [rows (map #(map parse-long (clojure.string/split (clojure.string/triml %) #"\s+"))
                  (clojure.string/split-lines s))
        cols (map (fn [i]
                    (map (fn [row] (nth row i)) rows))
                  (range 5))]
    {:rows-and-cols (concat rows cols)
     :all-nums      (mapcat identity rows)}))

(defn parse-input [input]
  (let [[drawn-numbers & boards] (clojure.string/split input #"\n\n")]
    {:numbers (map parse-long (clojure.string/split drawn-numbers #","))
     :boards  (map parse-board boards)}))

(defn winning-board? [drawn-numbers board]
  (some (partial every? (set drawn-numbers)) (:rows-and-cols board)))

(defn winning-boards [{:keys [numbers boards]}]
  (:winning-boards (reduce
                     (fn [{:keys [drawn-numbers remaining-boards winning-boards]} last-drawn-number]
                       (let [drawn-numbers (conj drawn-numbers last-drawn-number)
                             new-winning-boards (seq (filter (partial winning-board? drawn-numbers) remaining-boards))]
                         {:drawn-numbers    drawn-numbers
                          :remaining-boards (reduce disj remaining-boards new-winning-boards)
                          :winning-boards   (concat winning-boards (map (fn [board]
                                                                          {:board             board
                                                                           :drawn-numbers     drawn-numbers})
                                                                        new-winning-boards))}))
                     {:drawn-numbers    []
                      :remaining-boards (set boards)
                      :winning-boards   []}
                     numbers)))

(defn checksum [{:keys [board drawn-numbers]}]
  (* (last drawn-numbers) (reduce + (remove (set drawn-numbers) (:all-nums board)))))

(defn part1 [input]
  (checksum (first (winning-boards (parse-input input)))))

(defn part2 [input]
  (checksum (last (winning-boards (parse-input input)))))
