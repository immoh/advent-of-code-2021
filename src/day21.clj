(ns day21
  (:require
    clojure.string))

(defn parse-input [input]
  (map (fn [line]
         (-> line
             (clojure.string/split #": ")
             (second)
             (parse-long)))
       (clojure.string/split-lines input)))

(defn play-turn [{:keys [dice player positions scores]}]
  (let [n (+ (* 3 dice) 3)
        new-position (mod (+ (positions player) n) 10)]
    {:dice      (+ 3 dice)
     :player    (mod (inc player) 2)
     :positions (assoc positions player new-position)
     :scores    (update scores player + (inc new-position))}))

(defn checksum [{:keys [dice scores]}]
  (* (dec dice) (apply min scores)))

(defn part1 [input]
  (->> {:dice     1
       :player    0
       :positions (mapv dec (parse-input input))
       :scores    [0 0]}
       (iterate play-turn)
       (drop-while (fn [game] (every? #(< % 1000) (:scores game))))
       (first)
       (checksum)))

(def dice-result-freqs (frequencies (for [dice1 (range 1 4)
                                          dice2 (range 1 4)
                                          dice3 (range 1 4)]
                                      (+ dice1 dice2 dice3))))

(defn play-turn2 [{:keys [universes player positions scores]}]
  (map
    (fn [[n c]]
      (let [new-position (mod (+ (positions player) n) 10)]
        {:player    (mod (inc player) 2)
         :positions (assoc positions player new-position)
         :scores    (update scores player + (inc new-position))
         :universes (* universes c)}))
    dice-result-freqs))

(defn part2 [input]
  (loop [remaining [{:player    0
                     :positions (mapv dec (parse-input input))
                     :scores    [0 0]
                     :universes 1}]
         wins [0 0]]
    (if-let [{:keys [scores universes] :as game} (first remaining)]
      (if-let [winning-position (->> scores
                                     (map-indexed vector)
                                     (filter (fn [[_ score]] (>= score 21)))
                                     (ffirst))]
        (recur (rest remaining) (update wins winning-position + universes))
        (recur (into (rest remaining) (play-turn2 game)) wins))
      (apply max wins))))
