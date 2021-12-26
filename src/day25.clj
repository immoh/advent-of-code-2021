(ns day25
  (:require
    clojure.string))

(defn parse-input [input]
  (let [lines (clojure.string/split-lines input)
        grid (into {} (for [[i line] (map-indexed vector lines)
                            [j c] (map-indexed vector line)
                            :when (not= \. c)]
                        [[i j] c]))]

    {:eastbound  (set (keys (filter (comp #{\>} val) grid)))
     :southbound (set (keys (filter (comp #{\v} val) grid)))
     :rows       (count lines)
     :columns    (count (first lines))}))

(defn move-eastbound-1 [{:keys [eastbound southbound columns]} [x y :as pos]]
  (let [new-pos [x (mod (inc y) columns)]]
    (if (or (eastbound new-pos) (southbound new-pos))
      pos
      new-pos)))

(defn move-southbound-1 [{:keys [eastbound southbound rows]} [x y :as pos]]
  (let [new-pos [(mod (inc x) rows) y]]
    (if (or (eastbound new-pos) (southbound new-pos))
      pos
      new-pos)))

(defn move-eastbound [game]
  (update game :eastbound (fn [eastbound] (set (map (partial move-eastbound-1 game) eastbound)))))

(defn move-southbound [game]
  (update game :southbound (fn [southbound] (set (map (partial move-southbound-1 game) southbound)))))

(defn move [game]
  (-> game
      (move-eastbound)
      (move-southbound)))

(defn find-stable [game]
  (loop [game game
         i 1]
    (let [new-game (move game)]
      (if (= new-game game)
        i
        (recur new-game (inc i))))))

(defn part1 [input]
  (find-stable (parse-input input)))
