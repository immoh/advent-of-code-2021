(ns day23
  (:require
    clojure.set
    clojure.string))

(def amphipods [\A \B \C \D])

(def amphipod-to-col (zipmap amphipods [3 5 7 9]))

(def col-to-amphipod (clojure.set/map-invert amphipod-to-col))

(def amphipod-score (zipmap amphipods [1 10 100 1000]))

(defn parse-input [input]
  (let [board (into {} (for [[i line] (map-indexed vector (clojure.string/split-lines input))
                             [j c] (map-indexed vector line)
                             :when (not (#{\# \space} c))]
                         [[i j] c]))
        rooms (sort (keys (filter #((set amphipods) (val %)) board)))
        room-cols (set (map second rooms))]

    {:board   board
     :rooms   rooms
     :hallway (sort (remove #(room-cols (second %)) (keys (filter #(= \. (val %)) board))))}))


(defn room-vals [rooms board]
  (map (partial board) rooms))

(defn finished? [rooms board]
  (let [vs (room-vals rooms board)]
    (= vs (take (count vs) (cycle amphipods)))))

(defn in-correct-position? [rooms board [x y ]]
  (let [amphipod (col-to-amphipod y)]
    (every? #(= (get board %) amphipod) (filter (fn [[x' y']] (and (= y y') (>= x' x))) rooms))))

(defn route* [[x1 y1] [x2 y2]]
  (rest (if (< x1 x2)
          (concat (if (< y1 y2)
                    (for [z (range y1 (inc y2))]
                      [x1 z])
                    (for [z (range y1 (dec y2) -1)]
                      [x1 z]))
                  (rest (for [z (range x1 (inc x2))]
                          [z y2])))
          (concat (for [z (range x1 (dec x2) -1)]
                    [z y1])
                  (rest (if (< y1 y2)
                          (for [z (range y1 (inc y2))]
                            [x2 z])
                          (for [z (range y1 (dec y2) -1)]
                            [x2 z])))))))

(def route (memoize route*))

(defn route-free? [board route]
  (every? #(= (get board %) \.) route))

(defn move [board from to]
  (assoc board from (get board to)
               to (get board from)))

(defn moves-from-rooms [rooms hallway board]
  (into {} (for [room (remove (partial in-correct-position? rooms board) (remove #(= (get board %) \.) rooms))
                 hallway-space (filter (fn [p] (= (get board p) \.)) hallway)
                 :let [route (route room hallway-space)]
                 :when (route-free? board route)]
             [(move board room hallway-space)
              (* (count route) (amphipod-score (get board room)))])))

(defn find-dest-room [rooms board hallway-space]
  (let [amphipod (get board hallway-space)
        col (amphipod-to-col amphipod)]
    (let [dest (->> rooms
                    (filter (fn [[_ y]] (= y col)))
                    (sort)
                    (reverse)
                    (drop-while #(= (get board %) amphipod))
                    (first))]
      (when (= (get board dest) \.)
        dest))))


(defn moves-from-hallway [rooms hallway board]
  (into {} (keep (fn [hallway-space]
                   (when-let [room (find-dest-room rooms board hallway-space)]
                     (let [route (route hallway-space room)]
                       (when (route-free? board route)
                         [(move board hallway-space room)
                          (* (count route) (amphipod-score (get board hallway-space)))]))))
                 (remove (fn [p] (= (get board p) \.)) hallway))))

(defn neighbors [rooms hallway board]
  (let [hallway-moves (moves-from-hallway rooms hallway board)]
    (if (seq hallway-moves)
      hallway-moves
      (moves-from-rooms rooms hallway board))))

(defn find-shortest-path [start end-pred neighbors-to-dist-fn]
  (loop [wip {start 0}
         seen #{}]
    (let [sorted (sort-by val (remove (comp seen key) wip))]
      (when-let [[node dist] (first sorted)]
        (if (end-pred node)
          dist
          (recur (concat (rest sorted) (let [neighbors-to-dist (neighbors-to-dist-fn node)]
                                         (zipmap (keys neighbors-to-dist)
                                                 (map (partial + dist) (vals neighbors-to-dist)))))
                 (conj seen node)))))))

(defn part1 [input]
  (let [{:keys [board rooms hallway]} (parse-input input)]
    (find-shortest-path board (partial finished? rooms) (partial neighbors rooms hallway))))


(defn patch-input [input]
  (let [[start end] (split-at 3 (clojure.string/split-lines input))]
    (clojure.string/join \newline (concat start (clojure.string/split-lines "  #D#C#B#A#\n  #D#B#A#C#") end))))

(defn part2 [input]
  (let [{:keys [board rooms hallway]} (parse-input (patch-input input))]
    (find-shortest-path board (partial finished? rooms) (partial neighbors rooms hallway))))
