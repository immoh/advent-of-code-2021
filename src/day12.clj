(ns day12
  (:require
    clojure.string))

(defn parse-input [input]
  (distinct (mapcat (fn [line]
                      (let [[a b] (clojure.string/split line #"-")]
                        [[a b] [b a]]))
                    (clojure.string/split-lines input))))

(defn lower-case? [s]
  (= s (clojure.string/lower-case s)))

(defn next-paths [connections target-allowed-fn path]
  (let [last (last path)]
    (if (= last "end")
      [path]
      (->> connections
           (filter #(= (first %) last))
           (map second)
           (filter (partial target-allowed-fn path))
           (map (partial conj path))))))

(defn find-all-paths
  ([connections target-allowed-fn]
   (find-all-paths connections target-allowed-fn [["start"]]))
  ([connections not-allowed-fn paths]
   (let [new-paths (mapcat (partial next-paths connections not-allowed-fn) paths)]
     (if (= new-paths paths)
       paths
       (recur connections not-allowed-fn new-paths)))))

(defn allow-lower-case-once [path target]
  (not (and (lower-case? target)
            ((set path) target))))

(defn part1 [input]
  (count (find-all-paths (parse-input input) allow-lower-case-once)))

(defn lower-case-visited-twice? [path]
  (some #(> (val %) 1) (frequencies (filter lower-case? path))))


(defn allow-one-lowercase-twice [path target]
  (not (or (= target "start")
           (and (lower-case? target)
                (lower-case-visited-twice? path)
                ((set path) target)))))

(defn part2 [input]
  (count (find-all-paths (parse-input input) allow-one-lowercase-twice)))
