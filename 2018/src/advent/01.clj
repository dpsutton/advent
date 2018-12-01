(ns advent.01
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-line [line]
  (let [op     (subs line 0 1)
        digits (apply str (drop 1 line))]
    ((case op "+" + "-" -) (Integer/parseInt digits))))

(defn load-data! [file]
  (line-seq (io/reader file)))

(defn solve1
  ([] (solve1 (->> "src/advent/01.data"
                   load-data!
                   (map parse-line))))
  ([values]
   (reduce + 0 values)))

(defn first-duplicate
  ([l] (first-duplicate l #{}))
  ([l seen]
   (let [[head & tail] l]
     (if (contains? seen head)
       head
       (recur tail (conj seen head))))))

(defn solve2
  ([] (solve2 (map parse-line (load-data! "src/advent/01.data"))))
  ([op&values]
   (let [values (cycle op&values)
         sums   (drop 1 (reductions + 0 values))]
     (first-duplicate sums))))
