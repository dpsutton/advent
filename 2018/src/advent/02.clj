(ns advent.02
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

(def data
  (delay (->> "src/advent/02.data"
              io/reader
              line-seq)))

(defn classify [s]
  (let [counts (-> s frequencies vals set (set/intersection #{2 3}))]
    (case counts
      #{2 3} :both
      #{2}   :two
      #{3}   :three
      :none)))

(defn checksum [classificiations]
  (letfn [(combine [[x1 x2] [y1 y2]] [(+ x1 y1) (+ x2 y2)])]
    (let [f             {:both [1 1] :two [1 0] :three [0 1] :none [0 0]}
          [twos threes] (reduce (fn [acc c] (combine acc (f c)))
                                [0 0]
                                classificiations)]
      (* twos threes))))

(def test-data
  ["abcdef" "bababc" "abbcde" "abcccd" "aabcdd" "abcdee" "ababab"])

(defn solve1
  ([] (solve1 @data))
  ([input] (->> input (map classify) (checksum))))

(def test-data-2
  ["abcde" "fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz"])

(defn seq-differences [s1 s2]
  (let [paired (map vector s1 s2)]
    (->> paired
         (remove (fn [[x y]] (= x y)))
         count)))

(defn single-difference? [s1 s2]
  (= 1 (seq-differences s1 s2)))

(defn without-discrepancy [[s1 s2]]
  (let [paired (map vector s1 s2)]
    (str/join (reduce (fn [without [x y]]
                        (if (= x y)
                          (conj without x)
                          without))
                      []
                      paired))))

(defn solve2
  ([] (solve2 @data))
  ([input]
   (let [sorted (sort input)
         joined (map vector sorted (drop 1 (cycle sorted)))]
     (->> joined
          (filter #(apply single-difference? %))
          first
          without-discrepancy))))

(defn without-index [n s]
  (str (subs s 0 n)
       (subs s (inc n) (count s))))

(defn first-duplicate
  ([xs] (first-duplicate xs #{}))
  ([[x & xs] seen]
   (when x
     (if (seen x)
       x
       (recur xs (conj seen x))))))

(defn solve2'
  ([] (solve2' @data))
  ([input]
   (some first-duplicate
         (map (fn [n] (map (partial without-index n) input))
              (range)))))
