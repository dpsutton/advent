(ns day04
  (:require
    [clojure.string :as str]))

(def problem-input (slurp "src/day04.txt"))

(def sample-input
  "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(defn ctns? [x y]
  (and (<= (first x) (first y))
       (>= (second x) (second y))))

(defn overlaps? [[x1 x2] [y1 y2]]
  (cond (= x1 y1) true
        (< x1 y1) (>= x2 y1)
        (> x1 y1) (<= x2 y2)))

(defn parse [s] (mapv parse-long (next (re-find #"(\d+)-(\d+)" s))))

(defn process-line [l]
  (str/split l #","))

(defn solve*
  [pred input]
  (transduce (comp (map process-line)
                   (map #(map parse %))
                   (filter (fn [[x y]]
                             (or (pred x y) (pred y x)))))
             (fn
               ([] (conj))
               ([acc] (count acc))
               ([acc x] (conj acc x)))
             (str/split-lines input)))

(defn solve-a
  ([] (solve-a problem-input))
  ([input]
   (solve* ctns? input)))

(defn solve-b
  ([] (solve-b problem-input))
  ([input]
   (solve* overlaps? input)))


(defn solve [{:keys [input]}]
  (let [input (if input (slurp input) problem-input)]
    (time (println (solve-a input)))
    (time (println (solve-b input)))))

(comment
  (solve-a)
  (solve-b)
  )
