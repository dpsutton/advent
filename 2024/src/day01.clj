(ns day01
  (:require
   [clojure.string :as str]))

(def problem-input (slurp "src/day01.txt"))

(def sample-input
  "3   4
4   3
2   5
1   3
3   9
3   3")

(defn difference [x y] (abs (- x y)))

(defn parse-a [l] (->> (str/split l #"\s+") (map parse-long)))

(comment
  (parse-a "1    4")
  )

(defn solve-a
  ([] (solve-a problem-input))
  ([input]
   (letfn [(pointwise-conj [colls tuple]
             (map conj colls tuple))]
     (let [[a b] (->> (str/split-lines input)
                      (map parse-a)
                      (reduce pointwise-conj [[] []])
                      (map sort))]
       (reduce + 0 (map difference a b))))))

(comment
  (solve-a sample-input)
  (solve-b sample-input)
  )

(defn solve-b
  ([] (solve-b problem-input))
  ([input]
   (letfn [(pointwise-conj [colls tuple]
             (map conj colls tuple))]
     (let [[a b] (->> (str/split-lines input)
                      (map parse-a)
                      (reduce pointwise-conj [[] []]))
           freqs (frequencies b)]
       (transduce (map (fn [x] (* x (freqs x 0))))
                  + 0 a)))))

(defn solve [{:keys [input]}]
  (let [input (if input (slurp input) problem-input)]
    (time (println (solve-a input)))
    (time (println (solve-b input)))))

(comment
  (solve-a)
  (solve-b)
  )
