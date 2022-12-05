(ns day03
  (:require
    [clojure.string :as str]
    [clojure.set :as set]))

(def sample-input
  "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(def problem-input (slurp "src/day03.txt"))

(def priority
  (letfn [(letters [start end]
            (map char (range (int start) (inc (int end)))))]
    (zipmap (concat (letters \a \z) (letters \A \Z)) (next (range)))))

(defn buckets [row]
  (let [c (count row)]
    (map set (split-at (/ c 2) row))))

(defn solve-a
  ([] (solve-a problem-input))
  ([input] (transduce (map (fn [line]
                             (->> line
                                  buckets
                                  (apply set/intersection)
                                  first
                                  priority)))
                      +
                      0
                      (str/split-lines input))))

(defn solve-b
  ([] (solve-b problem-input))
  ([input] (transduce (comp (map set)
                            (partition-all 3)
                            (map (comp first (partial apply set/intersection)))
                            (map priority))
                      +
                      0
                      (str/split-lines input))))

(defn solve [{:keys [input]}]
  (let [input (if input (slurp input) problem-input)]
    (time (println (solve-a input)))
    (time (println (solve-b input)))))
