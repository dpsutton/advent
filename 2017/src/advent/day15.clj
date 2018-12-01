(ns advent.day15
  (:require [clojure.string :as str]
            #_[advent.day15data :as data]
            [clojure.test :refer [deftest is are testing]]
            [clojure.set :as set]))

;; Integer/MAX_VALUE
(def divisor 2147483647)

(let [low-16-bitmask (int (dec (Math/pow 2 16)))]
  (defn lower-16-bits [n]
    (bit-and n low-16-bitmask)))

(defn match? [x y]
  (= (lower-16-bits x)
     (lower-16-bits y)))

(def million (int 1e6))

(defn multiple-of [n]
  (fn [x]
    (= 0 (mod x n))))

(defn generations [{:keys [factor seed multiples-of]}]
  (let [matching? (multiple-of multiples-of)]
    (->> seed
         (iterate (fn [previous]
                    (rem (* previous factor)
                         divisor)))
         (drop 1)
         (filter matching?))))

(def sample-generator-a {:factor 16807
                         :seed 65
                         :multiples-of 4})
(def sample-generator-b {:factor 48271
                         :seed 8921
                         :multiples-of 8})

(def generator-a {:factor 16807
                  :seed 873
                  :multiples-of 4})
(def generator-b {:factor 48271
                  :seed 583
                  :multiples-of 8})

(defn solve
  [g1 g2 iterations]
  (reduce (fn [match-count [gen1 gen2]]
            (if (match? gen1 gen2)
              (inc match-count)
              match-count))
          0
          (map vector
               (take iterations (generations g1))
               (take iterations (generations g2)))))

(defn solve1
  ([] (solve1 generator-a generator-b (* 40 million)))
  ([ga gb iterations]
   (solve (assoc ga :multiples-of 1)
          (assoc gb :multiples-of 1)
          iterations)))

(defn solve2
  ([] (solve2 generator-a generator-b (* 5 million)))
  ([ga gb iterations] (solve ga gb iterations)))
