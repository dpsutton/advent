(ns advent.day3
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing are]]))

(def odd-numbers (filter odd? (range)))
(def bottom-right-corners (->> odd-numbers
                               (map #(* % %))))

(def rings-and-corners (map vector (range) bottom-right-corners))

(defn elements-per-side [ring] (nth odd-numbers ring))

(defn ring-midpoints [n]
  (if (= n 0) '(1 1 1 1)
      (let [side-length (elements-per-side n)
            starting-point (inc (nth bottom-right-corners (dec n)))
            initial-midpoint (dec (+ starting-point (quot side-length 2)))]
        (take 4 (iterate #(+ (dec side-length) %) initial-midpoint)))))

(defn which-ring [corners n]
  (let [[ring max-number] (first (filter (fn [[ring max-number]]
                                           (<= n max-number))
                                         corners))]
    ring))

(defn distance-from-middle [midpoints n]
  (apply min (map #(Math/abs (- % n)) midpoints)))

(defn manhattan-metric [x y] (+ x y))

(defn steps-from [metric n]
  (let [ring (which-ring rings-and-corners n)
        midpoints (ring-midpoints ring)
        distance-from-middle (distance-from-middle midpoints n)]
    ;; the taxicab or manhattan metric is determined by how far in one
    ;; direction and then how far in the other. this is how many rings
    ;; out and then how far from the middle
    (metric ring distance-from-middle)))

(deftest steps-from-test
  (testing "with manhattan metric"
    (are [square steps] (= (steps-from manhattan-metric square) steps)
      1    0
      12   3
      23   2
      1024 31)))

(comment
  ;; part a
  (steps-from manhattan-metric 312051)
  ;; 430
  )

(def side-ranges (cons [1 1] (map vector (map inc bottom-right-corners) (drop 1 bottom-right-corners))))

(defn sides-for-ring [ring]
  (if (= ring 0) (take 4 (repeat '(1)))
      (let [[low high] (nth side-ranges ring)
            side       (elements-per-side ring)]
        (partition side (dec side) (cons high (range low (inc high)))))))

(defn sides [n])

(defn relevant-sides [n]
  (if (= n 1) [[1] []]
      (let [ring  (which-ring rings-and-corners n)
            paired-sides (map vector (sides-for-ring ring) (sides-for-ring (dec ring)))]
        (some (fn [[outer inner]] (when (some #{n} outer)
                                  [outer inner]))
              paired-sides))))

(defn inner-touching [n]
  (let [[outer inner]  (relevant-sides n)
        pad            [:none :none]
        inner          (concat pad inner pad)
        touching-pairs (map vector outer (partition 3 1 inner))]
    (some (fn [[cell adjacent]] (when (= cell n)
                                  (distinct (cons (dec n) (remove #{:none} adjacent)))))
          touching-pairs)))

(defn outer-touching [n]
  (if (= n 1)
    []
    (let [ring               (which-ring rings-and-corners n)
          sides              (sides-for-ring ring)
          near-corner        (map second (drop 1 sides))
          touching-in-corner (when (some #{n} near-corner)
                               [(- n 2)])
          touches-first      (let [last-side (last sides)
                                   last-two (take 2 (reverse last-side))]
                               (when (some #{n} last-two)
                                 [(second (first sides))]))]
      (concat [(dec n)] touching-in-corner touches-first))))

(defn cell-value [n]
  (if (= n 1)
    1
    (let [inner (inner-touching n)
          outer (outer-touching n)
          touching (distinct (concat inner outer))]
      (apply + (map cell-value touching)))))

(deftest cell-value-tests
  (are [cell value] (= (cell-value cell) value)
    1 1
    2 1
    3 2
    4 4
    5 5
    6 10
    7 11
    8 23
    9 25
    10 26
    11 54))

(comment
  (let [f (memoize cell-value)]
    (first (drop-while (fn [cell]
                         (< (f cell) 312051))
                       (drop 1 (range)))))
  ;; => 61
  (cell-value 61)
  ;; => 312453
  )
