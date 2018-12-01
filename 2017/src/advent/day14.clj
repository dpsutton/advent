(ns advent.day14
  (:require [clojure.string :as str]
            #_[advent.day14data :as data]
            [clojure.test :refer [deftest is are testing]]
            [clojure.set :as set]
            [advent.day10 :as knot]))

(def data "hfdlxzhv")

(defn hex->binary [hex]
  (as-> (Integer/toBinaryString (Integer/parseInt (str hex) 16)) <>
    (reverse <>)
    (concat <> (take 4 (repeat "0")))
    (take 4 <>)
    (reverse <>)
    (map #(Integer/parseInt (str %)) <>)))

(defn hash->bits [hash]
  (->> hash
       str
       (map hex->binary)
       (apply concat)))

(defn hash->sum-of-ones [hash]
  (reduce + (hash->bits hash)))

(defn adjoin-row [input n]
  (str input "-" n))

(defn solve1
  ([] (solve1 data))
  ([input] (->> (range 128)
                (map (partial adjoin-row input))
                (map knot/knot-hash)
                (map hash->sum-of-ones)
                (reduce +))))

(defn hashes [input]
  (->> (range 128)
       (map (partial adjoin-row input))
       (map knot/knot-hash)))

(defn hash->grid-row [row-number hash]
  (reduce (fn [row [bit index]]
            (if (= 1 bit)
              (assoc row [row-number index] true)
              row))
          {}
          (map vector
               (hash->bits hash)
               (range))))

(defn hash->grid [input]
  (apply merge (map hash->grid-row (range) (hashes input))))

(defn neighbors [[x y]]
  [           [x (dec y)]
   [(dec x) y]           [(inc x) y]
              [x (inc y)]           ])

(defn find-contiguous [cell grid']
  (loop [grid grid'
         group #{}
         cells #{cell}]
    (if-let [cell (first cells)]
      (let [neighbors (->> (neighbors cell)
                           (filter (partial contains? grid))
                           set)]
        (recur (apply dissoc grid cell neighbors)
               (conj group cell)
               (-> cells
                   (set/union neighbors)
                   (set/difference #{cell}))))
      [grid group])))

(defn find-groups [grid']
  (loop [grid grid'
         groups #{}]
    (if-let [[cell _] (first grid)]
      (let [[new-grid group] (find-contiguous cell grid)]
        (assert (not= (count grid) (count new-grid))
                (str "didn't decrease grid size: " cell))
        (recur new-grid (conj groups group)))
      groups)))

(defn solve2
 ([] (solve2 data))
 ([input] (count (find-groups (hash->grid input)))))

