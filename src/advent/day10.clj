(ns advent.day10
  (:require [clojure.string :as str]))

(def terminating-bytes [17, 31, 73, 47, 23])
(def data [199,0,255,136,174,254,227,16,51,85,1,2,22,17,7,192])
(def data-string "199,0,255,136,174,254,227,16,51,85,1,2,22,17,7,192")

(defn apply-transform [data [transform skip]]
  (let [cycled   (cycle data)
        reversed (->> cycled
                      (take transform)
                      reverse)]
    (->> data
         (drop transform)
         (concat reversed)
         cycle
         (drop (+ transform skip))
         (take (count data)))))

(defn fast-forward-amount [size-of-list transforms]
  (let [skips (reduce (fn [acc [t skip]] (+ acc t skip))
                      0
                      (map vector transforms (range)))]
    (- size-of-list (mod skips size-of-list))))

(defn recenter [arr transforms]
  (let [size (count arr)]
    (->> arr
         cycle
         (drop (fast-forward-amount size transforms))
         (take size))))

(defn solve1
  ([] (solve1 (range 0 256) data))
  ([arr transforms]
   (let [transformed (reduce apply-transform arr (map vector transforms (range)))
         recentered  (recenter transformed transforms)]
     (* (first recentered) (second recentered)))))

(defn dense-hash [arr]
  (map (fn [partition] (apply bit-xor partition))
       (partition-all 16 arr)))

(defn dense-hash->str
  [d-hash]
  (str/join (map (fn [h]
                   (let [s (Integer/toString h 16)]
                     (if (= 1 (count s))
                       (str "0" s)
                       s)))
                 d-hash)))

(defn solve2
  ([] (solve2 data-string))
  ([input]
   (let [transforms   (apply concat (take 64 (repeat (concat (map int input) terminating-bytes))))
         transformed  (reduce apply-transform (range 0 256) (map vector transforms (range)))]
     (-> transformed
         (recenter transforms)
         dense-hash
         dense-hash->str))))
