(ns advent.day13
  (:require [clojure.string :as str]
            [advent.day13data :as data]
            [clojure.test :refer [deftest is are testing]]))

(defn severity [[depth span]]
  (* depth span))

(defn collision? [delay [depth span]]
  (= (mod (+ depth delay) (* 2 (dec span))) 0))

(defn solve1
  ([] (solve1 data/data))
  ([scanners] (->> scanners
                   (filter (partial collision? 0))
                   (map severity)
                   (apply +))))

(defn caught? [delay-time] (->> sample
                             (filter (partial collision? delay-time))
                             empty?))

(defn solve2
  ([] (solve2 data/data))
  ([scanners]
   (let [evaded? (fn [delay-time] (->> scanners
                                       (filter (partial collision? delay-time))
                                       empty?))
         hypos (map (juxt evaded? identity) (range))]
     (some (fn [[evades delayed-time]]
             (if (= 0 (mod delayed-time 1000000)) (prn delayed-time))
             (when evades  delayed-time))
           hypos))))
