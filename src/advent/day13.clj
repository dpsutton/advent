(ns advent.day13
  (:require [clojure.string :as str]
            [advent.day13data :as data]
            [clojure.test :refer [deftest is are testing]]))

(defn severity [[depth span]]
  (* depth span))

;; this is bad and stupid and makes part two take hours instead of ten seconds
;; (defn scanner-position [span time]
;;   (nth (cycle (concat (range span) (reverse (range 1 (dec span))))) time))

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
             (when evades  delayed-time))
           hypos))))

(defn first-multiple-above [multiple threshold]
  (->> (range)
       (map (partial * multiple))
       (filter (partial <= threshold))
       first))

(defn sieve-method [scanners]
  (loop [scanners        scanners
         possible-delays (range)]
    (if (seq scanners)
      (let [[distance span]             (first scanners)
            period                      (* 2 (dec span))
            first-chance-to-collide     (first-multiple-above period distance)
            delay-for-first-chance      (- first-chance-to-collide distance)
            delays-arriving-at-bad-time (fn [delay-time] (= delay-for-first-chance (mod delay-time period)))]
        (recur (rest scanners) (remove delays-arriving-at-bad-time possible-delays)))
      (first possible-delays))))

(comment
  (time (solve2))
  ;; "Elapsed time: 10507.820736 msecs"
  ;; 3870382
  (dotimes [_ 20] (time (sieve-method data/data)))
  ;; "Elapsed time: 1293.195798 msecs"
  ;; 3870382
  )
