(ns advent.day17
  (:require [clojure.string :as str]))

(def initial-spinlock {:head 0
                       :values '(0)
                       :size   1})

(defn insert-spinlock [advance spinlock new-val]
  (let [{:keys [head values size]} spinlock
        insert-point               (inc (mod (+ head advance) size))
        [before after]             (split-at insert-point values)]
    {:head insert-point
     :values (apply conj (vec before) new-val (vec after))
     :size (inc size)}))

(def data 314)

(defn solve1
  ([] (solve1 data))
  ([advance]
   (let [spinlock (reduce (fn [spin v]
                            (insert-spinlock advance spin v))
                          initial-spinlock
                          (range 1 2018))
         [before at-after] (split-with (partial not= 2017) (:values spinlock))]
     (second at-after))))

(defn insert-point [^long advance ^long position ^long size]
  (inc (mod (+ position advance) size)))

(defn solve2
  ([] (solve2 data (int 50e6)))
  ([advance ^long iterations]
   (loop [zero-position 0
          after-zero    0
          position      0
          times         0
          size          1]
     (if (= iterations times)
       after-zero
       (let [insertion-point (insert-point advance position size)]
         (recur (if (< ^long insertion-point zero-position)
                  ^long (inc zero-position)
                  zero-position)
                (if (= insertion-point (inc zero-position))
                   size ;; inserting value is equal to size
                  after-zero)
                insertion-point
                (inc times)
                (inc size)))))))
