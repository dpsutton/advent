(ns advent.day6
  (:require [advent.day6data :as data]))

(defn sieve [s]
  (cons (first s)
        (lazy-seq (sieve (filter #(not= 0 (mod % (first s)))
                                 (rest s))))))

(def primes (sieve (iterate inc 2)))

(defn signature [arr]
  (apply + (map #(Math/pow %1 %2) primes arr)))

(defn redistribute [memory]
  (let [blocks-and-size (map-indexed (fn [id blocks] [id blocks]) memory)
        [id blocks]     (apply max-key second (reverse blocks-and-size))
        start           (inc id)
        memory-size     (count memory)]
    (mapv + (assoc memory id 0)
          (reduce (fn [diff-blocks offset]
                    (update diff-blocks (mod (+ start offset) memory-size) inc))
                  (vec (take memory-size (repeat 0)))
                  (range blocks)))))

(defn solve
  ([] (solve data/data))
  ([arr] (loop [memory arr
                seen   {}
                steps  0]
           (if (seen memory)
             [steps [memory (seen memory)]]
             (recur (redistribute memory) (assoc seen memory steps) (inc steps))))))

(defn solve1 [] (first (solve)))
(defn solve2 [] (let [[seen-again [memory seen-first]] (solve)]
                  (- seen-again seen-first)))

(comment
  (solve [0 2 7 0])
  (solve)
  
  )


