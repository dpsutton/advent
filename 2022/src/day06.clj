(ns day06
  (:require
    [clojure.string :as str]))

(def sample-input "mjqjpqmgbljsphdztnvjfqwrcgsmlb")

(def problem-input (slurp "src/day06.txt"))

(defn solve* [n input]
  (->> input
       (partition n 1)
       (map vector (drop n (range)))
       (filter (fn [[_i vs]]
                 (= vs (distinct vs))))
       ffirst))

(defn solve-a
  ([] (solve-a problem-input))
  ([input]
   (solve* 4 input)))

(defn solve-b
  ([] (solve-b problem-input))
  ([input]
   (solve* 14 input)))

(defn solve [{:keys [input]}]
  (let [input (if input (slurp input) problem-input)]
    (time (println (solve-a input)))
    (time (println (solve-b input)))))

(comment
  (solve {})
  (solve-a sample-input)
  (solve-b sample-input)
  )

(defprotocol RB
  (different? [rb])
  (rb-add [rb x])
  (index [rb]))

(deftype RingBuffer [n ^:volatile-mutable cnt arr]
  RB
  (different? [_] (and (>= (count arr) n)
                       (= (seq arr) (distinct arr))))
  (rb-add [rb x]
    (locking arr
      (aset-long arr (mod (inc cnt) n) x)
      (set! cnt (inc cnt))
      rb))
  (index [_] cnt))

(defn ring-buffer [n] (RingBuffer. n 0 (long-array n)))

(defn alternate-solve [n input]
  (transduce identity
             (fn
               ([] (ring-buffer n))
               ([rb] (index rb))
               ([rb c]
                (let [rb (rb-add rb c)]
                  (if (different? rb)
                    (reduced rb)
                    rb))))
             input))

(comment
  (time (solve-b problem-input))
  (time
    (alternate-solve 14 problem-input))

  (def super-long (slurp "src/super-long.txt"))
  (time (solve-b super-long))
  (let [window 14
        input super-long]
    (time (alternate-solve window input)))
  (spit "src/super-long.txt"
        (str (apply str (repeat 400000 "a")) "wxya")))
