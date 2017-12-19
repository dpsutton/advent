(ns advent.day8
  (:require [clojure.string :as str]
            [advent.day8data :as data]))

(defn target-register [statement] (first statement))

(def != not=)

(defn satisfied? [machine statement]
  (let [[register test threshold] (drop 4 statement)
        current-value (get machine register 0)]
    ((resolve test) current-value threshold)))

(defn apply-statement [machine statement]
  (let [[register op value] (take 3 statement)]
    (update machine register
            (fnil (case op
                    inc +
                    dec -) 0)
            value)))

(defn process-statement [machine statement]
  (if (satisfied? machine statement)
    (apply-statement machine statement)
    machine))

(defn largest-register-value [machine]
  (apply max (vals machine)))

(defn solve1
  ([] (solve1 data/data))
  ([instructions] (let [final-state (reduce process-statement
                                            {}
                                            instructions)]
                    (largest-register-value final-state))))

(defn solve2
  ([] (solve2 data/data))
  ([instructions]
   (let [states (reductions process-statement
                            {}
                            instructions)
         ;; reductions includes the initial state {} which has no
         ;; values for the max
         max-values (map largest-register-value (drop 1 states))]
     (apply max max-values))))
