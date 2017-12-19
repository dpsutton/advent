(ns advent.day05
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing are]]
            [advent.day05data :as data]))

(defn run-machine [incrementer machine]
  (loop [machine    machine
         register   1
         operations 0]
    (let [value (get machine register ::escape)]
      (if (= value ::escape)
        operations
        (recur (update machine register incrementer)
               (+ register value)
               (inc operations))))))

(def simple-instructions (->> [0 3 0 1 -3]
                              (zipmap (drop 1 (range)))))

(defn solve1
  ([] (solve1 data/data))
  ([machine] (run-machine inc machine)))

(deftest solve1-tests
  (is (= 5 (solve1 simple-instructions))))

(defn solve2
  ([] (solve2 data/data))
  ([machine] (run-machine (fn [register] (if (>= register 3)
                                           (dec register)
                                           (inc register)))
                          machine)))


