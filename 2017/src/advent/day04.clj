(ns advent.day04
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing are]]
            [advent.day04data :as data]))

(defn are-unique? [things]
  (->> things frequencies vals (every? #{1})))

(defn valid-passphrase? [phrase]
  (->> phrase
   (#(str/split % #"\s+")) 
   are-unique?))

(deftest valid-passphrase-tests
  (are [phrase valid?] (= (valid-passphrase? phrase) valid?)
    "aa bb cc dd ee" true
    "aa bb cc dd aa" false))

(defn solve-part1
  ([] (solve-part1 data/data))
  ([phrases] (->> phrases
                  (filter valid-passphrase?)
                  count)))

(defn valid-no-rearrangement? [phrase]
  (and (valid-passphrase? phrase)
       (->> phrase
            (#(str/split % #"\s"))
            (map sort)
            are-unique?)))

(defn solve-part2
  ([] (solve-part2 data/data))
  ([phrases] (->> phrases
                  (filter valid-no-rearrangement?)
                  count)))
