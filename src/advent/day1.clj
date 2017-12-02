(ns advent.day1
  (:require [clojure.test :refer [deftest is testing are]]))

(defn digit->seq
  [d]
  (->> d
       str
       (map #(Integer/parseInt (str %)))))

(defn captcha2 [d]
  (let [original           (digit->seq d)
        size               (count original)
        permuted           (->> original cycle (drop (quot size 2)))
        joined             (map vector original permuted)
        next-to-duplicates (->> joined
                                (filter (fn [pair] (apply = pair)))
                                (map first))]
    (reduce + next-to-duplicates)))

(deftest captcha2-tests
  (testing captcha2
    (are [input _ output] (= (captcha2 input) output)
      1212     --> 6
      1221     --> 0
      123425   --> 4
      123123   --> 12
      12131415 --> 4)))
