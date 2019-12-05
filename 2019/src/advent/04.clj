(ns advent.04
  (:refer-clojure :exclude [==])
  (:require
   [clojure.core.logic.fd :as fd]
   [clojure.core.logic :as l :refer [run fresh == run*]]
   [clojure.core.logic.nominal]
   [clojure.string :as str]))

(defn monotonico [[x1 x2 x3 x4 x5 x6]]
  (l/and* [(fd/<= x1 x2)
           (fd/<= x2 x3)
           (fd/<= x3 x4)
           (fd/<= x4 x5)
           (fd/<= x5 x6)]))

(defn one-repeato [[x1 x2 x3 x4 x5 x6]]
  (l/conde
    [(== x1 x2)]
    [(== x2 x3)]
    [(== x3 x4)]
    [(== x4 x5)]
    [(== x5 x6)]))

(defn one-repeat-no-group-o [[x1 x2 x3 x4 x5 x6]]
  (l/conde
    [             (== x1 x2) (l/!= x2 x3)]
    [(l/!= x1 x2) (== x2 x3) (l/!= x3 x4)]
    [(l/!= x2 x3) (== x3 x4) (l/!= x4 x5)]
    [(l/!= x3 x4) (== x4 x5) (l/!= x5 x6)]
    [(l/!= x4 x5) (== x5 x6)             ]))

(defn solve-1
  ([] (solve-1 387638 919123))
  ([lower upper]
   (let [candidates (let [vars (repeatedly 6 l/lvar)]
                      (run* [q]
                        (== q vars)
                        (l/everyg #(fd/in % (fd/interval 0 9)) vars)
                        (monotonico vars)
                        (one-repeato vars)))]
     (->> candidates
          (map (comp #(Long/parseLong %) str/join))
          (filter #(<= lower % upper))
          distinct
          sort
          count))))

(defn solve-2
  ([] (solve-2 387638 919123))
  ([lower upper]
   (let [candidates (let [vars (repeatedly 6 l/lvar)]
                      (run* [q]
                        (== q vars)
                        (l/everyg #(fd/in % (fd/interval 0 9)) vars)
                        (monotonico vars)
                        (one-repeat-no-group-o vars)))]
     (->> candidates
          (map (comp #(Long/parseLong %) str/join))
          (filter #(<= lower % upper))
          distinct
          sort
          count))))

(defn one-adjacent [digits]
  (->> digits (partition-by identity) (filter #(> (count %) 1)) seq))

(defn increasing? [digits]
  (let [less-than-or-eq (fn [[a b]] (not (pos? (compare a b))))]
    (every? less-than-or-eq (map vector digits (rest digits)))))

(defn solve-1-no-logic
  ([] (solve-1-no-logic 387638 919123))
  ([lower upper]
   (->> (range lower upper)
        (map str)
        (filter (every-pred one-adjacent increasing?))
        (map #(Long/parseLong %))
        count)))
