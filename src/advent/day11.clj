(ns advent.day11
  (:require [clojure.string :as str]
            [advent.day11data :as data]
            [clojure.test :refer [deftest is are testing]]))

(def equivalences {[:ne :sw] []
                   [:n :s]   []
                   [:nw :se] []})

(def simplifications {[:se :n]  [:ne]
                      [:sw :n]  [:nw]
                      [:s :ne]  [:se]
                      [:nw :ne] [:n]
                      [:sw :se] [:s]
                      [:nw :s]  [:sw] })

(defn has-pair [input [x y]]
  (when (and (some #{x} input)
             (some #{y} input))
    [x y]))

(defn without [input x]
  (let [[before after] (split-with (partial not= x) input)]
    (concat before (drop 1 after))))

(defn replace-pair [input [x y] replacement]
  (let [without-x    (without input x)
        without-both (without without-x y)]
    (concat without-both replacement)))

(defn iterate-to-fixed-point
  [input relations]
  (let [r (keys relations)]
    (loop [bag input]
      (if-let [reduction (some (partial has-pair bag) r)]
        (recur (replace-pair bag reduction (get relations reduction)))
        bag))))

(defn simplified-path [path]
  (-> path
      (iterate-to-fixed-point simplifications)
      (iterate-to-fixed-point equivalences)))

(defn solve1
  ([] (solve1 data/data))
  ([directions]
   (count (simplified-path directions))))

(deftest solve1-tests
  (are [directions length] (= (solve1 directions) length)
    [:ne :ne :ne] 3
    [:ne :ne :sw :sw] 0
    [:ne :ne :s :s] 2
    [:se :sw :se :sw :sw] 3))

(defn solve2
  ([] (solve2 data/data))
  ([directions]
   (let [paths-and-distances (reductions (fn [[minified distance] new-direction]
                                           (let [net-path (simplified-path (conj minified new-direction))]
                                             [net-path (count net-path)]))
                                         [[] 0]
                                         directions)]
     (apply max (map second paths-and-distances)))))
