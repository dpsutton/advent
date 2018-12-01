(ns advent.day11
  (:require [clojure.string :as str]
            [advent.day11data :as data]
            [clojure.test :refer [deftest is are testing]]))

;;; Free (Abelian) Group
;; a free group is just a list of characters along with a few
;; relations over those characters. We are taking the free group over
;; :n :s :ne :nw :se :sw.

;; we know the inverses of each element is just "back where you came
;; from"
(def equivalences {[:ne :sw] []
                   [:n :s]   []
                   [:nw :se] []})

;; and we can simplify a few things like moving south east and then
;; north is just moving north east
(def simplifications {[:se :n]  [:ne]
                      [:sw :n]  [:nw]
                      [:s :ne]  [:se]
                      [:nw :ne] [:n]
                      [:sw :se] [:s]
                      [:nw :s]  [:sw] })

;; since we are in an abelian free group, it doesn't matter what order
;; the symbols are in. If we find a pair that can collapse to the one
;; or no symbols, we will apply that transformation
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

;; we actually don't need to worry about fixed points since we do the
;; simplifications first and then the equivalences. In other words, we
;; collapse two elements into one as much as we can. And then we loop
;; over the collection removing all annihilating pairs (eg, north and
;; south). Once this pass has been done there can be no more
;; optimizations as we do not introduce any other symbols that could
;; create new relations
(defn iterate-to-fixed-point
  [input relations]
  (let [r (keys relations)]
    (loop [bag input]
      (if-let [reduction (some (partial has-pair bag) r)]
        (recur (replace-pair bag reduction (get relations reduction)))
        bag))))

;; so we apply all simplifications and then combine all annihilators.
(defn simplified-path [path]
  (-> path
      (iterate-to-fixed-point simplifications)
      (iterate-to-fixed-point equivalences)))

;; since we have simplified the path to only "meaningful" movements,
;; the distance we are aware is simply the length of the path after
;; all simplifications
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
