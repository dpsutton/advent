(ns advent.03
  (:require
   [advent.utils :as utils]
   [clojure.set :as set]
   [clojure.string :as str]))

(defn parse-line
  [directions-string]
  (let [translate {\R :right \L :left \D :down \U :up}]
    (map (fn [dir]
           [(translate (first dir)) (Long/parseLong (subs dir 1))])
         (str/split directions-string #","))))

(defn enumerate-positions
  "Assume we start at [0,0] and enumerate all board positions."
  [directions]
  (let [travel (fn [[x y :as pos] direction length]
                 (let [moves (range 1 (inc length))]
                   (reduce (fn [[_ path] delta]
                             (let [new-pos [(+ x (case direction
                                                   :left (- delta)
                                                   :right delta
                                                   0))
                                            (+ y (case direction
                                                   :up delta
                                                   :down (- delta)
                                                   0))]]
                               [new-pos (conj path new-pos)]))
                           [pos []]
                           moves)))]
    (->> directions
         (reduce (fn [{:keys [pos] :as state} [direction length]]
                   (let [[new-pos moves] (travel pos direction length)]
                     (-> state
                         (assoc :pos new-pos)
                         (update :path into moves))))
                 {:pos  [0 0]
                  :path []})
         :path)))

(defn metric [[x y]] (+ (Math/abs x) (Math/abs y)))

(def input
  (delay
    (utils/input "03")))

(def input->points (comp enumerate-positions parse-line))

(defn solve-1
  ([]
   (apply solve-1 @input))
  ([line1 line2]
   (let [f (comp set input->points)]
     (->> (set/intersection (f line1)
                            (f line2))
          (map metric)
          (sort)
          (first)))))

(defn solve-2
  ([]
   (apply solve-2 @input))
  ([line1 line2]
   (let [f (fn [m [x idx]] (if (contains? m x) m (assoc m x idx)))
         paths-1 (input->points line1)
         paths-2 (input->points line2)
         m1 (reduce f {} (map vector paths-1 (rest (range))))
         m2 (reduce f {} (map vector paths-2 (rest (range))))
         sum-of-distances #(+ (m1 %) (m2 %))]
     (->> (set/intersection (set paths-1) (set paths-2))
          (map sum-of-distances)
          (sort)
          (first)))))

(comment

  (solve-2 "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83")
  (let [paths-1 (->> "R75,D30,R83,U83,L12,D49,R71,U7,L72"
                     parse-line
                     enumerate-positions
                     set)
        paths-2 (->> "U62,R66,U55,R34,D71,R55,D58,R83"
                     parse-line
                     enumerate-positions
                     set)]
    (sort-by metric (set/intersection paths-1 paths-2)))
  )
