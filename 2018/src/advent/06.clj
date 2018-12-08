(ns advent.06
  (:require [clojure.string :as str]
            [advent.06data :as data]
            [clojure.set :as set]))

(defn << [coll] (apply min coll))
(defn >> [coll] (apply max coll))

(defn bounding-borders [points]
  (let [[left right] ((juxt << >>) (map first points))
        [top bottom] ((juxt << >>) (map second points))]
    [top left bottom right]))

(defn metric [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x2 x1))
     (Math/abs (- y2 y1))))

(defn cell-owners [square cells]
  (letfn [(cell-distances [square cells]
            (reduce (fn [distances cell]
                      (update distances (metric square cell) (fnil conj #{}) cell))
                    (sorted-map-by <)
                    cells))]
    (val (first (cell-distances square cells)))))

(def test-data
  [[1, 1] ;a
   [1, 6] ;b
   [8, 3] ;c
   [3, 4] ;d
   [5, 5] ;e
   [8, 9]];f
  )

(defn solve1
  ([] (solve1 data/data))
  ([cells]
   (let [[top left bottom right] (bounding-borders cells)
         owners                  (for [y (range top (inc bottom))
                                       x (range left (inc right))]
                                   (let [owners (cell-owners [x y] cells)]
                                     (if (> (count owners) 1)
                                       :many
                                       (first owners))))
         rows                    (partition-all (inc (- right left)) owners)
         bounding-box            (concat (first rows)
                                         (last rows)
                                         (map first rows)
                                         (map last rows))
         ;; assumption that if you touch the bounding box you will
         ;; continue infinitely
         infinite-areas          (into #{}
                                       (remove #{:many})
                                       bounding-box)]
     (->> (apply concat rows)
          frequencies
          (sort-by second >)
          (remove (comp (conj infinite-areas :many) first))
          first
          val))))

(defn grid [[top left bottom right]]
  (for [y (range top (inc bottom))
        x (range left (inc right))]
    [x y]))

(defn cell->distances [cell cells]
  (reduce + 0 (map (partial metric cell) cells)))

(defn solve2
  ([] (solve2 10000 data/data))
  ([distance-limit points]
   ;; this assumes that the "region" is contiguous which seems weird.
   (->> (bounding-borders points)
        grid
        (map #(cell->distances % points))
        (filter #(< % distance-limit))
        count)))


;; rather than find the count of cells under the threshold i look for
;; contiguous regions satisfying such constraint. It appears that the
;; data in the challenge satisfies this but it does not state that
;; this can be assumed. This enumerates all such contiguous regions
;; and displays their counts in descending order.

(defn make-graph
  [grid cells]
  (letfn [(distance [square] (cell->distances square cells))]
    (loop [graph {}
           grid  grid]
      (if-let [square (first grid)]
        (recur (assoc graph square (distance square)) (rest grid))
        graph))))

(defn neighbors [[x y]]

  [           [x (dec y)]
   [(dec x) y]           [(inc x) y]
              [x (inc y)]           ])

(defn enumerate-regions [grid]
  (letfn [(adjacent-in [grid node]
            (into #{}
                  (filter grid)
                  (neighbors node)))
          (random-node [grid] (ffirst grid))]
    (let [start (random-node grid)]
      (loop [grid    (dissoc grid start)
             check   (adjacent-in grid start)
             region  #{start}
             regions #{}]
        (cond (seq check)
              (let [node (first check)]
                (recur (dissoc grid node)
                       (set/union (disj check node) (adjacent-in grid node))
                       (conj region node)
                       regions))

              (seq grid)
              (let [new-start (random-node grid)]
                (recur (dissoc grid new-start)
                       (adjacent-in grid new-start)
                       #{new-start}
                       (conj regions region)))
              :else
              (conj regions region))))))

(defn solve2'
  ([] (solve2 10000 data/data))
  ([distance-limit data]
   (->> (make-graph (grid (bounding-borders data))
                    data)
        (into {}
              (filter (fn [[k v]] (< v distance-limit))))
        enumerate-regions
        (map count)
        sort)))
