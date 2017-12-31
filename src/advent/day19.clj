(ns advent.day19
  (:require #_[advent.day18data :as data]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.java.io :as io]))

(def test-graph ["    |           "
                 "    |  +--+     "
                 "    A  |  C     "
                 "F---|----E|--+  "
                 "    |  |  |  D  "
                 "    +B-+  +--+  "])

(defn parse-node [node]
  (case node
    \| 'vertical
    \- 'horizontal
    \+ 'junction
    \  'none
    (str node)))

(def nodes (for [x (range (count (first test-graph)))
                 y (range (count test-graph))]
             [[x y] (parse-node (nth (nth test-graph y) x))]))

(def data
  (let [input  (io/reader (io/resource "advent/day19.txt"))
        lines  (line-seq input)
        length (apply max (map count lines))
        ;; autowhitespace cleanup trims lines.
        lines  (map #(take length (concat % (repeat \ ))) lines)
        nodes  (for [x (range (count (first lines)))
                     y (range (count lines))]
                 [[x y] (parse-node (nth (nth lines y) x))])]
    (->> nodes
         (filter (fn [[_ v]] (not= v 'none)))
         (into {}))))

(def graph
  (->> nodes
       (filter (fn [[_ v]] (not= v 'none)))
       (into {})))

(defn entrance [g]
  (->> g
       (filter (fn [[[_ row] v]] (and (= row 0) (= v 'vertical))))
       first))

(defn opposite-direction [direction]
  (case direction
    up    'down
    down  'up
    left  'right
    right 'left))

(defn directions-except [direction]
  (set/difference #{'up 'down 'left 'right}
                  #{direction}))

(defn crossing? [v1 v2]
  (= #{v1 v2} #{'vertical 'horizontal}))

(defn next-cell [g [col row] direction]
  (case direction
    up    [col (dec row)]
    down  [col (inc row)]
    left  [(dec col) row]
    right [(inc col) row]))

(defn new-direction [g previous-cell cell direction]
  (let [v1 (g previous-cell)
        v2 (g cell)]
    (if (= v2 'junction)
      ;; only turn at junction
      (let [possible-directions  (directions-except (opposite-direction direction))
            directions (filter (fn [d] (contains? g (next-cell g cell d))) possible-directions)]
        (when (empty? directions)
          (throw (ex-info "no valid directions!"
                          {:previous-cell previous-cell
                           :p-cell-value v1
                           :cell cell
                           :cell-vale v2
                           :possible-directions possible-directions})))
        (first directions))
      direction)))

(defn travel [g]
  (fn [[cell direction letters]]
    (let [next-coords     (next-cell g cell direction)
          next-cell-value (g next-coords)]
      (if next-cell-value
        [next-coords (new-direction g cell next-coords direction) (if (string? next-cell-value)
                                                                    (conj letters next-cell-value)
                                                                    letters)]
        :finished))))

(defn travels [g]
  (let [[entrance-coords v] (entrance g)
        travels (iterate (travel g) [entrance-coords 'down []])]
    (take-while #(not= % :finished) travels)))

(defn solve1
  ([] (solve1 data))
  ([g] (let [steps (travels g)]
         (apply str (last (last steps))))))

(defn solve2
  ([] (solve2 data))
  ([g] (let [steps (travels g)]
         (count steps))))
