(ns advent.03
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def data
  (delay (->> "src/advent/03.data"
              io/reader
              line-seq)))

(def test-data
  ["#1 @ 1,3: 4x4"
   "#2 @ 3,1: 4x4"
   "#3 @ 5,5: 2x2"])

(defn remove-all [s chars]
  (reduce #(str/replace %1 %2 " ") s chars))

(defn cells [{:keys [left top width height]}]
  (for [x (range left (+ left width))
        y (range top (+ top height))]
    [x y]))

(defn parse-line [line]
  (letfn [(chop [s] (str/split s #" "))
          (clean [s] (remove-all s ["#" "@" "," ":" "x"]))
          (tokens [ts]  (map #(Integer/parseInt %) (remove str/blank? ts)))
          ]
    (zipmap [:id :left :top :width :height]
            (-> line clean chop tokens))))

(defn process-input [input]
  (map parse-line input))

(defn solve1
  ([] (solve1 (process-input @data)))
  ([data]
   (->> data
        (mapcat cells)
        frequencies
        vals
        (filter #(> % 1))
        count)))

(defn solve2
  ([] (solve2 (process-input @data)))
  ([data]
   (let [ids       (into #{} (map :id) data)
         cell->ids (reduce (fn [acc [cell id]]
                             (update acc cell (fnil conj #{}) id))
                           {}
                           (mapcat (fn [info]
                                     (for [cell (cells info)]
                                       [cell (:id info)]))
                                   data))]
     (set/difference ids
                     (->> cell->ids
                          vals
                          (remove #(= 1 (count %)))
                          (reduce into #{}))))))
