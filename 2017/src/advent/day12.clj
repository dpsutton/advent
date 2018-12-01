(ns advent.day12
  (:require [clojure.string :as str]
            [advent.day12data :as data]
            [clojure.set :as set]
            [clojure.test :refer [deftest is are testing]]))

(defn connected-nodes [graph node]
  (set (get graph node)))

(defn connected-graph [graph start]
  (loop [connected #{}
         todo      #{start}]
    (if-let [node (first todo)]
      (recur (conj connected node)
             (set/difference (set/union todo (connected-nodes graph node))
                             connected))
      connected)))

(defn solve1
  ([] (solve1 data/data 0))
  ([graph start] (count (connected-graph graph start))))

(defn solve2
  ([] (solve2 data/data 0))
  ([graph start]
   (loop [graph            graph
          connected-graphs []
          entry-node       start]
     (if entry-node
       (let [connected-subgraph (connected-graph graph entry-node)
             smaller-graph      (apply dissoc graph connected-subgraph)]
         (recur smaller-graph
                (conj connected-graphs connected-subgraph)
                (ffirst smaller-graph)))
       (count connected-graphs)))))
