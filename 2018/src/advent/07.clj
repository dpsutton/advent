(ns advent.07
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(defn leading-edges [graph]
  (set/difference (set (keys graph)) (apply set/union (vals graph))))

(defn parse-line [line]
  (let [re #"Step (.*) must be finished before step (.*) can begin."
        [n1 n2] (rest (re-find re line))]
    [n1 n2]))

(defn into-graph [lines]
  (transduce (map parse-line)
             (completing (fn [acc [k v]]
                           (update acc k (fnil conj #{}) v)))
             {}
             lines))

(def test-data
  (into-graph
   (str/split-lines
    "Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.")))

(defn topo-sort [graph]
  (loop [graph       graph
         traversal   []
         unprocessed #{}]
    (let [no-inputs (-> graph leading-edges sort first)]
      (cond no-inputs
            (recur (dissoc graph no-inputs) (conj traversal no-inputs)
                   (into unprocessed (remove graph) (graph no-inputs)))

            (seq unprocessed)
            (let [no-inputs (-> unprocessed sort first)]
              (recur (dissoc graph no-inputs) (conj traversal no-inputs)
                     (disj unprocessed no-inputs)))
            :else
            traversal))))

(def data
  (delay (->> "src/advent/07.data"
              io/reader
              line-seq)))

(defn solve1
  ([] (solve1 (into-graph @data)))
  ([graph] (apply str (topo-sort graph))))
