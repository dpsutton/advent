(ns advent.day7
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [advent.day7data :as data]
            [clojure.test :refer [testing deftest are is]]))


(defn has-edges? [node] (-> node second :edges not-empty))

(defn no-incoming-edges [adjacency]
  (let [nodes-with-edges (->> adjacency
                              (filter (fn [[node info]] (not-empty (:edges info))))
                              (map first)
                              set)]
    (first (set/difference nodes-with-edges (set (mapcat :edges (vals adjacency)))))))

(defn solve1
  ([] (solve1 data/data))
  ([graph] (no-incoming-edges adjacency)))

(defn node-weight [cache adjacency node-name]
  (if-let [weight (cache node-name)]
    {:cache cache :weight weight}
    (let [{:keys [edges weight]} (adjacency node-name)
          caches-and-weights     (map (partial node-weight cache adjacency) edges)
          [cache sum] (reduce (fn [[cache weight] {c :cache w :weight}] [(merge cache c) (+ weight w)])
                              [cache weight]
                              caches-and-weights)]
      {:cache (assoc cache node-name sum) :weight sum})))

(defn equal-weights? [weights]
  (if (seq weights)
    (apply = weights)
    true))

(defn balanced? [cache adjacency node-name]
  (let [{:keys [edges]} (adjacency node-name)]
    (equal-weights? (map (partial node-weight cache adjacency) edges))))

(deftest node-weight-tests
  (let [adjacency data/sample]
    (are [node weight] (= (:weight (node-weight {} adjacency node)) weight)
      :ugml 251
      :padx 243
      :fwft 243)))

(defn looking-for [cache adjacency node-name]
  (let [{:keys [edges weight]}]))

(deftest balanced?-tests
  (let [adjacency data/sample]
    (are [node b?] (= (balanced? adjacency node) b?)
      :ugml true
      :padx true
      :fwft true
      :tknk false)))

(defn unbalanced-parent-balanced-children
  [adjacency root]
  (let [{:keys [cache]} (node-weight {} adjacency root)]
    (loop [parent        nil
           current       root
           parent-weight 0]
      (let [{:keys [weight edges]} (adjacency current)
            partitions             (->> edges
                                        (map (fn [kid] [(cache kid) kid]))
                                        (group-by first))
            [size distinct-edge]          (->> partitions
                                        (filter (fn [[p-size nodes]] (= 1 (count nodes))))
                                        first
                                        second
                                        first)]
        (if (balanced? cache adjacency distinct-edge)
          {:bad-edge current
           :good-edge-below distinct-edge
           :parent parent
           :other-partition-sizes (->> partitions
                                       (filter (fn [[p-size nodes]] (not= 1 (count nodes))))
                                       first
                                       first)
           :bad-size size}
          (recur current distinct-edge weight))))))

(defn solve2
  ([] (solve2 data/data))
  ([adjacency] ))
