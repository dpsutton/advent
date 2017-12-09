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

(defn node-weight [adjacency node-name]
  (let [{:keys [edges weight]} (adjacency node-name)
        weights     (map (partial node-weight adjacency) edges)]
    (apply + weight weights)))

(defn equal-weights? [weights]
  (if (seq weights)
    (apply = weights)
    true))

(defn balanced? [adjacency node-name]
  (let [{:keys [edges]} (adjacency node-name)]
    (equal-weights? (map (partial node-weight adjacency) edges))))

(deftest node-weight-tests
  (let [adjacency data/sample]
    (are [node weight] (= (node-weight adjacency node) weight)
      :ugml 251
      :padx 243
      :fwft 243)))

(deftest balanced?-tests
  (let [adjacency data/sample]
    (are [node b?] (= (balanced? adjacency node) b?)
      :ugml true
      :padx true
      :fwft true
      :tknk false)))

(defn unbalanced-parent-balanced-children
  [adjacency root]
  (loop [current root]
    (let [{:keys [weight edges]} (adjacency current)
          partitions             (->> edges
                                      (map (fn [kid] [(node-weight adjacency kid) kid]))
                                      (group-by first))
          [size distinct-edge]   (->> partitions
                                      (filter (fn [[p-size nodes]] (= 1 (count nodes))))
                                      first
                                      second
                                      first)]
      ;; we're looking for the first balanced node after a chain of
      ;; unbalanced nodes. We need to stay look down because we need
      ;; that node's siblings to know how far to adjust it.
      (if (balanced? adjacency distinct-edge)
        (let [correct-total-size (->> partitions
                                      (filter (fn [[p-size nodes]] (not= 1 (count nodes))))
                                      first
                                      first)
              current-size       (:weight (adjacency distinct-edge))]
          {:last-bad-node           current
           :node-needing-adjustment distinct-edge
           :correct-size            (+ current-size
                                       (- correct-total-size size))
           :current-size            current-size})
        (recur distinct-edge)))))

(defn imbalanced-child
  [adjacency {:keys [state]}]
  (let [{:keys [weight edges]} (adjacency state)
        weights                (map (partial node-weight adjacency) edges)
        unique                 (->> (reduce (fn [acc [edge weight]]
                                              (update acc weight (fnil conj []) edge))
                                            {}
                                            (map vector edges weights))
                                    (some (fn [[size nodes]]
                                            (when (= (count nodes) 1)
                                              (first nodes)))))]
    (when unique
      (let [total-weight           (node-weight adjacency unique)
            total-expected         (node-weight adjacency (first (remove #{unique} edges)))
            diff                   (- total-expected total-weight)
            size                   (:weight (adjacency unique))]
        {:state     unique
         :size      size
         :should-be (+ size diff)}))))

(defn imbalanced-child'
  [adjacency state]
  (let [{:keys [weight edges]} (adjacency state)
        weights                (map (partial node-weight adjacency) edges)
        unique                 (->> (reduce (fn [acc [edge weight]]
                                              (update acc weight (fnil conj []) edge))
                                            {}
                                            (map vector edges weights))
                                    (some (fn [[size nodes]]
                                            (when (= (count nodes) 1)
                                              (first nodes)))))]
    unique))

(defn imbalanced-states
  [adjacency]
  (iterate (partial imbalanced-child adjacency) {:state (no-incoming-edges adjacency)}))

(defn imbalanced-states'
  [adjacency]
  (iterate (partial imbalanced-child' adjacency) (no-incoming-edges adjacency)))

(defn solve2'
  "We can remember the imbalance as we go and just keep taking states while there is an imbalance"
  ([] (solve2' data/data))
  ([adjacency] (->> adjacency imbalanced-states (take-while some?) last :should-be)))

(defn solve2''
  "or we can look for the imbalance at the toplevel. since there is only
  one malformed state, the entire observed imbalance is at that site"
  ([] (solve2'' data/data))
  ([adjacency] (let [state           (last (take-while some? (imbalanced-states' adjacency)))
                     {:keys [edges]} (adjacency (no-incoming-edges adjacency))
                     diff            (let [weights  (map (partial node-weight adjacency) edges)
                                           c        (count weights)
                                           d        (distinct weights)
                                           sum      (apply + weights)
                                           without  (/ (- sum (apply + d))
                                                       (- c 2))
                                           standout (first (remove #{without} d))]
                                       (- without standout))
                     size            (:weight (adjacency state))]
                 (+ size diff))))

(defn solve2
  "Or we can walk down a little more explicitly and compute things as we go"
  ([] (solve2 data/data))
  ([adjacency] (:correct-size (unbalanced-parent-balanced-children adjacency
                                                                   (no-incoming-edges adjacency)))))
