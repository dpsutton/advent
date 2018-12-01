(ns advent.day09
  (:require [clojure.string :as str]
            [advent.day09data :as data]))

(defn read-garbage [full-input]
  (loop [input full-input
         garbage []
         state :garbage]
    (let [c (first input)]
      (case state
        :garbage
        (case c
          ;; end garbage reading
          \> [(str/join (conj garbage c)) (rest input)]
          \! (recur (rest input) garbage :ignore)
          ;; else
          (recur (rest input) (conj garbage c) :garbage))
        :ignore
        ;; annoyingly, they don't want to include the ignored chars in
        ;; the count so we omit them here
        (recur (rest input) garbage :garbage)))))

(defn read-group [full-input]
  (loop [input full-input
         group {:groups []
                :garbage []}]
    (if (seq input)
      (let [c (first input)]
        (case c
          ;; starting garbage reading: stick garbage into garbage
          ;; holder and recur on remaining string
          \< (let [[garbage remaining-input] (read-garbage input)]
               (recur remaining-input (update group :garbage conj garbage)))
          ;; new subgroup, recurse to get group and then recur on
          ;; remaining with groups added in
          \{ (let [[nested remaining] (read-group (rest input))]
               (recur remaining (update group :groups conj nested)))
          ;; ending of a group, RETURN from a recurse
          \} [group (rest input)]
          ;; separator ignore
          \, (recur (rest input) group)
          ))
      ;; when returning, if there is no more input just return the
      ;; group rather than [group remaining-input] as earlier
      group)))

(defn annotate-tree
  ;; walk the tree and annotate with level of nesting
  ([tree] (annotate-tree 0 tree))
  ([level tree]
   (-> tree
       (assoc :nesting level)
       (update :groups (fn [groups] (mapv (partial annotate-tree (inc level)) groups))))))

(defn walk-tree [node-fn tree]
  (apply + (node-fn tree) (map (partial walk-tree node-fn) (:groups tree))))

(defn score-tree [tree]
  (walk-tree (fn [node] (:nesting node)) tree))
(defn count-garbage [tree]
  (walk-tree (fn [node]
               (let [garbage (:garbage node)]
                 (if (seq garbage)
                   (apply + (map (fn [g] (- (count g) 2)) garbage))
                   0)))
             tree))

;; strip garbage from tree if you like
(defn cleanse-string [full-input]
  (loop [input    full-input
         cleansed []]
    (if (seq input)
      (let [c (first input)]
        (if (= c \<)
          (let [[garbage remaining] (read-garbage input)]
            (recur remaining cleansed))
          (recur (rest input) (conj cleansed c))))
      (str/join cleansed))))

(defn solve1
  ([] (solve1 data/data))
  ([input] (-> input read-group annotate-tree score-tree)))

(defn solve2
  ([] (solve2 data/data))
  ([input] (-> input read-group count-garbage)))
