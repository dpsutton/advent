(ns advent.day20
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn euclidean [x y z]
  (Math/sqrt (+ (* x x) (* y y) (* z z))))

(def line "p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>")
(defn parse-pair [p]
  (map read-string (str/split (str/trim (str/replace (str/replace p #".=<" "") #">," "")) #",")))

(defn parse-line [index l]
  (let [matches       (re-seq #"<([-| ]?\d+,-?\d+,-?\d+)>" l)
        [pos vel acc] (->> matches
                           (map second)
                           (map #(str "[" % "]"))
                           (map read-string)
                           vec)]
    {:position     pos
     :velocity     vel
     :acceleration acc
     :particle     index
     :net-acc      (apply euclidean acc)}))

(def data
  (let [lines (line-seq (io/reader (io/resource "advent/day20data.txt")))]
    (map-indexed parse-line lines)))

(defn solve1
  ([] (solve1 data))
  ([particles]
   (let [net-accelerations (map :net-acc particles)
         min-f             (apply min net-accelerations)
         particle          (filter (fn [p] (= min-f (:net-acc p))) particles)]
     (assert (= 1 (count particle))
             "I can only solve this when there is a unique particle
             with the smallest net acceleration")
     (:particle (first particle)))))

(defn next-position [{:keys [position velocity acceleration] :as particle}]
  (let [updated-v (mapv + velocity acceleration)
        updated-p (mapv + position updated-v)]
    (assoc particle
           :position updated-p
           :velocity updated-v)))

(defn prune-collisions [pos-map]
  (into {}
        (comp
         (remove (fn [[pos particles]]
                   (> (count particles) 1)))
         (map (fn [[pos particles]]
                [pos (first particles)])))
        pos-map))

(defn particle-list->map [particles]
  (let [by-position (reduce (fn [acc particle]
                              (update acc (:position particle) (fnil conj []) particle))
                            {}
                            particles)]
    (prune-collisions by-position)))

(defn step-pos-map [pos-map]
  (particle-list->map (map next-position (vals pos-map))))

(defn particle-states [particles]
  (iterate step-pos-map (particle-list->map particles)))

(defn distance [p1 p2]
  (apply + (mapv (comp #(Math/abs ^long %) -) p2 p1)))

(defn minimum-distance [pos-map]
  (apply min (for [pos1 (keys pos-map)
                   pos2 (keys pos-map)
                   :when (not= pos1 pos2)]
               (distance pos1 pos2))))

(defn moving-away? [p1 p2])

(defn solve2 []
  ;; there's only a single arity because this solution is a fluke and
  ;; not general, and my only confidence in its veracity is that the
  ;; website accepted its answer
  (->> data
       particle-states
       (take 80)
       last
       count))

(comment
  (map count (take 80 (particle-states data)))
  ;; yields (1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 979 972
  ;; 956 948 936 936 924 900 886 886 871 845 845 836 826 803 787 777
  ;; 765 755 743 739 737 724 717 717 701 688 685 657 657 657 657 657
  ;; 657 657 657 657 657 657 657 657 657 657 657 657 657 657 657 657
  ;; 657 657 657 657 657 657 657 657 657 657 657 657 657 657 657 657
  ;; 657 657 657 657) and it appears that 657 is an answer accepted by
  ;; aoc. but this is by no means conclusive.

  (= (solve2) 657)
  )
