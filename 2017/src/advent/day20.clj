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

  (let [states (particle-states data)]
    (map minimum-distance (take 80 states)))
  
  ;; (65 73 77 85 53 88 88 42 30 20 84 70 52 96 48 68 48 66 145 68 46
  ;; 112 140 92 63 112 103 105 63 84 36 43 116 79 17 56 59 48 73 129
  ;; 260 59 116 152 219 335 448 409 393 403 394 362 169 389 390 735
  ;; 944 926 907 998 815 626 431 768 1131 1125 959 990 1412 1478 1456
  ;; 1668 1787 2013 2038 2063 2088 2113 2138 2191)

  ;; we see that the minimum distance between points is growing giving
  ;; us confidence that the particles have come to have acceleration
  ;; dominate and the collisions that will happen have already
  ;; happened.

  (= (solve2) 657)
  )
