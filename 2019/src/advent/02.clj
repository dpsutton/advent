(ns advent.02
  (:require
   [advent.utils :as utils]
   [clojure.core.logic :as l :refer [fresh run* run]]
   [clojure.core.logic.fd :as fd]
   [clojure.string :as str]))

(def input
  (let [f #(->> (str/split % #",")
                (map (fn [x] (Long/parseLong x))))]
    (delay
      (into [] (mapcat f (utils/input "02"))))))

(defn interpret
  [instructions]
  (let [ins-at (fn [instructions couplet]
                 (->> instructions (drop (* 4 couplet)) (take 4)))]
    (loop [couplet 0
           state instructions]
      (let [[opcode a b r] (ins-at state couplet)]
        (if (= opcode 99)
          state
          (let [va (get state a)
                vb (get state b)
                op (case opcode 1 + 2 *)]
            (recur (inc couplet)
                   (assoc state r (op va vb)))))))))

(defn solve-1
  ([]
   (solve-1 (assoc @input 1 12 2 2)))
  ([instructions]
   (first (interpret instructions))))

(defn run-with
  ([instructions noun verb]
   (solve-1 (assoc instructions 1 noun 2 verb))))

(comment

  (run 2 [x y]
    (fresh [x' y' z']
      (fd/in x y (fd/interval 1 100))
      (fd/* 384000 x x')
      (fd/* 1 y y')
      (fd/+ x' y' z')
      (fd/+ z' 106699 19690720)))
;; => ([51 21])
  )
(defn solve-2
  []
  (let [coarse (- (run-with @input 2 1) (run-with @input 1 1))
        goal   19690720
        base   (run-with @input 0 0)]
    (let [noun (quot (- goal base) coarse)
          verb (- goal (+ base (* noun coarse)))]
      [noun verb])))

(comment

  (interpret [1,9,10,3,2,3,11,0,99,30,40,50])
  (interpret [1,0,0,0,99])
  (interpret [2,3,0,3,99])
  (interpret [2,4,4,5,99,0])
  (interpret [1,1,1,4,99,5,6,0,99]))
