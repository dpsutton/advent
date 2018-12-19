(ns advent.16
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def data
  (delay
   (->> "src/advent/16.data"
        slurp
        )))

;; props to mfikes for the parsing help. don't like how much parsing
;; and regex there is this year. The parse methods are essentially
;; lifted from his version.
(defn parse-before-and-after [input]
  (->> input
       (re-seq #"(?m)Before: \[(\d), (\d), (\d), (\d)\]\n(\d+) (\d) (\d) (\d)\nAfter:  \[(\d), (\d), (\d), (\d)\]\n")
       (map #(map read-string (rest %)))
       (map #(zipmap [:before :inst :after] (partition 4 %)))))

(defn bit-boolean [x]
  (if x 1 0))

(def ops
  {:add +
   :mul *
   :ban bit-and
   :bor bit-or
   :set (fn [a _] a)
   :gt  (comp bit-boolean >)
   :eq  (comp bit-boolean =)})

(defn translate [op]
  (or (-> op name (subs 0 3) keyword ops)
      (-> op name (subs 0 2) keyword ops)))

(defn fetch [registers op a b]
  (case op
    (:gtrr :eqrr :addr :mulr :banr :borr :setr) [(nth registers a) (nth registers b)]
    (:gtir :eqir :seti)                         [a (nth registers b)]
    [(nth registers a) b]))

(defn process [registers [op a b c]]
  (assoc registers c (apply (translate op) (fetch registers op a b))))

(def all-ops #{:addi :addr :mulr :muli :bani :banr :borr :bori
               :seti :setr :gtir :gtri :gtrr :eqir :eqri :eqrr})

(defn solve1
  ([] (solve1 (parse-before-and-after @data)))
  ([before-inst-afters] (->> before-inst-afters
                             (map (fn [{:keys [before inst after]}]
                                    (filter #(= after (outcome (vec before) (assoc (vec inst) 0 %)))
                                            all-ops)))
                             (filter #(>= (count %) 3))
                             count)))

(defn deduce [n->opscodes insts]
  (if (= 16 (count (keys n->opscodes)))
    n->opscodes
    (let [to-check                    (apply disj all-ops (-> n->opscodes vals))
          {:keys [before inst after]} (first insts)]
      (if (n->opscodes (first inst))
        (recur n->opscodes (rest insts))
        (let [candidates (filter #(= after (outcome (vec before)
                                                    (assoc (vec inst) 0 %)))
                                 to-check)]
          (if (= 1 (count candidates))
            (recur (assoc n->opscodes (first inst) (first candidates)) (rest insts))
            (recur n->opscodes (rest insts))))))))

(def translations (delay (deduce {} (cycle (parse-before-and-after @data)))))

(defn solve2
  []
  (let [instructions (->> @data
                          str/split-lines
                          ;; again taken from mfikes version because
                          ;; it was annoying and he's really good at
                          ;; it
                          (drop (+ 2 (* 4 (count (parse-before-and-after @data)))))
                          (map #(map read-string (re-seq #"\d+" %)))
                          (map #(cons (@translations (first %)) (rest %))))]
    (reduce process [0 0 0 0] instructions)))
