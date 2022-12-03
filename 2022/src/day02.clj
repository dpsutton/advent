(ns day02
  (:require
   [clojure.set :as set]
   [clojure.string :as str]))

(def problem-input (slurp "src/day02.txt"))

(def sample-input "A Y
B X
C Z
")

(def win
  {:rock     :scissors
   :paper    :rock
   :scissors :paper})

(def play-scores {:rock 1 :paper 2 :scissors 3})
(def outcome-scores {:lose 0 :draw 3 :win 6})

(defn translate-a
  "Takes a string like \"C Z\" and translates it to [:rock :scissors]"
  [[opponent _ you]]
  [({\A :rock \B :paper \C :scissors} opponent) ({\X :rock \Y :paper \Z :scissors} you)])

(defn score-hand
  [[opponent you]]
  (letfn [(outcome [opponent you]
            (cond (= opponent you)       :draw
                  (= (win you) opponent) :win
                  :else                  :lose))]
    (+ (outcome-scores (outcome opponent you)) (play-scores you))))

(defn score-games
  [translate games]
  (reduce + 0 (map (comp score-hand translate) games)))

(defn solve-a
  ([] (solve-a problem-input))
  ([input-txt]
   (score-games translate-a (str/split-lines input-txt))))

(defn translate-b
  "Opponent: A, B, C for rock, paper, scissors.
  You: X:lose Y: draw, Z: win"
  [[opponent _ you]]
  (let [opp    ({\A :rock \B :paper \C :scissors} opponent)
        lose   (set/map-invert win)
        choice (case you
                 \X (win opp)
                 \Y opp
                 \Z (lose opp))]
    [opp choice]))

(defn solve-b
  ([] (solve-b problem-input))
  ([input-txt]
   (score-games translate-b (str/split-lines input-txt))))

(comment
  (solve-a)
  (solve-b)
  )

(defn solve [{:keys [input]}]
  (let [input (if input (slurp input) problem-input)]
    (time (println (solve-a input)))
    (time (println (solve-b input)))))
