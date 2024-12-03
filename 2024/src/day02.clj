(ns day02
  (:require
   [clojure.string :as str]))

(def problem-input (slurp "src/day02.txt"))

(def sample-input
  "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")

(defn safe?
  [level]
  (letfn [(monotonic? [ns] (or (apply >= ns) (apply <= ns)))
          (smooth? [ns] (reduce (fn [_ [n n']] (or (<= 1 (abs (- n n')) 3)
                                                   (reduced false)))
                                true (map vector ns (rest ns))))]
    (and (monotonic? level)
         (smooth? level))))

(defn tolerant-safe?
  [level]
  (letfn [(without [ns]
            (map (fn [i] (for [i' (range (count ns))
                               :when (not= i' i)]
                           (nth ns i')))
                 (range (count ns))))]
    (or (safe? level)
        (some true? (for [sublist (without level)]
                      (safe? sublist))))))

(defn parse-line [l] (map parse-long (str/split l #"\s+")))

(defn solve-a
  ([] (solve-a problem-input))
  ([txt]
   (transduce (comp (map parse-line) (filter safe?) (map (constantly 1)))
              + 0 (str/split-lines txt))))

(defn solve-b
  ([] (solve-b problem-input))
  ([txt]
   (transduce (comp (map parse-line) (filter tolerant-safe?) (map (constantly 1)))
              + 0 (str/split-lines txt))))

(defn solve [{:keys [input]}]
  (let [input (or (when input (slurp input))
                  problem-input)]
    (time (println (solve-a input)))
    (time (println (solve-b input)))))

(comment
  (solve-a sample-input)
  (solve-a)
  (solve-b sample-input)
  (solve-b)

  (solve {})
  (pst)

  "clj -X day02/solve"

  )
