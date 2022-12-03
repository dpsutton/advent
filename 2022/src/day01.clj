(ns day01
  (:require
   [clojure.string :as str])
  (:import java.util.PriorityQueue))

(def problem-input (slurp "src/day01.txt"))

(defn process
  [lines]
  (->> lines
       str/split-lines
       (partition-by str/blank?)
       (take-nth 2)
       (map (fn [c] (map parse-long c)))))

(defn solve-a
  ([] (solve-a problem-input))
  ([txt]
   (apply max (map (partial apply +) (process txt)))))

(defn heap-keep
  "A transducing reducer that keeps a bounded min-heap under
  `j-u-compare-f` to keep only the top `threshold` items. Results are
  returned in descending order sorted under this compare
  function. `threshold` should be an integer and `j-u-compare-f`
  should be a function of two args returning less than zero, zero, or
  greater than zero if the first arg is less than, equal to, or
  greater than the second argument, respectively."
  [threshold j-u-compare-f]
  (fn heap-keep-reducer
    ([] (PriorityQueue. 30 j-u-compare-f))
    ([^PriorityQueue q]
     (loop [acc (transient [])]
       (if-let [x (.poll q)]
         (recur (conj! acc x))
         (reverse (persistent! acc)))))
    ([^PriorityQueue q item]
     (if (>= (.size q) threshold)
       (let [smallest (.peek q)]
         (if (pos? (j-u-compare-f item smallest))
           (doto q
             (.poll)
             (.offer item))
           q))
       (doto q
         (.offer item))))))

(defn solve-b
  ([] (solve-b problem-input))
  ([txt]
   (apply + (transduce (map (fn [c] (apply + c)))
                       (heap-keep 3 compare)
                       (process txt)))))

(defn solve [{:keys [input]}]
  (let [input (if input (slurp input) problem-input)]
    (time (println (solve-a input)))
    (time (println (solve-b input)))))

(comment
  (solve-a)
  (solve-b)
  ;; clj -X day01/solve :input "\"src/random.txt\""
  )
