(ns day03)

(def problem-input
  (slurp "src/day03.txt"))

(defn parse-a [s] (map parse-long (re-seq #"\d{1,3}" s)))

(def sample-input-a
  "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(def sample-input-b
  "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")


(defn solve-a
  ([] (solve-a problem-input))
  ([input]
   (let [tuples (map parse-a (re-seq #"mul\(\d{1,3},\d{1,3}\)" input))]
     (->> tuples (map (partial apply *)) (reduce + 0)))))

(defn parse-b [s] (case s
                    "don't()" :disabled
                    "do()" :enabled
                    (map parse-long (re-seq #"\d{1,3}" s))))

(defn solve-b
  ([] (solve-b problem-input))
  ([input]
   (let [xs (map parse-b (re-seq #"mul\(\d{1,3},\d{1,3}\)|do\(\)|don't\(\)"
                                 input))]
     (reduce (fn [[state acc] input]
               (if (= state :disabled)
                 [(if (= input :enabled) :enabled state) acc]
                 (cond
                   (sequential? input)
                   [state (+ acc (apply * input))]

                   (= :disabled input)
                   [input acc]

                   (= :enabled input)
                   [state acc]

                   :else (throw (ex-info "unrecognized state" {:input input
                                                               :state state
                                                               :acc acc})))))
             [:enabled 0]
             xs))))

(defn solve
  [{:keys [input]}]
  (let [input (if input (slurp input) problem-input)]
    (time (println (solve-a input)))
    (time (println (solve-b input)))))

(comment
  (solve {})
  (solve-a sample-input-a)
  (solve-a)

  (solve-b sample-input-b)
  (solve-b)
  )
