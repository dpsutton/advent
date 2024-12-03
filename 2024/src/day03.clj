(ns day03)

;; For example, consider the following section of corrupted memory:

;; xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))

;; Only the four highlighted sections are real mul
;; instructions. Adding up the result of each instruction produces
;; 161 (2*4 + 5*5 + 11*8 + 8*5).

;; Scan the corrupted memory for uncorrupted mul instructions. What do
;; you get if you add up all of the results of the multiplications?

;; There are two new instructions you'll need to handle:

;;     The do() instruction enables future mul instructions.
;;     The don't() instruction disables future mul instructions.

;; Only the most recent do() or don't() instruction applies. At the beginning of the program, mul instructions are enabled.

;; For example:

;; xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))

;; This corrupted memory is similar to the example from before, but
;; this time the mul(5,5) and mul(11,8) instructions are disabled
;; because there is a don't() instruction before them. The other mul
;; instructions function normally, including the one at the end that
;; gets re-enabled by a do() instruction.

;; This time, the sum of the results is 48 (2*4 + 8*5).

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

(comment
  (-> *e ex-data :input sequential?)
  (fn [args] (apply * args))
  (map parse-a (re-seq #"mul\(\d{1,3},\d{1,3}\)" sample-input-a))
  (re-seq #"mul\(\d{1,3},\d{1,3}\)" "mul(23,4)")

  (solve-a sample-input-a)
  (solve-a)

  (map parse-b (re-seq #"mul\(\d{1,3},\d{1,3}\)|do\(\)|don't\(\)"
           "x_mul(2,4)_badbad_don't()bad_mul(3,4)_do()"))

  (solve-b sample-input-b)
  (solve-b)
  (ex-data *e)

  (throw (ex-info "an error" {:some :context}))
  )
