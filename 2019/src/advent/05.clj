(ns advent.05
  (:require
   [advent.utils :as utils]
   [clojure.string :as str]))

(def input
  (let [f #(->> (str/split % #",")
                (map (fn [x] (Long/parseLong x))))]
    (delay
      (into [] (mapcat f (utils/input "05"))))))

(defn chomp* [x n]
  (when (pos? n)
    (let [x' (mod x 10)]
      (lazy-seq
        (cons x'
              (chomp* (/ (- x x') 10) (dec n)))))))

(defn chomp [x n]
  (let [x' (/ (- x (mod x 100)) 100)]
    (chomp* x' n)))

(defn decode-op [instruction]
  (let [ins (mod instruction 100)]
    (into [ins]
          (chomp instruction (case ins
                               (1 2) 3
                               (3 4) 2
                               (5 6) 2
                               (7 8) 3
                               99 0)))))

(defn current-instruction [{:keys [instructions ip]}]
  (drop ip instructions))

(defn interpret [{:keys [instructions input ip] :as state}]
  (let [[instruction a1 a2 a3] (current-instruction state)
        [opcode m1 m2 m3]      (decode-op instruction)
        f                      (fn [x m]
                                 (if (zero? m)
                                   (get instructions x)
                                   x))]
    (case opcode
      99 (assoc state :halted? true)
      1  (-> state
             (update :ip + 4)
             (update :instructions assoc a3 (+ (f a1 m1) (f a2 m2))))
      2  (-> state
             (update :ip + 4)
             (update :instructions assoc a3 (* (f a1 m1) (f a2 m2))))
      3  (-> state
             (update :ip + 2)
             (update :input rest)
             (update :instructions assoc a1 (first input)))
      4  (-> state
             (update :ip + 2)
             (update :output conj (f a1 m1)))
      5  (let [test (f a1 m1)]
           (-> state
               (assoc :ip (if (zero? test)
                            (+ ip 3)
                            (f a2 m2)))))
      6  (let [test (f a1 m1)]
           (-> state
               (assoc :ip (if (zero? test)
                            (f a2 m2)
                            (+ ip 3)))))
      7  (-> state
             (update :ip + 4)
             (update :instructions assoc a3 (if (< (f a1 m1) (f a2 m2)) 1 0)))
      8  (-> state
             (update :ip + 4)
             (update :instructions assoc a3 (if (= (f a1 m1) (f a2 m2)) 1 0))))))

(defn ->state
  ([ins] (->state ins []))
  ([ins input] {:ip 0 :instructions ins :output [] :input input}))

(defn run-machine [instructions input]
  (take-while (complement :halted?)
              (iterate interpret (->state instructions input))))

(defn solve-1
  ([] (solve-1 @input [1]))
  ([instructions input]
   (-> (run-machine instructions input)
       last
       :output
       peek)))

(defn solve-2
  ([] (solve-1 @input [5])))

(comment

  (-> (run-machine [3,9,8,9,10,9,4,9,99,-1,8] [8])
      last
      (dissoc :instructions))
  ;; input = 8
  (solve-1 [3,9,8,9,10,9,4,9,99,-1,8] [7])
  (solve-1 [3,9,8,9,10,9,4,9,99,-1,8] [8])
  ;; input < 8
  (solve-1 [3,9,7,9,10,9,4,9,99,-1,8] [4])
  (solve-1 [3,9,7,9,10,9,4,9,99,-1,8] [9])
  ;; input = 8 with immediate mode
  (solve-1 [3,3,1108,-1,8,3,4,3,99] [8])
  (solve-1 [3,3,1108,-1,8,3,4,3,99] [7])
  ;; output 0 if input was zero else 1
  (solve-1 [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] [0])
  (solve-1 [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] [0])
  (solve-1 [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] [2])
  (solve-1 [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] [2]))
