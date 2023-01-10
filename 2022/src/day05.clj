(ns day05
  (:require
    [clojure.string :as str]))

(def problem-input (slurp "src/day05.txt"))

(def sample-input
  "    [D]
[N] [C]
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(defn parse-stacks
  [width lines]
  (let [empty-stacks (vec (repeat width []))
        indices      (map-indexed vector (range 1 (inc (* 4 width)) 4))
        g            (fn [x] (when (not= x \space) x))]
    (transduce identity
               (completing (fn [stacks line]
                             (reduce (fn [ss [i i']]
                                       (if-let [x (g (get line i'))]
                                         (update ss i conj x)
                                         ss))
                                     stacks
                                     indices))
                           (partial zipmap (next (range))))
               empty-stacks
               (reverse lines))))

(defn parse-movements
  [line]
  (let [[_ amount origin dest] (re-find #"move (\d+) from (\d+) to (\d+)" line)]
    (update-vals {:amount amount :origin origin :destination dest} parse-long)))

(defn run
  "Run the virtual machine. `reverse?` is a boolean describing how the
  elements are moved, false for as a stack, true for as a queue.

  State is a map with keys:
  - stacks: a map of integer to stacks
  - instructions: a vector of maps {:amount :origin :destination}"
  [reverse? state]
  (letfn [(iter [n f init]
            (if (pos? n)
              (recur (dec n) f (f init))
              init))
          (stack-items [s n]
            (letfn [(iter [s n acc]
                      (if (zero? n)
                        (if reverse? (reverse acc) acc)
                        (recur (pop s) (dec n) (conj acc (peek s)))))]
              (iter s n [])))]
   (reduce (fn [stacks {:keys [amount origin destination]}]
             (let [new-s (iter amount pop (stacks origin))]
               (-> stacks
                   (assoc origin new-s)
                   (update destination
                           (partial apply conj)
                           (stack-items (stacks origin) amount)))))
           (:stacks state)
           (:instructions state))))

(defn top-of-stack
  [stacks]
  (let [ks (sort (keys stacks))]
    (map (comp peek stacks) ks)))

(def index-row? #(re-find #"^ 1" %))

(defn identify-width
  [number-row]
  (->> (str/split number-row #"\s+") last parse-long))

(defn split
  [lines]
  (let [[stacks index-row movements] (partition-by index-row? (str/split-lines lines))
        width                        (identify-width (first index-row))]
    {:stacks       (parse-stacks width stacks)
     :width        width
     :instructions (map parse-movements (next movements))}))

(defn solve-a
  ([] (solve-a problem-input))
  ([input]
   (apply str (->> input split (run false) top-of-stack))))

(defn solve-b
  ([] (solve-b problem-input))
  ([input]
   (apply str (->> input split (run true) top-of-stack))))


(defn solve [{:keys [input]}]
  (let [input (if input (slurp input) problem-input)]
    (time (println (solve-a input)))
    (time (println (solve-b input)))))

(comment
  (solve {})
  (solve-a)
  (solve-b)
  )
