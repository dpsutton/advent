(ns advent.day16
  (:require [clojure.string :as str]
            [advent.day16data :as data]
            [clojure.test :refer [deftest is are testing]]
            [clojure.set :as set]))

(defn spin [input amount]
  (let [size (count input)]
    (concat (drop (- size amount) input)
            (take (- size amount) input))))

(defn replace-with [col old new]
  (if (some #{old} col)
    (let [[before after-including] (split-with (partial not= old) col)]
      (concat before [new] (rest after-including)))
    col))

(defn exchange [input pos-1 pos-2]
  (cond (= pos-1 pos-2) input
        (< pos-2 pos-1) (exchange input pos-2 pos-1)
        :else
        ;; x occurs before y | pos-1 < pos-2
        (let [x       (nth input pos-1)
              y       (nth input pos-2)
              before  (take pos-1 input)
              between (take (dec (- pos-2 pos-1)) (drop (inc pos-1) input))
              after   (drop (inc pos-2) input)]
          (concat before
                  [y]
                  between
                  [x]
                  after))))

(defn partner [input x y]
  (let [[before [_ & after]] (split-with (partial not= x) input)]
    (concat (replace-with before y x) [y] (replace-with after y x))))

(def a->p (map (comp symbol str char) (range (int \a) (inc (int \p)))))

(defn solve1
  ([] (solve1 a->p data/data))
  ([input instructions]
   (reduce (fn [input instruction]
             (let [state (apply (case (first instruction)
                                  :spin spin
                                  :exchange exchange
                                  :partner partner)
                                input (rest instruction))]
               (when (not= (count state) (count input))
                 (throw (ex-info "dropped a program"
                                 {:input input
                                  :state state
                                  :instruction instruction})))
               state))
           input
           instructions)))

(deftest solve-tests
  (is (= (solve1 '(a b c d e)
                 [[:spin 1]
                  [:exchange 3 4]
                  [:partner 'e 'b]])
         '(b a e d c)))
  (is (= (solve1 '(a b c d e)
                 data/sample)
         '(b a e d c))))

(defn position-in [s output]
  (get (into {} (map vector output (range))) s))


;; all this permutation stuff cannot work because the operations can
;; refer to the elments by name: [:partner 'a 'b] is not positional
;; but representational
(defn determine-permutation [input output]
  (let [output-map (into {} (map vector output (range)))]
    (reduce (fn [m [s index]] (assoc m index (output-map s)))
            {}
            (map vector input (range)))))

(defn apply-permutation [input order-map]
  (let [reverse-order (set/map-invert order-map)
        input-map     (into {} (map-indexed (fn [i e] [i e]) input))]
    (seq (reduce (fn [new-order index]
                   (conj new-order (get input-map (get reverse-order index))))
                 []
                 (range (count input))))))

(deftest determine-permutation-tests
  (testing "simple"
    (let [input  '(a b c)
          output '(c a b)
          expected {0 1
                    1 2
                    2 0}]
      (is (= expected (determine-permutation input output)))))
  (testing "accurately determines"
    (let [initial a->p
          four-times (last (take 4 (iterate (fn [in] (solve1 in data/data)) initial)))
          determined (determine-permutation initial four-times)]
      (is (= (apply-permutation initial determined)
             four-times)))))

(deftest apply-permutation-test
  (let [done-once   (solve1 a->p data/data)
        permutation (determine-permutation a->p done-once)
        done-twice  (solve1 done-once data/data)]
    (testing "does it once"
      (is (= done-once
             (apply-permutation a->p permutation))))
    (testing "does it twice"
      (let [permutation-twice (determine-permutation a->p done-twice)]
        (is (= done-twice
               (apply-permutation a->p permutation-twice)))))
    (testing "composes"
      (is (= (apply-permutation (apply-permutation a->p permutation) permutation)
             done-twice)))))

(defn compose-permutation [perm1 perm2]
  (let [initial-state (range (count perm1))
        intermediate  (apply-permutation initial-state perm1)
        final-state   (apply-permutation (seq intermediate) perm2)]
    (prn initial-state final-state)
    (determine-permutation initial-state final-state)))

(deftest compose-permutation-tests
  (let [ident (into {} (map (fn [i] [i i]) (range 3)))]
    (is (= ident
           (reduce compose-permutation
                   ident
                   (take 1000 (repeat ident))))))
  (let [done-once   (solve1 a->p data/data)
        done-twice  (solve1 done-once data/data)
        permutation (determine-permutation a->p done-twice)]
    (testing "stuff"
      (is (= done-twice
             (apply-permutation a->p (compose-permutation permutation permutation)))))
    (testing "can correctly determine permutation"
      (let [permutation-twice (determine-permutation a->p done-twice)
            composition-twice (compose-permutation permutation permutation)]
        (is (= permutation-twice composition-twice))))))


;; i really like this implementation except that we cannot think of
;; the operations as permutations on a set since they can refer to
;; elements by name and not just position. Permutations cannot
;; "compose" as used below because of this
(defn solve2--doesntwork
  ([] (solve2--doesntwork a->p data/data))
  ([order instructions]
   (let [permutation (determine-permutation order
                                            (solve1 order instructions))
         perm1e3     (reduce compose-permutation permutation (take 1e3 (repeat permutation)))
         perm1e6     (reduce compose-permutation perm1e3 (take 1e3 (repeat perm1e3)))
         perm1e9     (reduce compose-permutation perm1e6 (take 1e3 (repeat perm1e6)))]
     (apply-permutation order perm1e9))))

(defn period [input instructions]
  (let [states (drop 1 (iterate (fn [state] (solve1 state instructions)) input))
        indices (drop 1 (range))]
    (second (first (filter (fn [[state index]] (= state input))
                           (map vector states indices))))))


;; this relies on a cycle being present and the first element being in
;; that cycle. Cycles are guaranteed as there are only 16! states but
;; this number is larged than 1e9 so we aren't guaranteed to see a
;; cycle in this example. Further, its possible that the cycle
;; excludes the first state so we get lucky. This is not a "general"
;; solution
(defn solve2
  ([] (solve2 a->p data/data 1e9))
  ([initial instructions iterations]
   (let [states (iterate (fn [state] (solve1 state instructions)) initial)
         period (->> states
                     (map vector (range))
                     (drop 1)
                     (filter (fn [[index state]] (= state initial)))
                     first
                     first)]
     (apply str (nth states (mod iterations period))))))
