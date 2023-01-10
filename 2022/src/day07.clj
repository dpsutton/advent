(ns day07
  (:require
    [clojure.string :as str]))

(def sample-input "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

(def command? #(str/starts-with? % "$"))
(defn movement [[command]]
  (when-let [[_ dir] (re-matches #"^\$ cd (\S+)" command)]
    [:movement dir]))

(defn parse-move [outputs]
  (or (and (= 1 (count outputs)) (movement outputs))
      outputs))

(defn chunker
  "Like partition-by but starts a collection when `(f x)` is true and
  builds up the collection until the next element for which `f` is
  true.
  eg: (into [] (chunker number?) [1 :arg :arg 2 :arg :arg 3 :arg])
      -> [[1 :arg :arg] [2 :arg :arg] [3 :arg]]"
  [f]
  (fn [rf]
    (let [a (java.util.ArrayList.)
          initial? (volatile! ::none)]
      (fn
        ([] (rf))
        ([result]
         (let [result (if (.isEmpty a)
                        result
                        (let [v (vec (.toArray a))]
                          ;;clear first!
                          (.clear a)
                          (unreduced (rf result v))))]
           (rf result)))
        ([result input]
         (let [pval @initial?]
           (cond (identical? pval ::none)
                 (do (vreset! initial? ::some)
                     (.add a input)
                     result)
                 (f input)
                 (let [v (vec (.toArray a))]
                   (.clear a)
                   (.add a input)
                   (rf result v))
                 :else
                 (do (.add a input)
                     result))))))))

(defmulti update-tree (fn [_tree command] (first command)))
(defmethod update-tree :movement
  [tree [_ dir]]
  (if (= dir "..")
    (update tree :cwd pop)
    (update tree :cwd conj dir)))

(defmethod update-tree "$ ls"
  [tree [_ & entries]]
  (reduce (fn [tree entry]
            (let [[_ size name] (re-matches #"(\d+) (.*)" entry)
                  [_ dir-name] (re-matches #"dir (\w+)" entry)]
              (if dir-name
                (assoc-in tree (conj (:cwd tree) dir-name :name) dir-name)
                (update-in tree (conj (:cwd tree) :files) assoc name (parse-long size)))))
          tree
          entries))

(defn annotate-sizes [tree]
  (let [files-size (or (some->> tree :files vals (apply +)) 0)
        sub-folders (not-empty (dissoc tree :files :name))
        tree' (update-vals sub-folders annotate-sizes)
        sub-folders-size (map (comp :size val) (dissoc tree' :files :name))]
    (assoc tree'
           :name (:name tree)
           :files (:files tree {})
           :size (apply + files-size sub-folders-size))))

(defn tree-builder
  ([] {:cwd []
       :name "/"})
  ([tree] (-> tree
              (dissoc :cwd)
              (annotate-sizes)))
  ([tree row]
   (update-tree tree row)))

(def problem-input (slurp "src/day07.txt"))

(defn solve*
  [input]
  (let [fs (transduce (comp (chunker command?)
                            (map parse-move))
                      tree-builder
                      (str/split-lines input))]
    (tree-seq (every-pred map? #(contains? % :size))
              #(vals (dissoc % :files :size :name))
              fs)))

(comment
  (defn walk-tree [input]
    (let [tree (transduce (comp (chunker command?)
                                (map parse-move))
                          tree-builder
                          (str/split-lines input))
          total-disk    70000000
          unused-target 30000000
          total-used    (:size tree)
          target-delete (- (+ total-used unused-target) total-disk)]
      (letfn [(best-to-delete [tree best-so-far]
                (let [here (select-keys tree [:name :size :files])]
                  (if (<= (:size here) target-delete))))]
        (best-to-delete tree Long/MAX_VALUE))))
  (walk-tree sample-input)
  )

(defn solve-a
  ([] (solve-a problem-input))
  ([input]
   (let [threshold 100000
         file-tree (solve* input)]
     (->> file-tree
          (map :size)
          (filter #(<= % threshold))
          (apply +)))))

(defn solve-b
  ([] (solve-b problem-input))
  ([input]
   (let [total-disk    70000000
         unused-target 30000000
         [root :as file-tree] (solve* input)
         total-used    (:size root)
         target-delete (- (+ total-used unused-target) total-disk)]
     (->> file-tree
          (map #(select-keys % [:size :name]))
          (filter (comp #(>= % target-delete) :size))
          (sort-by :size)
          first
          :size))))

(defn solve [{:keys [input]}]
  (let [input (if input (slurp input) problem-input)]
    (time (println (solve-a input)))
    (time (println (solve-b input)))))

(comment
  (solve {})
  (solve-a sample-input)
  (solve-b sample-input)
  )
