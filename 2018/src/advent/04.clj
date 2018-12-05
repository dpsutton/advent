(ns advent.04
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def data
  (delay (->> "src/advent/04.data"
              io/reader
              line-seq)))

(def test-data
  (drop 1
        (str/split-lines
         "
[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up")))

(defn parse-op [line]
  (let [begin  #"Guard #(\d+) begins shift"
        asleep #"falls asleep"
        wakes  #"wakes up"]
    (cond (re-find begin line)
          (let [[_ guard] (re-find begin line)]
            {:op    :start
             :guard (Integer/parseInt guard)})
          (re-find asleep line)
          {:op :falls-asleep}
          (re-find wakes line)
          {:op :wakes-up})))

(defn parse-date [line]
  (let [[_ year month day hour minutes] (re-find #"(\d+)\-(\d+)\-(\d+) (\d+):(\d+)" line)]
    (zipmap [:year :month :day :hour :minutes]
            (map #(Integer/parseInt %) [year month day hour minutes]))))

(defn parse-line [line]
  (let [op (parse-op line)]
    (assoc op :time (parse-date line))))

(defn partitions [ops]
  (when (seq ops)
    (let [start       (first ops)
          sleep&wakes (take-while (comp (complement #{:start}) :op) (rest ops))]
      (cons {:start       start
             :sleep&wakes sleep&wakes}
            (partitions (drop (inc (count sleep&wakes)) ops))))))

(defn combine-heatmaps [m1 m2]
  (merge-with (fnil + 0) m1 m2))

(defn process-sleeps&wakes [s&ws]
  (let [s&ws (partition 2 s&ws)]
    (reduce (fn [stats sleep-wake-pair]
              (let [start (-> sleep-wake-pair first :time :minutes)
                    stop  (-> sleep-wake-pair second :time :minutes)]
                (-> stats
                    (update :duration + (- stop start))
                    (update :heatmap
                            combine-heatmaps
                            (zipmap (range start stop) (repeat 1))))))
            {:duration 0
             :heatmap  {}}
            s&ws)))

(defn process-data [data]
  (reduce (fn [acc {:keys [start sleep&wakes]}]
            (let [stats (process-sleeps&wakes sleep&wakes)]
              (update acc (:guard start)
                      (fn [guard-stats]
                        (-> guard-stats
                            (update :duration (fnil + 0) (:duration stats))
                            (update :heatmap combine-heatmaps (:heatmap stats)))))))
          {}
          (partitions data)))

(defn solve1
  ([] (solve1 (sort @data)))
  ([data]
   (let [[guard stats] (->> data
                            (map parse-line)
                            process-data
                            (apply max-key (comp :duration val)))]
     (* guard (->> stats :heatmap (apply max-key val) first)))))

(defn map-max [m]
  (if (seq m)
    (apply max-key val m)
    [0 0]))

(defn solve2
  ([] (solve2 (sort @data)))
  ([data]
   (let [guard-stats (->> data
                          (map parse-line)
                          process-data)
         [guard [minute minutes]]
         (->> guard-stats
              (map (fn [[guard stats]]
                     [guard (->> stats :heatmap map-max)]))
              ;; {:guard-id [minute-most-asleep minutes-asleep-at-that-minute]
              (apply max-key (comp second second)))]
     (* guard minute))))
