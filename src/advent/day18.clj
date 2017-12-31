(ns advent.day18
  (:require [advent.day18data :as data]))

(defn fmap [f] (fn [x] (when x (f x))))

(defn try-parse-int [s]
  (try (Integer/parseInt (str s))
       (catch Exception e)))


(let [arg-value (fn [m optional-arg] (cond (nil? optional-arg) nil
                                           (number? optional-arg) optional-arg
                                           (symbol? optional-arg) (m optional-arg)))]
  (defn regeval [machine instruction]
    (let [[op reg optional-arg] instruction
          arg (arg-value machine optional-arg)]
      (case op
        set (assoc machine reg arg)
        add (update machine reg (fnil + 0) arg)
        mul (update machine reg (fnil * 0) arg)
        mod (update machine reg (fnil mod 0) arg)
        snd (assoc machine :sound (machine reg))
        rcv (assoc machine :recovered (if (not= 0 (machine reg))
                                        (:sound machine)
                                        (:recovered machine)))
        jgz (if (> (or (machine reg) 0) 0)
              (assoc machine :jump arg)
              machine)))))

(defn eval-loop [instructions instruction-number exit-fn]
  (loop [machine     {}
         instruction instruction-number]
    (if-let [next-ins (get instructions instruction)]
      (let [new-machine (regeval machine next-ins)]
        (if (exit-fn new-machine)
          {:machine     new-machine
           :instruction instruction}
          (recur (dissoc new-machine :jump)
                 (if-let [jump (:jump new-machine)]
                   (+ jump instruction)
                   (inc instruction)))))
      {:machine  machine
       :finished true})))

(defn solve1
  ([] (solve1 data/data (fn [m] (:recovered m))))
  ([instructions short-circuit-fn]
   (let [{:keys [machine]} (eval-loop instructions 0 short-circuit-fn)]
     (:recovered machine))))

(comment
  (= 3423 (solve1))
  )

(defn arg-value [machine optional-arg]
  (cond (nil? optional-arg) nil
        (number? optional-arg) optional-arg
        (symbol? optional-arg) (machine optional-arg)))

(defn regeval-channels [machine instruction]
  (let [[op reg optional-arg] instruction
        arg (arg-value machine optional-arg)]
    (case op
      set (assoc machine reg arg)
      add (update machine reg (fnil + 0) arg)
      mul (update machine reg (fnil * 0) arg)
      mod (update machine reg (fnil mod 0) arg)
      snd (-> machine
              (update :send #(conj % (or (machine reg)
                                         reg)))
              (update :send-count (fnil inc 0)))
      rcv (if-let [v (first (:received machine))]
            (-> machine
                (assoc reg v)
                (update :received (fn [col] (subvec col 1))))
            (assoc machine :waiting? true))
      jgz (if (> (or (machine reg) reg) 0)
            (assoc machine :jump arg)
            machine))))

(defn compute [instructions machine-info]
  (loop [machine     (:machine machine-info)
         instruction (:instruction machine-info)]
    (if-let [next-ins (get instructions instruction)]
      (let [new-machine (regeval-channels machine next-ins)
            waiting?    (:waiting? new-machine)]
        (cond
          waiting? [(assoc machine-info
                           :machine (dissoc new-machine :waiting?)
                           :instruction instruction ;; rerun same rcv instruction
                           :waiting? true) (:received new-machine)]
          :else
          (recur (dissoc new-machine :jump)
                 (if-let [jump (:jump new-machine)]
                   (+ jump instruction)
                   (inc instruction)))))
      [(-> machine-info
           (assoc :machine machine)
           (assoc :finished? true)
           (assoc :waiting? false))
       (:received machine)])))

(defn new-machine [n pid]
  {:machine     {'p          pid
                 :send       []
                 :received   []
                 :send-count 0}
   :instruction 0
   :name        n
   :finished?   false})

(defn eval-loop-channels [instructions]
  (let [compute' (partial compute instructions)]
    (loop [m-a           (new-machine 'a 0)
           m-b           (new-machine 'b 1)
           iters         0]
      (let [pending-queue (-> m-b :machine :send)
            ;; inter process communication. take one's send queue and
            ;; mount as the received queue in the other machine. this
            ;; is modified in the original so then replace afterwards
            ;; minus the values it has consumed
            compute-info (assoc-in m-a [:machine :received] pending-queue)
            [new-m-a new-pending-queue] (compute' compute-info)
            m-b (assoc-in m-b [:machine :send] new-pending-queue)]
        (cond
          ;; deadlock m-a is in a state that it cannot continue
          (or (:finished? m-b)
              (and (:waiting? m-b)
                   (empty? (-> new-m-a :machine :send))))
          [new-m-a m-b]

          :else
          (recur m-b new-m-a (inc iters)))))))
