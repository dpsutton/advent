(ns advent.01
  (:require
   [advent.utils :as utils]))

(def data-1
  (delay
    (mapv #(Long/parseLong %) (utils/input "01"))))

(defn mass->fuel [mass]
  (-> mass (/ 3) (Math/floor) (- 2) long))

(defn solve-1
  ([]
   (solve-1 @data-1))
  ([masses]
   (reduce + 0 (map mass->fuel masses))))

(defn total-fuel
  [mass]
  (->> (iterate mass->fuel mass)
       ;; only want the mass of the fuel, not including the payload
       ;; mass
       (drop 1)
       (take-while pos?)))

(defn solve-2
  ([]
   (solve-2 @data-1))
  ([masses]
   (reduce + 0 (mapcat total-fuel masses))))
