(ns advent.01
  (:require [clojure.java.io :as io]))

(def data-1
  (delay
    (with-open [rdr (io/reader "resources/01.txt")]
      (mapv #(Long/parseLong %) (line-seq rdr)))))

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
