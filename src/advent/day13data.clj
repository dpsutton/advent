(ns advent.day13data
  (:require [clojure.string :as str]))

(def data-text
  "0: 3
  1: 2
  2: 6
  4: 4
  6: 4
  8: 8
  10: 6
  12: 8
  14: 5
  16: 6
  18: 8
  20: 8
  22: 12
  24: 6
  26: 9
  28: 8
  30: 12
  32: 12
  34: 17
  36: 12
  38: 8
  40: 12
  42: 12
  44: 10
  46: 12
  48: 12
  50: 12
  52: 14
  54: 14
  56: 10
  58: 14
  60: 12
  62: 14
  64: 14
  66: 14
  68: 14
  70: 14
  72: 14
  74: 14
  76: 14
  86: 14
  94: 20
  96: 18")

(defn process-line [line]
  (as-> line <>
      (str/trim <>)
      (str/replace <> #":" "")
      (str/split <> #"\s")
      (map read-string <>)
      (vec <>)))

(def data (map process-line (str/split-lines data-text)))
