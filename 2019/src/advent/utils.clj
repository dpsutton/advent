(ns advent.utils
  (:require [clojure.java.io :as io]))

(defn input [day]
  (with-open [in (io/reader (str "resources/" day ".txt"))]
    (into [] (line-seq in))))
