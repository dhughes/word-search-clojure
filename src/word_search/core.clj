(ns word-search.core
  (:require [clojure.string :as str]))

(def random-item (comp first shuffle))

(defn random-letter []
  (random-item (map (comp str char) (range 65 91))))



(defn random-word [min-length max-length]
  (random-item (filter (fn [word] (and (>= (count word) min-length)
                                       (<= (count word) max-length)))
                       (str/split-lines (slurp "words.txt")))))

(defn empty-table [rows columns]
  (repeat rows (repeat columns nil)))

(defn generate-line [size]
  (loop [
         remaining size
         result []]
    (if (= remaining 0) result
           (recur (dec remaining) (conj result (random-letter))))))

(defn generate-table [rows columns]
  (loop [
         remaining rows
         result []]
    (if (= remaining 0) result (recur (dec remaining) (conj result (generate-line columns))))))
