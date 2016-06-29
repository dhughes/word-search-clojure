(ns word-search.core
  (:require [clojure.string :as str]))

(def random-item (comp first shuffle))

(def words (str/split-lines (slurp "words.txt")))

(defn random-letter []
  (random-item (map (comp str char) (range 65 91))))

(defn random-word [min-length max-length]
  (random-item (filter (fn [word] (and (>= (count word) min-length)
                                       (<= (count word) max-length)))
                       words)))

(defn rand-range [min max]
  (rand-nth (range min max)))

;; a generic function for extracting sets of characters from a table of characters
(defn extract [fnx fny table x y length]
  (let [
        width (count (get table 0))
        height (count table)]
    (loop [
           x x
           y y
           remaining length
           result []]
      (cond
        (= remaining 0) result
        (> 0 x) result
        (> 0 y) result
        (>= x width) result
        (>= y height) result
        ;; find the next letter in the table
        :else (recur
                (fnx x)
                (fny y)
                (dec remaining)
                (conj result {:x x :y y :value (get-in table [y x])}))))))

;; use left / top
(defn use-zero [& args]
  0)

;; use offset (from left / top)
(defn use-offset [_ offset]
  (dec offset))

;; use size (height / width)
(defn use-size [size & args]
  size)

;; offset size
(defn use-size-minus-offset [size offset]
  (- size (dec offset)))

;; picks a random point using the bounds defined by the provided functions
(defn random-point [fnLeft fnTop fnRight fnBottom puzzle length]
  (let [
        width (count (get puzzle 0))
        height (count puzzle)]
    {
     :x (rand-range (fnLeft width length) (fnRight width length))
     :y (rand-range (fnTop height length) (fnBottom height length))}))

(def directions {
                 :north {
                         :random-point (partial random-point use-zero use-offset use-size use-size)
                         :extract (partial extract identity dec)}
                 :northeast {
                             :random-point (partial random-point use-zero use-offset use-size-minus-offset use-size)
                             :extract (partial extract inc dec)}
                 :east {
                        :random-point (partial random-point use-zero use-zero use-size-minus-offset use-size)
                        :extract (partial extract inc identity)}
                 :southeast {
                             :random-point (partial random-point use-zero use-zero use-size-minus-offset use-size-minus-offset)
                             :extract (partial extract inc inc)}
                 :south {
                         :random-point (partial random-point use-zero use-zero use-size use-size-minus-offset)
                         :extract (partial extract identity inc)}
                 :southwest {
                             :random-point (partial random-point use-offset use-zero use-size use-size-minus-offset)
                             :extract (partial extract dec inc)}
                 :west {
                        :random-point (partial random-point use-offset use-zero use-size use-size)
                        :extract (partial extract dec identity)}
                 :northwest {
                             :random-point (partial random-point use-offset use-offset use-size use-size)
                             :extract (partial extract dec dec)}})

(defn get-word [pattern]
  (map (fn [x] (random-letter)) pattern))

(defn insert-random-word [puzzle min-length max-length]
  (let [
        direction (rand-nth (keys directions))
        extract ((directions direction) :extract)
        point (((directions direction) :random-point) puzzle min-length)
        extracted (extract puzzle (get point :x) (get point :y) max-length)]
        ;word (get-word pattern)]
    (println extracted)
    puzzle))

  ;(let [
  ;      x (rand-int columns)
  ;      y (rand-int rows)
  ;      length (+ min-length (rand-int (- max-length (dec min-length))))
  ;      extract-direction (rand-extract-direction x y length)]))


(defn empty-table [rows columns]
  (into []
        (repeat rows (into []
                           (repeat columns nil)))))

(defn generate-puzzle [columns rows number-of-words min-length max-length]
  (loop [
         remaining-words number-of-words
         puzzle (empty-table rows columns)]
    (if (= remaining-words 0)
      puzzle
      (recur (dec remaining-words) (insert-random-word puzzle min-length max-length)))))

