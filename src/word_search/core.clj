(ns word-search.core
  (:require [clojure.string :as str]))

(def random-item (comp first shuffle))

(def words (str/split-lines (str/upper-case (slurp "words.txt"))))

(defn random-letter []
  (random-item (map (comp str char) (range 65 91))))

(defn random-word [min-length max-length]
  (random-item (filter (fn [word] (and (>= (count word) min-length)
                                       (<= (count word) max-length)))
                       words)))

(defn rand-range [min max]
  (rand-nth (range min max)))

(defn puzzle-bounds [puzzle]
  {
   :left 0
   :top 0
   :right (dec (count (get puzzle 0)))
   :bottom (dec (count puzzle))})

(defn offset-bounds [puzzle offset-from offset]
  (let [
        bounds (puzzle-bounds puzzle)
        offset (dec offset)]
    {:left (if (.contains offset-from :left)
             (+ (bounds :left) offset)
             (bounds :left))
     :top (if (.contains offset-from :top)
            (+ (bounds :top) offset)
            0)
     :right (if (.contains offset-from :right)
              (- (bounds :right) offset)
              (bounds :right))
     :bottom  (if (.contains offset-from :bottom)
                (- (bounds :bottom) offset)
                (bounds :bottom))}))


;; a generic function for extracting sets of characters from a table of characters
(defn extract [fnx fny table point length]
  (loop [
         x (get point :x)
         y (get point :y)
         remaining length
         result []]
    (cond
      (= remaining 0) result
      ;; find the next letter in the table
      :else (recur
              (fnx x)
              (fny y)
              (dec remaining)
              (conj result {:x x :y y :value (get-in table [y x])})))))

;; picks a random point using the bounds defined by the provided functions
(defn random-point [bounds]
  {:x (rand-range (bounds :left) (inc (bounds :right)))
   :y (rand-range (bounds :top) (inc (bounds :bottom)))})

(def directions {
                 :north {
                         :offsets [:top]
                         :extract (partial extract identity dec)}
                 :northeast {
                             :offsets [:top :right]
                             :extract (partial extract inc dec)}
                 :east {
                        :offsets [:right]
                        :extract (partial extract inc identity)}
                 :southeast {
                             :offsets [:bottom :right]
                             :extract (partial extract inc inc)}
                 :south {
                         :offsets [:bottom]
                         :extract (partial extract identity inc)}
                 :southwest {
                             :offsets [:bottom :left]
                             :extract (partial extract dec inc)}
                 :west {
                        :offsets [:left]
                        :extract (partial extract dec identity)}
                 :northwest {
                             :offsets [:top :left]
                             :extract (partial extract dec dec)}})

(defn insertable-word [extracted]
  (let [
        pattern-string (str "^" (str/join (map #(or (% :value) ".") extracted)) "$")
        pattern (re-pattern pattern-string)
        words (filter #(re-matches pattern %) words)
        word (if (> (count words) 0) (rand-nth words) nil)]
    (if (nil? word)
      nil
      (let [
            letters (str/split word #"")]
           (map-indexed (fn [i ext] (assoc ext :value (get letters i))) extracted)))))


(defn insert-random-word [puzzle min-length max-length]
  (let [
        direction-key (rand-nth (keys directions))
        direction (directions direction-key)
        length (rand-range min-length (inc max-length))
        bounds (offset-bounds (puzzle :table) (direction :offsets) length)
        point (random-point bounds)
        extracted ((direction :extract) (puzzle :table) point length)
        insertable (insertable-word extracted)]
    (if (not insertable)
      puzzle
      {:table (loop [
                     insertable insertable
                     table (puzzle :table)]
                (if (= (count insertable) 0)
                  table
                  (let [
                        item (first insertable)
                        table (assoc-in table [(item :y) (item :x)] (item :value))]
                    (recur (rest insertable) table))))
       :words (conj (puzzle :words) {:word (str/join (map :value insertable))
                                     :direction direction-key
                                     :start {:x ((first insertable) :x)
                                             :y ((first insertable) :y)}})})))

(defn empty-table [rows columns]
  (into []
        (repeat rows (into []
                           (repeat columns nil)))))

(defn empty-puzzle [rows columns]
  {
   :table (empty-table rows columns)
   :words []})

(defn random-letter-if-nil [letter]
  (or letter (random-letter)))

(defn fill-nils [puzzle]
  (assoc puzzle :table (map #(map random-letter-if-nil %) (puzzle :table))))

(defn generate-puzzle [columns rows number-of-words min-length max-length]
  (fill-nils (loop [
                      puzzle (empty-puzzle rows columns)
                      tries (* number-of-words 20)]
                 (if (or (= (count (puzzle :words)) number-of-words) (= tries 0))
                   puzzle
                   (recur (insert-random-word puzzle min-length max-length) (dec tries))))))

