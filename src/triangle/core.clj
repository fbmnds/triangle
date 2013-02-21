;(ns triangle.core)



;; Love Triangle #127
;;
;; Difficulty:	Hard
;; Topics:	search data-analysis
;;
;;
;; Everyone loves triangles, and it's easy to understand why they're so wonderfully symmetric
;; (except scalenes, they suck).
;;
;; Your passion for triangles has led you to become a miner (and part-time Clojure programmer)
;; where you work all day to chip out isosceles-shaped minerals from rocks gathered in a nearby
;; open-pit mine. There are too many rocks coming from the mine to harvest them all so you've been
;; tasked with writing a program to analyze the mineral patterns of each rock, and determine which
;; rocks have the biggest minerals.
;;
;; Someone has already written a computer-vision system for the mine. It images each rock as it
;; comes into the processing centre and creates a cross-sectional bitmap of mineral (1) and
;; rock (0) concentrations for each one.
;;
;; You must now create a function which accepts a collection of integers, each integer when read
;; in base-2 gives the bit-representation of the rock (again, 1s are mineral and 0s are worthless
;; scalene-like rock).
;;
;; You must return the cross-sectional area of the largest harvestable mineral from the input rock,
;; as follows:
;; - The minerals only have smooth faces when sheared vertically or horizontally from the rock's
;;   cross-section
;; - The mine is only concerned with harvesting isosceles triangles (such that one or two sides
;;   an be sheared)
;; - If only one face of the mineral is sheared, its opposing vertex must be a point
;;   (ie. the smooth face must be of odd length), and its two equal-length sides must intersect
;;   the shear face at 45° (ie. those sides must cut even-diagonally)
;; - The harvested mineral may not contain any traces of rock
;; - The mineral may lie in any orientation in the plane
;; - Area should be calculated as the sum of 1s that comprise the mineral
;; - Minerals must have a minimum of three measures of area to be harvested
;; - If no minerals can be harvested from the rock, your function should return nil

(defn int2bit [n]
  (letfn [(int2bit-r
            [n]
            (if (< n 2) (list n) (conj (int2bit-r (quot n 2)) (rem n 2))))]
    (vec (reverse (int2bit-r n)))))


;; (defn do-matrix [v]
;;   (let [m (map int2bit v)
;;         dim-m (apply max (map count m))]
;;     (vec (for [i m :let [p (repeat (- dim-m (count i)) 0)]]
;;            (if (empty? p)
;;              [i]
;;              (vec (concat p i)))))))

(defn make-matrix [v]
  (let [m (map int2bit v)
        dim-m (apply max (map count m))]
    (vec (for [i m :let [p (repeat (- dim-m (count i)) 0)]]
           (if (empty? p)
             i
             (vec (concat p i)))))))

(defn make-loc [m]
  (vec (filter #(not (nil? %))
               (for [i (range (count m)), j (range (count (first m)))]
                 (if (= 1 (get-in m [i j]))
                   [i j]
                   nil)))))


(defn shift-shape [s loc]
  (vec (map (fn [v] [(+ (first v) (first loc)) (+ (last v) (last loc))]) s)))

;; e.g., shape = [[0 0] [0 1] [0 2] [1 0] [1 1] [2 0]], loc = [4 5]
;;
(defn shape-in-matrix-at-loc? [m s loc]
  (let [shifted-s (set (shift-shape s loc))]
    (clojure.set/subset? shifted-s (set (make-loc m)))))


(defn shape-in-matrix? [m s]
  (reduce #(or %1 %2)
          false
          (for [i (range (count m)), j (range (count (first m)))]
            (shape-in-matrix-at-loc? m s [i j]))))

(defn shape-in-matrix? [m s]
  (for [i (range (count m)), j (range (count (first m)))]
    (shape-in-matrix-at-loc? m s [i j])))


(defn shape [n]
  (cond (zero? n) [[0 0] [1 0] [1 1]]
        :else (vec (concat (shape (dec n))
                           (for [i (range (+ n 2))] [(inc n) i])))))


(defn transpose [m]
  (vec (apply map vector m)))

(defn rot-90 [m]
  (vec (map #(vec (reverse %)) (transpose m))))
