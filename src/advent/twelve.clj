(ns advent.twelve
  (:require [advent.utils :refer [read-input-lines]]
            [taoensso.encore :as enc]))

(def dc
  {:N :E
   :E :S
   :S :W
   :W :N})

(defn parse-lines
  [lines]
  (mapv (fn [[c & v]]
          (let [v (Integer/parseInt (apply str v))
                c (keyword (str c))]
            (if (#{:R :L} c)
              [:T (mod (+ 4 (({:R + :L -} c) (/ v 90))) 4)]
              [c v])))
        lines))

(defn read-file
  [filename]
  (parse-lines (read-input-lines filename)))

(comment

  ; quiz one:
  (let [[x y]
        (reduce
          (fn [[x y d] [c v]]
            (let [c (if (= :F c) d c)]
              (case c
                :E [(+ x v) y d]
                :S [x (+ y v) d]
                :W [(- x v) y d]
                :N [x (- y v) d]
                :T [x y (nth (iterate dc d) v)])))
          [0 0 :E]
          (read-file "resources/twelve.data"))]
    (+ (Math/abs x) (Math/abs y)))
  ; => 1603


  ; second quiz:

  (defn rotate [[x y]] [(- y) x])

  (let [[[x y]]
        (reduce
          (fn [[[sx sy :as s] [wx wy]] [c v]]
            (case c
              :E [s [(+ wx v) wy]]
              :S [s [wx (+ wy v)]]
              :W [s [(- wx v) wy]]
              :N [s [wx (- wy v)]]
              :T [s (nth (iterate rotate [wx wy]) v)]
              :F [[(+ sx (* v wx)) (+ sy (* v wy))] [wx wy]]))
          [[0 0] [10 -1]]
          (read-file "resources/twelve.data"))]
    (+ (Math/abs x) (Math/abs y)))
  ; => 52866
  )

