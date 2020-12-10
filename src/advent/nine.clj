(ns advent.nine
  (:require [advent.utils :refer [read-input-lines]]
            [clojure.string :as str]))

(defn read-file
  [filename]
  (mapv bigdec (read-input-lines filename)))

; first quiz

(defn sumable?
  "returns true if any 2 numbers in nums can be summed up to the number n"
  [n nums]
  (some
    #(= n (+ (first %) (second %)))
    (for [a nums
          b nums]
      [a b])))

(defn find-wrong-number
  [nums preamble]
  (reduce-kv
    (fn [_ i n]
      (if (< i preamble)
        nil
        (when-not (sumable? n (subvec nums (- i preamble) i))
          (reduced n))))
    nil
    nums))


(comment
  (find-wrong-number (read-file "resources/nine.data") 25)
  ; => 138879426M
  )

; second quiz

(defn check-sum
  [nums n]
  (reduce-kv
    (fn [acc idx num]
      (let [acc (conj acc num)
            sum (apply + acc)]
        (cond
          (= sum n) (reduced (+ (apply min acc) (apply max acc)))
          (> sum n) (reduced false)
          (< sum n) acc)))
    [(first nums)]
    (vec (rest nums))))

(check-sum [20 30 1 2 10] 63)

(defn find-sum
  [nums num]
  (reduce
    (fn [_ idx]
      (when-let [c (check-sum (subvec nums idx) num)]
        (reduced c)))
    nil
    (range (count nums))))

(comment
  (find-sum (read-file "resources/nine.data") 138879426M)
  ; => 23761694M
  )