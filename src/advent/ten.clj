(ns advent.ten
  (:require [advent.utils :refer [read-input-lines]]
            [clojure.string :as str]))

(defn read-file
  [filename]
  (sort (map #(Integer/parseInt %) (read-input-lines filename))))



;; first quiz
(defn find-one-three-jolts
  [nums]
  (reduce
    (fn [{:keys [p ones threes]} n]
      (case (- n p)
        1 {:p n :ones (inc ones) :threes threes}
        3 {:p n :ones ones :threes (inc threes)}
        {:p n :ones ones :threes threes}))
    {:p (first nums) :ones 1 :threes 1}
    nums))

(comment
  (let [{:keys [ones threes]} (find-one-three-jolts (read-file "resources/ten.data"))]
    (* ones threes))
  ; => 1856
  )


;; second quiz

(defn count-options
  "given a list of jolts
  returns the amount of distinct arrangements
  attention:
  this can get very expensive and our limited recursive call stack can exceed when list is too long
  only use with with short lists instead"
  ([nums]
   (count-options nums (dec (first nums))))
  ([nums p]
   (let [x (first nums)
         r (rest nums)
         n (first r)]
     (if (not n)
       1
       (if (<= (- n p) 3)
         (+ (count-options r p) (count-options r x))
         (count-options r x))))))

(comment
  (count-options [1 2 3 4])
  ; => 7

  (count-options [1 4])
  ; => 1

  (count-options [1 2 4])
  ; => 3
  )

(defn split-adapter-groups
  "given a vector of adapters
  returns a vector of vectors, where each vector is a subvec of the original adaopter vector
  we always split at places where we find an adapter which is not optional

  we can than count the options of each group and just multiply all groups to get the full amount of distinct arrangements"
  [nums]
  (loop [r   nums
         p   0
         acc []
         res []]
    (let [x  (first r)
          nr (rest r)
          na (vec (sort (conj acc x)))
          n  (first nr)]
      (if n
        (if (<= (- n p) 3)
          (recur nr x na res)
          (recur nr x [] (conj res na)))
        (conj res na)))))

(comment
  (split-adapter-groups [1 4 5 7 8 10 12 13 14 16 18 19 20 22])
  ; => [[1] [4] [5 7 8 10] [12 13 14 16] [18 19 20 22]]


  (apply * (map #(count-options %)
                (split-adapter-groups (read-file "resources/ten.data"))))

  ; => 2314037239808

  )



;; alternative O(n) solution for second quiz:
;; considering that we actually never have gaps of 2 jolts between adapters
;; but only gaps of 1 or 3 jolts
;; we can also use the tribonacci sequence to calculate the distinct combinations
;; why can we do that? I have no clue. It was just a observation I made.
;; I am sure there is a smart logical mathematical reason for that.

;; the first part of this solution
;; which is changing the list of adapter jolts to a list of jolt difference to its previous adapter
;; from here: https://www.youtube.com/watch?v=1CPuvWvJMFo

;; the tribonacci sequence
;; although we could use a function to calculate them to als support higher values
;; but we actually only need pretty low tribonacci numbers for our quiz, so I just hardcode the first ones:
(def trib [1, 1, 2, 4, 7, 13, 24, 44, 81, 149])

(defn count-options-trib
  [jolts]                                                   ; [1 4 5 8 11 12 13 14 17 18 19 20]
       ; prepend the outlet (adapter with 0 jolts):
  (->> (into [0] jolts)                                     ; [0 1 4 5 8 11 12 13 14 17 18 19 20]
       ; for each adapter we need the jolts difference to its previous adapter:
       (partition 2 1)                                      ; ((0 1) (1 4) (4 5) (5 8) (8 11) (11 12) (12 13) (13 14) (14 17) (17 18) (18 19) (19 20) (20 23))
       (mapv #(->> % reverse (apply -)))                    ; [1 3 1 3 3 1 1 1 3 1 1 1 3]
       ; replacing the 3s with 0s because a difference of 3 means that adapter does not add any new configuration options:
       (mapv {3 0 1 1})                                     ; [1 0 1 0 0 1 1 1 0 1 1 1 0]
       ; grouping by same values:
       (partition-by identity)                              ; ((1) (0) (1) (0 0) (1 1 1) (0) (1 1 1) (0))
       ; replacing the groups of 0s with 1 and
       ; replacing each group of 1s with the tribonacci number of its count:
       (mapv #(trib (apply + %)))                           ; [1 1 1 1 4 1 4 1]
       ; multiplicate all these values:
       (reduce *)))                                         ; 16

(comment
  (count-options-trib [1 4 5 8 11 12 13 14 17 18 19 20])
  ; => 16
  (count-options-trib (read-file "resources/ten.data"))
  ; => 2314037239808

  )