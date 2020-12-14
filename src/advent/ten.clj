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

