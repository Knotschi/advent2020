(ns advent.one
  (:require [advent.utils :refer [read-input-lines]]))

(defn read-data [filename]
  (into #{} (mapv #(Integer/parseInt %) (read-input-lines filename))))

(read-data "resources/one.data")

(defn find-sum
  "returns the first two elements in `values` whos sum is
   equal to `check-num`"
  [values check-num]
  (reduce
    (fn [_ n]
      (when-let [n2 (values (- 2020 n))]
        (reduced (* n n2))))
    values))

(defn find-sum-2
  "returns the first three elements in `values` whos sum is
   equal to `check-num`"
  [values check-num]
  (reduce
    (fn [_ [n1 n2]]
      (when-let [n3 (values (- 2020 n1 n2))]
        (reduced (* n1 n2 n3))))
    (for [a values
          b values]
      [a b])))

(defn solution-1 []
  (find-sum (read-data "resources/one.data") 2020))

(defn solution-2 []
  (find-sum-2 (read-data "resources/one.data") 2020))

(comment
  ;; solution for day 1, problem 1
  (solution-1) ;; => 1016964
  (solution-2) ;; => 182588480
  )
