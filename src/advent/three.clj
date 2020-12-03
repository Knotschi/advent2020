(ns advent.three
  (:require [advent.utils :refer [read-input-lines]]))

(defn parse-line [s]
  (->>
    s
    (mapv #(if (= \# %) 1 0))
    repeat
    flatten))

(defn read-input
  [path]
  (mapv parse-line (read-input-lines path)))

(def input
  (map parse-line (read-input-lines "resources/three.data")))

(defn count-trees
  [input slope-step]
  (:cnt
    (reduce
      (fn [{:keys [pos cnt]} line]
        (let [n (nth line pos)]
          {:pos (+ slope-step pos)
           :cnt (+ cnt n)}))
      {:pos 0 :cnt 0}
      input)))

(comment
  (def input (read-input "resources/three.data"))

  ;; Part 1:
  (count-trees input 3)
  ; => 198


  ;; Part 2
  (*
    (count-trees input 1)
    (count-trees input 3)
    (count-trees input 5)
    (count-trees input 7)
    (count-trees (take-nth 2 input) 1)
    )

  ; => => 5140884672
  )
