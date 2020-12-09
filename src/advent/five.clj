(ns advent.five
  (:require [advent.utils :refer [read-input-lines]]
            [clojure.string :as str]))



;; first quiz:

(defn seat->id
  [s]
  (-> s
      (str/replace #"F|L" "0")
      (str/replace #"B|R" "1")
      (Integer/parseInt 2)))

(comment
  (seat->id "FBFBBFFRLR")
  ; => 357

  )


(def used-seats
  (set (mapv seat->id (read-input-lines "resources/five.data"))))


(comment
  (apply max used-seats)
  ;; => 828
  )

;; second quiz
(comment
  (reduce
    (fn [_ sid]
      (when (and
              (not (used-seats sid))
              (used-seats (dec sid))
              (used-seats (inc sid)))
        (reduced sid)))
    (range 2 827))
  ; => 565
  )