(ns advent.six
  (:require [advent.utils :refer [read-input-lines]]
            [clojure.string :as str]))

; first quiz

(defn read-input
  [path]
  (mapv
    (fn [l]
      (set (map first (re-seq #"\S" l))))
    (read-input-lines path #"\R\R")))

(comment
  (apply + (map count (read-input "resources/six.data")))
  ;; => 6291
  )



; second quiz

(defn read-input2
  [path]
  (mapv
    (fn [l]
      (mapv set (str/split-lines l)))
    (read-input-lines path #"\R\R")))


(comment
  (apply + (mapv
             (fn [sets] (count (apply clojure.set/intersection sets)))
             (read-input2 "resources/six.data")))
  ; => 3052
  )