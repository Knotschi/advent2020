(ns advent.two
  (:require [advent.utils :refer [read-input-lines]]))

(def line-regex #"^(\d+)-(\d+) (\w): (\w+)$")

(comment
  (re-find line-regex "15-19 k: kkkkkkkkkkkkzkkkkkkk")
  ;; => ["15-19 k: kkkkkkkkkkkkzkkkkkkk" "15" "19" "k" "kkkkkkkkkkkkzkkkkkkk"]

  )

(defn parse-line
  "a line of input is composed by:
  - a range in the form of min-max
  - a blank space
  - a char which must be present at least min and at most max times in the password
  - a column character
  - a blank space
  - and finally the password"
  [line]
  (let [[_ min max char password] (re-find line-regex line)]
    [[(Integer/parseInt min) (Integer/parseInt max)] (get char 0) password]))

(comment
  (parse-line "15-19 k: kkkkkkkkkkkkzkkkkkkk")
  ;; => [[15 19] \k "kkkkkkkkkkkkzkkkkkkk"]
  )

(defn read-data [filename]
  (mapv parse-line (read-input-lines filename)))

(defn valid-passwords
  [passwords]
  (reduce
    (fn [acc [[min max] c pw]]
      (if (<= min ((frequencies pw) c 0) max)
        (inc acc)
        acc))
    0
    passwords))

(defn valid-passwords2
  [passwords]
  (reduce
    (fn [acc [[min max] c pw]]
      (let [a (= c (get pw (dec min)))
            b (= c (get pw (dec max)))]
        (if (or (and a (not b))
                (and (not a) b))
          (inc acc)
          acc)))
    0
    passwords))


(comment
  ;; solution 1
  (->> "resources/two.data"
    read-data
    valid-passwords) ;; => 655

  ;; solution 2
  (->> "resources/two.data"
    read-data
    valid-passwords2) ;; => 673
)
