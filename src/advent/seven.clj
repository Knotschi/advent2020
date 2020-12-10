(ns advent.seven
  (:require [advent.utils :refer [read-input-lines]]
            [clojure.string :as str]))



(defn parse-contained-bag
  [s]
  (into {}
        (mapv (fn [[_ n b]] [b (Integer/parseInt n)])
              (re-seq #"(\d+) (.+?) bags? ?" s))))

(defn parse-line
  [s]
  (let [[_ bag contained-bags-s] (re-matches #"^(.*?) bags? contain (.* bags?)\.$" s)
        containted-bags (parse-contained-bag contained-bags-s)]
    [bag containted-bags]
    ))

(defn parse-lines
  [lines]
  (into {} (mapv parse-line lines)))

(defn read-file
  [filename]
  (parse-lines (read-input-lines filename)))

(comment
  (parse-contained-bag "1 bright white bag, 2 muted yellow bags")
  ; => {"bright white" "1", "muted yellow" "2"}

  (parse-line "light red bags contain 1 bright white bag, 2 muted yellow bags.")
  ; => ["light red" {"bright white" "1", "muted yellow" "2"}]

  (parse-lines ["light red bags contain 1 bright white bag, 2 muted yellow bags." "faded blue bags contain no other bags."])
  ; => {"light red" {"bright white" "1", "muted yellow" "2"}, "faded blue" {}}

  (read-file "resources/seven.data")

  )


;; first quiz

(defn bag-contained-in-bags
  [bags-contained-in-map bag]
  (reduce
    (fn [acc b]
      (into acc (bag-contained-in-bags bags-contained-in-map b)))
    #{bag}
    (bags-contained-in-map bag)))

(comment
  (dec (count (bag-contained-in-bags
                (bags-contained-in (read-file "resources/seven.data"))
                "shiny gold")))
  ; => 172
  )


;; second quiz

(defn bag-count
  [bags-map bag]
  (let [bags (bags-map bag)]
    (if (not-empty bags)
      (apply
        +
        (mapv
          (fn [[b cnt]]
            (* cnt (inc (bag-count bags-map b))))
          bags))
      0)))

(comment
  (bag-count (read-file "resources/seven.data") "shiny gold")
  ; => 39645
  )