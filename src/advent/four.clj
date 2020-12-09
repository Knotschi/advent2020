(ns advent.four
  (:require [advent.utils :refer [read-input-lines]]
            [clojure.string :as str]))

(defn parse-line [s]
  (reduce
    (fn [m [_ k v]] (assoc m k v))
    {}
    (re-seq #"(\S+):(\S+)" s)))

(defn read-input
  [path]
  (mapv parse-line (read-input-lines path #"\R\R")))

;; first quiz

(def required #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"})

(defn contains-all-keys?
  [m]
  (clojure.set/superset? (set (keys m)) required))

(comment
  (count (filter contains-all-keys? (read-input "resources/four.data")))
  ; => 237
  )




;; second quiz

(def validators
  {"byr" (fn [s] (<= 1920 (Integer/parseInt (first (re-seq #"^\d{4}$" s))) 2002))
   "iyr" (fn [s] (<= 2010 (Integer/parseInt (first (re-seq #"^\d{4}$" s))) 2020))
   "eyr" (fn [s] (<= 2020 (Integer/parseInt (first (re-seq #"^\d{4}$" s))) 2030))
   "hgt" (fn [s]
           (let [[_ _ cms ins] (first (re-seq #"^((\d+)cm|(\d+)in)$" s))
                 cm (if cms (Integer/parseInt cms) 0)
                 in (if ins (Integer/parseInt ins) 0)]
             (or (<= 150 cm 193)
                 (<= 59 in 76))))
   "hcl" (fn [s] (boolean (re-matches #"^#[0-9a-f]{6}$" s)))
   "ecl" #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}
   "pid" (fn [s] (boolean (re-matches #"^\d{9}$" s)))
   })


(defn valid?
  [m]
  (every?
    (fn [[k v?]]
      (when-let [val (m k)]
        (v? val)))
    validators))



(comment
  (count (filter valid? (read-input "resources/four.data")))
  ; => 172
  )