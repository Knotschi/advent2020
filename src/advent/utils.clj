(ns advent.utils
  (:require [clojure.string :as str]))

(defn read-input-lines
  ([filename]
   (read-input-lines filename #"\R"))
  ([filename separator]
   (-> filename
       slurp
       (str/split separator))))
