(ns advent.eight
  (:require [advent.utils :refer [read-input-lines]]
            [clojure.string :as str]))



(defn parse-line
  [s]
  (let [[_ command val] (re-matches #"(.{3}) (.\d+)" s)]
    [command (Integer/parseInt val)]))

(defn parse-lines
  [lines]
  (mapv parse-line lines))

(defn read-file
  [filename]
  (parse-lines (read-input-lines filename)))

(comment
  (read-file "resources/eight.data"))

(defn run-code
  "return false when failing, otherwise value of acc
  if debug? is set to true, it will also return last value of acc when failing"
  ([lines] (run-code lines false))
  ([lines debug?]
   (loop
     [line 0
      acc 0
      visited #{}]
     (cond (visited line) (if debug? acc false)
           (= line (count lines)) acc
           :else
           (let [[command val] (get lines line)
                 new-visited (conj visited line)
                 new-line (inc line)]
             (case command
               "nop" (recur new-line acc new-visited)
               "acc" (recur new-line (+ acc val) new-visited)
               "jmp" (recur (+ line val) acc new-visited)))))))

(comment
  (run-code (read-file "resources/eight.data"))
  ; => false

  ; first quiz:
  (run-code (read-file "resources/eight.data") true)
  ; => 1179
  )


; second quiz

(defn fix-code
  [lines]
  (let [nop-jmp-lines ;; first list all "nop" and "jmp" lines
        (reduce-kv
          (fn [nops line [command _]]
            (if (#{"nop" "jmp"} command)
              (conj nops line)
              nops))
          #{}
          lines)]
    (reduce ;; for each of these swap "jmp" and "nop" and run the code, if successfull return result of new code
      (fn [_ line-to-change]
        (let [new-code
              (update lines line-to-change
                         (fn [[com val]] (if (= "nop" com) ["jmp" val] ["nop" val])))
              result (run-code new-code)]
          (when result
            (reduced result))
          ))
      nop-jmp-lines)
    ))

(comment
  (fix-code (read-file "resources/eight.data")))