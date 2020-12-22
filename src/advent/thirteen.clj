(ns advent.thirteen
  (:require [advent.utils :refer [read-input-lines]]
            [clojure.string :as str]))

; first quiz:

(defn fastest-bus
  [filename]
  (apply *
    (let [[ts bids] (read-input-lines filename)
          ts (Integer/parseInt ts)
          bids (mapv #(when (not= "x" %) (Integer/parseInt %)) (str/split bids #","))]
      (reduce
        (fn [[mv :as old] bid]
          (if bid
            (let [nv (- bid (mod ts bid))]
              (if (< nv mv)
                [nv bid]
                old))
            old))
        [(Integer/MAX_VALUE) false]
        bids))))

(comment
  (fastest-bus "resources/thirteen.data")
  ; => 4722
  )



;; second quiz:

(defn bus-map []
  (reverse
    (into (sorted-map)
          (reduce-kv
            (fn [m i v]
              (if (= "x" v)
                m
                (assoc m (Integer/parseInt v) i)))
            {}
            (str/split (second (read-input-lines "resources/thirteen.data")) #",")))))

(comment
  ; => ([787 48] [523 17] [41 7] [37 54] [29 77] [23 40] [19 36] [17 0] [13 35])
  )


(defn calc-bus-ts
  [buses]
  (loop [pos 0
         fac 1
         buses buses]

    (if (empty? buses)
      pos
      (let [[n p] (first buses)
            npos
            (reduce
              (fn [_ x]
                (let [v (+ pos (* fac x))]
                  (when (= 0 (mod (+ v p) n))
                    (reduced v))))
              (range))]
        (if-let [rb (rest buses)]
          (recur npos (* fac n) rb)
          npos)))

    ))

(comment
  (calc-bus-ts (bus-map))
  ; => 825305207525452

  )