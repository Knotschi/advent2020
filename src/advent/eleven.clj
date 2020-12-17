(ns advent.eleven
  (:require [advent.utils :refer [read-input-lines]]
            [taoensso.encore :as enc]))


(defn read-file
  [filename]
  (mapv (fn [line] (mapv #(if (= \L %) true nil) line))
        (read-input-lines filename)))

(comment

  (defn run-sim
    [data]
    (let [w (count (first data))
          h (count data)]
      (loop [data data
             updated data
             r 0
             c 0]
        (cond
          (and (= c w) (= r h)) (if (= data updated) updated (recur updated updated 0 0))
          (= c w) (recur data updated (inc r) 0)
          :else
          (let [occ (get-in data [r c])
                adjs (apply +
                            (for [xr (range (dec r) (+ 2 r))
                                  xc (range (dec c) (+ 2 c))]
                              (if (get-in data [xr xc]) 1 0)))]
            (cond
              (nil? occ) (recur data updated r (inc c))
              (and occ (>= adjs 5)) (recur data (assoc-in updated [r c] false) r (inc c))
              (and (not occ) (= 0 adjs)) (recur data (assoc-in updated [r c] true) r (inc c))
              :else (recur data updated r (inc c)))))
        )))

  (reduce
    (fn [a r]
      (+ a (apply + (mapv #(if % 1 0) r))))
    0
    (run-sim (read-file "resources/eleven.data")))
  ; => 2438

  )


;; second quiz:

(defn read-file2
  [filename]
  (reduce-kv
    (fn [m r row]
      (reduce-kv
        (fn [m c s]
          (if (= \L s)
            (assoc m [r c] {:occ 1 :r r :c c})
            (assoc m [r c] false)))
        m (vec row)))
    {}
    (read-input-lines filename)))

(defn find-adj
  [data r c rfn cfn]
  (let [[nr nc :as k] [(rfn r) (cfn c)]]
    (let [s (data k)]
      (case s
        nil nil
        false (find-adj data nr nc rfn cfn)
        k))))

(def find-adjs
  (memoize
    (fn [data r c]
      (remove nil?
        (for [rfn [dec identity inc]
              cfn [dec identity inc]]
          (find-adj data r c rfn cfn))))))

(comment
  (find-adj (read-file2 "resources/eleven.data") 0 5 identity inc)
  ; => [0 7]

  (find-adjs (read-file2 "resources/eleven.data") 0 5)
  ; => ([0 4] [0 5] [0 7] [1 4] [1 5] [1 6])
  )

(defn pp
  "pretty prints our data, for debugging"
  [data]
  (reduce-kv
    (fn [acc [r c] {:keys [occ] :as s}]
      (assoc-in acc [r c] (if s (str occ) ".")))
    (vec (take (inc (apply max (mapv (fn [[[r _] _]] r) data))) (repeat (vec (take (inc (apply max (mapv (fn [[[_ c] _]] c) data))) (repeat nil))))))
    data))

(comment
  (pp (read-file2 "resources/eleven.data"))
  ; => e.g. [["1" "1" "0" "." "1" ...] ["0" "." "0" "." "1" ...] ...]
  )

(defn one-round
  [data]
  (enc/map-vals
    (fn [{:keys [r c occ] :as s}]
      (if s
        (let [adjs (find-adjs data r c)
              occs (apply + (mapv #(-> % data :occ (or 0)) adjs))]
          (cond
            (and (= 1 occ) (> occs 5)) (assoc s :occ 0)
            (and (= 0 occ) (= 0 occs)) (assoc s :occ 1)
            :else s)
          )
        s))
    data))

(comment

  (let [new-data
        (loop [data (read-file2 "resources/eleven.data")]
                   (let [nd (one-round data)]
                     (if (= nd data)
                       data
                       (recur nd))))]
    (apply +
           (mapv #(-> % val :occ (or 0)) new-data)))
  ; => 2174

  )