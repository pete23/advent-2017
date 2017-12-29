(ns advent.day-17
  (:use clojure.test))


(defn spin [n step pos buf]
  (let [size (count buf)
        new-pos (rem (+ pos step) size)
        [before after] (split-at new-pos buf)
        new-buf (vec (concat before (vector n) after))]
    [new-pos new-buf]))
