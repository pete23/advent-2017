(ns advent.day-13
  (:use clojure.test))


(defn integerize-string [string]
  (mapv #(Integer/parseInt %)
        (clojure.string/split
         (clojure.string/replace string #":" "") #"\s+")))

(defn read-file [filename]
  (->> filename
       (slurp)
       (clojure.string/split-lines)
       (map integerize-string)
       (into {})))

(def input (read-file "input/day-13.txt"))

(def test-input {0 3, 1 2, 4 4, 6 4})

(defn layer-severity [[depth range]]
  ;; severity is depth * range iff the layer is hit
  ;; if we treat the layer as a modulo round then we hit when rem depth (2range-2) == 0
  (if (= 0 (rem depth (- (* 2 range) 2)))
    (* depth range)
    0))

(defn total-severity [firewall]
  (reduce + (map layer-severity firewall)))

(deftest part-1-example
  (is (= 24 (total-severity test-input))))

(defn part-1 [] (total-severity input))
