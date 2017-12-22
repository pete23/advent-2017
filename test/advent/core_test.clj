(ns advent.core-test
  (:require [clojure.test :refer :all]
            [advent.core :refer :all]))

(deftest spiral-to-cartesian-coordinates
  (is (= '([0 0] [1 0] [1 1] [0 1] [-1 1] [-1 0] [-1 -1] [0 -1] [1 -1] [2 -1] [2 0] [2 1] [2 2] [1 2] [0 2] [-1 2] [-2 2] [-2 1] [-2 0] [-2 -1] [-2 -2] [-1 -2] [0 -2] [1 -2] [2 -2] [3 -2])
         (map spiral-to-cartesian (range 26)))))
