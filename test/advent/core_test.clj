(ns advent.core-test
  (:require [clojure.test :refer :all]
            [advent.core :refer :all]))

(deftest advent-1-1-1122
  (is (= 3 (advent-1-1 "1122"))))

(deftest advent-1-1-1111
  (is (= 4 (advent-1-1 "1111"))))

(deftest advent-1-1-1234
  (is (= 0 (advent-1-1 "1234"))))

(deftest advent-1-1-91212129
  (is (= 9 (advent-1-1 "91212129"))))

(deftest advent-1-2-1212
  (is (= 6 (advent-1-2 "1212"))))

(deftest advent-1-2-1221
  (is (= 0 (advent-1-2 "1221"))))

(deftest advent-1-2-123425
  (is (= 4 (advent-1-2 "123425"))))

(deftest advent-1-2-123123
  (is (= 12 (advent-1-2 "123123"))))

(deftest advent-1-2-12131415
  (is (= 4 (advent-1-2 "12131415"))))

(deftest advent-2-1-example
  (is (= 18 (advent-2-1 [[5 1 9 5] [7 5 3] [2 4 6 8]]))))

(deftest advent-2-2-example
  (is (= 9 (advent-2-2 [[5 9 2 8] [9 4 7 3] [3 8 6 5]]))))

(deftest spiral-to-cartesian-coordinates
  (is (= '([0 0] [1 0] [1 1] [0 1] [-1 1] [-1 0] [-1 -1] [0 -1] [1 -1] [2 -1] [2 0] [2 1] [2 2] [1 2] [0 2] [-1 2] [-2 2] [-2 1] [-2 0] [-2 -1] [-2 -2] [-1 -2] [0 -2] [1 -2] [2 -2] [3 -2])
         (map spiral-to-cartesian (range 26)))))
