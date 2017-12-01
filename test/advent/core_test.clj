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
