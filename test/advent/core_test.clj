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

