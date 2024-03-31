(ns aoc.core-test
  (:require [clojure.test :refer :all]
            [aoc.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 1 1))))

(deftest add-test
  (is (= 3 (add 1 2))))

(deftest mul-test
  (is (= 13 (mul 3 4))))
