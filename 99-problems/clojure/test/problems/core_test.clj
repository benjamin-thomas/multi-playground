(ns problems.core-test
  (:require [clojure.test :refer :all]
            [problems.core :refer :all]))

(deftest my-last-test
  (is (= 3 (my-last [1 2 3])))
  (is (= 6 (my-last '(4 5 6))))
  )

(deftest my-last2-test
  (is (= 3 (my-last2 [1 2 3])))
  (is (= 6 (my-last2 '(4 5 6))))
  )

(deftest my-last3-test
  (is (= 3 (my-last3 [1 2 3])))
  (is (= 6 (my-last3 '(4 5 6))))
  )
