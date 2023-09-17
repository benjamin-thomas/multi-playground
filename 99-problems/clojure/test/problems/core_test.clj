(ns problems.core-test
  (:require [clojure.test :refer :all]
            [problems.core :refer :all]))

(deftest my-last-test
  (is (= nil (my-last [])))
  (is (= 3 (my-last [1 2 3])))
  (is (= 6 (my-last '(4 5 6))))

  ; I overflow the stack with 5100 items here. For comparaison, Ruby overflows with 8800 items with a similar function.
  ; NOTE: I overflowed the stack with 4900 items on another computer.
  (is (= 4799 (my-last (range 0 4800))))
  )

(deftest my-last2-test
  (is (= nil (my-last2 [])))
  (is (= 3 (my-last2 [1 2 3])))
  (is (= 6 (my-last2 '(4 5 6))))

  ; Ok with `recur`
  (is (= 9998 (my-last2 (range 0 9999))))
  )

(deftest my-last3-test
  (is (= nil (my-last3 [])))
  (is (= 3 (my-last3 [1 2 3])))
  (is (= 6 (my-last3 '(4 5 6))))

  ; Ok with `recur`
  (is (= 9998 (my-last3 (range 0 9999))))
  )

(deftest my-last4-test
  (is (= nil (my-last4 [])))
  (is (= 3 (my-last4 [1 2 3])))
  (is (= 6 (my-last4 '(4 5 6))))

  ; Ok with `reduce`
  (is (= 9998 (my-last3 (range 0 9999))))
  )
