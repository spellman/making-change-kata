(ns making-change-kata.core-test
  (:require [clojure.test :refer :all]
            [making-change-kata.core :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]))

;; Easy, spanning coins
;; =============================================================================
(def us-coins [1 5 10 25])

(deftest return-penny-for-1
  (is (= {1 1}
         (amount->coins us-coins 1))))

(deftest return-2-pennies-for-2
  (is (= {1 2}
         (amount->coins us-coins 2))))

(deftest return-1-nickel-for-5
  (is (= {5 1}
         (amount->coins us-coins 5))))

(deftest return-1-dime-for-10
  (is (= {10 1}
         (amount->coins us-coins 10))))

(deftest return-1-quarter-for-25
  (is (= {25 1}
         (amount->coins us-coins 25))))

(deftest return-change-for-one-of-each-coin
  ;; 1x1, 1x5, 1x10, 1x25 => 41
  (is (= {1 1, 5 1, 10 1, 25 1}
         (amount->coins us-coins 41))))



;; Non-spanning coins
;; =============================================================================
(def non-spanning-coins [2 7])

(deftest return-nil-when-cannot-make-change
  (is (nil? (amount->coins non-spanning-coins 1))))



;; Tricky coins
;; =============================================================================
(def tricky-coins [1 20 25])

;; Note that a naive solution of using as many as possible of successively
;; smaller coins will fail to provide a minimal number of coins here,
;; yielding {1 15, 25 1} = 16 coins instead of the correct {20 2} = 2 coins.
(deftest number-of-coins-returned-is-minimal
  (is (= {20 2}
         (amount->coins tricky-coins 40))))

(defspec amount-of-coins-equals-original-amount
  10
  (prop/for-all [available-coins (gen/not-empty (gen/vector gen/s-pos-int))]
                (let [amount (->> available-coins
                                  (map (partial * (inc (rand-int (count available-coins)))))
                                  (reduce + 0))
                      coins (amount->coins3 us-coins amount)
                      coins-amount (coins->amount coins)]
                  (= amount coins-amount))))
