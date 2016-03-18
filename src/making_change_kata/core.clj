(ns making-change-kata.core
  (:require [clojure.math.combinatorics :as comb]))

(defn possible-numbers-of-coin [amount coin]
  (map (partial vector coin)
       (range (inc (quot amount coin)))))

(defn coins->amount [coin-frequency-map]
  (reduce (fn [acc [coin number]] (+ acc (* coin number)))
          0
          coin-frequency-map))

(defn number-of-coins [coin-frequency-map]
  (reduce (fn [acc [_ number]] (+ acc number))
          0
          coin-frequency-map))

(defn amount->coins [coins amount]
  (when-let [coins-sets (->> coins
                             (map (partial possible-numbers-of-coin amount))
                             (apply comb/cartesian-product)
                             (filter #(= amount (coins->amount %)))
                             (map #(remove (fn [[_ number]] (zero? number)) %))
                             (map (partial into {}))
                             (seq))]
    (apply min-key number-of-coins coins-sets)))
