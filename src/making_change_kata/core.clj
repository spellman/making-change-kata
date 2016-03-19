(ns making-change-kata.core
  (:require [clojure.core.reducers :as r]
            [clojure.math.combinatorics :as comb]))

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

;; 1. Filter the cartesian product of all possible numbers of each coin.
(defn amount->coins [available-coins amount]
  (when-let [coin-freqs (->> available-coins
                             (map (partial possible-numbers-of-coin amount))
                             (apply comb/cartesian-product)
                             (filter #(= amount (coins->amount %)))
                             (seq))]
    (->> coin-freqs
         (apply min-key number-of-coins)
         (remove (fn [[_ number]] (zero? number)))
         (into {}))))

(defn sgn [x]
  (cond (pos? x) 1
        (neg? x) -1
        :else 0))

;; 2. Recursively deduct one coin of each type from the amount and track the
;;    number of each coin.
(defn amount->coins2 [available-coins amount]
  (loop [unfinished-amount-freqs-tuples #{[amount (zipmap available-coins (repeat 0))]}
         finished-freqs #{}]
    (let [new-coin-tuples (for [[amount freq] unfinished-amount-freqs-tuples
                                coin available-coins]
                            [(- amount coin) (update freq coin inc)])
          new-by-amt-sgn (group-by (fn [[amount _]] (sgn amount)) new-coin-tuples)
          new-unfinished (set (new-by-amt-sgn 1))
          new-finished (new-by-amt-sgn 0)
          freqs (into finished-freqs (map second new-finished))]
      (if (seq new-unfinished)
        (recur new-unfinished freqs)
        (when (seq freqs)
          (->> freqs
               (apply min-key number-of-coins)
               (remove (fn [[_ number]] (zero? number)))
               (into {})))))))

;; 3. Use reducers for a bit of a performance improvement.
;;    The cost of using sets (not foldable) seems to outweigh the cost of having
;;    more elements in vectors (foldable).
(defn amount->coins3 [available-coins amount]
  (loop [unfinished-amount-freqs-tuples #{[amount (zipmap available-coins (repeat 0))]}
         finished-freqs #{}]
    (let [new-coin-tuples (r/foldcat (r/mapcat (fn [[amount freq]]
                                                 (r/map (fn [coin]
                                                          [(- amount coin) (update freq coin inc)])
                                                        available-coins))
                                               unfinished-amount-freqs-tuples))
          new-unfinished (into #{} (r/filter (fn [[amount _]] (pos? amount))
                                             new-coin-tuples))
          freqs (into finished-freqs (r/map second
                                            (r/filter (fn [[amount _]] (zero? amount))
                                                      new-coin-tuples)))]
      (if (seq new-unfinished)
        (recur new-unfinished freqs)
        (when (seq freqs)
          (->> freqs
               (apply min-key number-of-coins)
               (remove (fn [[_ number]] (zero? number)))
               (into {})))))))
