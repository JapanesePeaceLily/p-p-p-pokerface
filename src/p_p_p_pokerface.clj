(ns p-p-p-pokerface)

(defn rank [card]
  (def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (replacements rank))))

(defn suit [card]
  (str (let [[_ suit] card]
         suit)))

(defn pair? [hand]
  (< 1 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (< 2 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (< 3 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (let [freqs (vals (frequencies (map rank hand)))]
    (or
     (= 4 (apply max freqs))
     (= [2 2 1] freqs))))

(defn straight? [hand]
  (let [hand-values (sort (map rank hand))]
    (let [first-value (first hand-values)]
      (let [first-value-range (range first-value (+ first-value 5))]
        (let [replaced-values (sort (replace {14 1} hand-values))]
          (let [first-replaced (first replaced-values)]
            (let [first-replaced-range
                  (range first-replaced (+ first-replaced 5))]
              (or
               (= hand-values first-value-range)
               (= replaced-values first-replaced-range)))))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[straight-flush? 8] [four-of-a-kind? 7]
                   [full-house? 6] [flush? 5]
                   [straight? 4] [three-of-a-kind? 3]
                   [two-pairs? 2] [pair? 1]
                   [high-card? 0]}]
    (apply max (map second (filter (fn [x] ((first x) hand)) checkers)))))
