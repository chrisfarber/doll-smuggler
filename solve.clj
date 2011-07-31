; Solution to the doll smuggling problem
; Copyright 2011 Chris Farber

; returns a random integer between lower and upper, inclusive
(defn rand_within [lower upper]
  (+ lower (rand-int (- upper lower -1))))

(defn get_dolls_from_dealer[]
  (let [number_of_dolls (rand_within 1 35)
        weight_range [1 20]
        value_range [1 100]
        ]
    (loop [dolls #{}]
      (if (< (count dolls) number_of_dolls)
        (let [new_doll {"w" (apply rand_within weight_range)
                        "v" (apply rand_within value_range)}]
          (recur (conj dolls new_doll)))
        ; return a vector of dolls instead of a set, so it can be accessed via
        ; an index
        (into [] dolls))
      )
    )
  )

(defn get_weight_capacity_of_handbag[]
  (rand_within 1 100))

(declare pick_dolls_cached)
(defn pick_dolls[dolls i max_capacity]
  ; I'm using the algorithm described on Wikipedia:
  ; http://en.wikipedia.org/wiki/Knapsack_problem#0-1_knapsack_problem
  (if (or (= 0 max_capacity) (< i 0))
    [[] 0]
    (let [doll (get dolls i)
          wi (get doll "w")
          vi (get doll "v")]
      (if (> wi max_capacity)
        (pick_dolls_cached dolls (- i 1) max_capacity)
        (let [[dolls_if_skipping value_if_skipping]
              (pick_dolls_cached dolls (- i 1) max_capacity)
              [dolls_if_keeping value_if_keeping]
              (pick_dolls_cached dolls (- i 1) (- max_capacity wi))]
          ; note that dolls_if_keeping and value_if_keeping do not yet include
          ; the current doll
          (if (> (+ value_if_keeping vi) value_if_skipping)
            ; keep the doll
            [(conj dolls_if_keeping doll) (+ value_if_keeping vi)]
            ; skip the doll
            [dolls_if_skipping value_if_skipping]
            )
          )
        )
      )
    )
  )

; I'm sure glad I found memoize in the docs, it saved me the trouble of
; doing my own caching!
(def pick_dolls_cached (memoize pick_dolls))

(defn run[]
  (let [handbag_capacity (get_weight_capacity_of_handbag)
        available_dolls (get_dolls_from_dealer)
        [selected_dolls total_value]
          (pick_dolls_cached available_dolls (- (count available_dolls) 1) handbag_capacity)]
    (do
      (println (str "Capacity of handbag: " handbag_capacity "\n"))
      (println (str "Dolls from dealer:   " available_dolls "\n"))
      (println (str "Dolls selected:      " selected_dolls "\n"))
      (println (str "Value of selected:   " total_value "\n"))
      ))
  )

(defn tests[]
  (let [test_dolls
        [
         {"w" 15 "v" 1}
         {"w" 30 "v" 10}
         {"w" 16 "v" 35}
         {"w" 5 "v" 900}
         {"w" 4 "v" 5}
         {"w" 5 "v" 4}
         {"w" 12 "v" 6}
         {"w" 13 "v" 24}
         {"w" 6 "v" 9}
         ]
        capacity 50
        [selected selected_value]
          (pick_dolls_cached test_dolls (- (count test_dolls) 1) capacity)
        best_selection
        [
         {"w" 5, "v" 900}
         {"w" 16, "v" 35}
         {"w" 13, "v" 24}
         {"w" 6, "v" 9}
         {"w" 4, "v" 5}
         {"w" 5, "v" 4}
         ]
        ]
    ; Check that we get back a vector
    (assert (vector? selected))
    ; Check that the vector contains only maps
    (assert (every? map? selected))
    ; Check that each map contains the keys "w" and "v"
    (assert (every? (fn [x] (and (contains? x "w") (contains? x "v"))) selected))
    ; Check that the values for keys "w" and "v" are integers >= 0
    (assert (every? (fn [x] (and (>= (get x "w") 0) (>= (get x "v") 0))) selected))
    ; Check that we actually get back the best selection
    (assert (= (into #{} best_selection) (into #{} selected)))
    )
  (let [dolls (get_dolls_from_dealer)]
    ; Check that we get back a vector
    (assert (vector? dolls))
    ; Check that the vector contains at least one doll... otherwise, why have a dealer?
    (assert (> (count dolls) 0))
    ; Check that the vector contains only maps
    (assert (every? map? dolls))
    ; Check that each map contains the keys "w" and "v"
    (assert (every? (fn [x] (and (contains? x "w") (contains? x "v"))) dolls))
    ; Check that the values for keys "w" and "v" are integers >= 0
    (assert (every? (fn [x] (and (>= (get x "w") 0) (>= (get x "v") 0))) dolls))
    )
  )

(tests)
(run)
