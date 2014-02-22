(ns dean.core)

(def a-note {:type :drum :values "R"})
(def b-note {:type :drum :values "L"})

(defn gcd 
  "Euclid's algorithm"
  [x y]
  (cond (zero? y) x
        :else (recur y (mod x y))))

(defn lcm
  "Least common multiple"
  [x y]
  (/ (* x y) (gcd x y)))

(def alternator 
  [{:events [a-note]} 
   {:events [b-note]}])
(def accentor [{} {} {:accent :hard} {}])

;; TODO: name this function correctly?
(defn add-single-accent 
  [y x] 
  (update-in x [:events] (partial map (partial merge y))))

(defn add-multi-accents [accentor melody]
  (let [
        new-length (lcm (count melody) (count accentor))
        melody-cycled (take new-length (cycle melody))
        accentor-cycled (take new-length (cycle accentor))
        ]
    (map add-single-accent accentor-cycled melody-cycled)))

(defn multiply 
  [melody multipliers]
  (for [x melody
        y multipliers]
    (merge x y)))
