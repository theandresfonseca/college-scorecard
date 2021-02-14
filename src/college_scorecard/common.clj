(ns college-scorecard.common)

(defn parse-int
  [s]
  (try
    (Integer/parseInt s)
    (catch Exception _ nil)))

(defn parse-double
  [s]
  (try
    (Double/parseDouble s)
    (catch Exception _ nil)))

(defn not-unknown [s] (when (and (not-empty s) (not= s "NULL")) s))

(defn mean [xs] (/ (double (reduce + xs)) (count xs)))

(defn round
  [n places]
  (/ (Math/round (* n (Math/pow 10 places)))
     (Math/pow 10 places)))

(defn prefix-k [prefix k] (keyword (str (name prefix) "-" (name k))))
