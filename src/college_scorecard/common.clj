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
