(ns college-scorecard.core
  (:require [college-scorecard.load :as load]))

(defn program-summary
  [{:keys [academics]}]
  (let [{:keys [program program-percentage]} academics]
    (->> program
         (map (fn [[level programs]]
                (->> programs
                     (filter (comp pos? second))
                     (map (fn [[k v]] [k {level v}]))
                     (into {}))))
         (reduce (partial merge-with merge))
         (map (fn [[k v]]
                (assoc v :program (name k)
                       :percentage (k program-percentage)))))))
