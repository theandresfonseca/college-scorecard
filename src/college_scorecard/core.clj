(ns college-scorecard.core
  (:require [clojure.set :as set]
            [college-scorecard.common :as common]
            [college-scorecard.load :as load]))

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

(defn scores
  [school score-key]
  (-> school
      (get-in [:admissions score-key])
      (select-keys [:25th-percentile :midpoint :75th-percentile])))

(defn reported-subjects
  [school score-key]
  (if-let [scores (not-empty (scores school score-key))]
    (->> scores (map (comp set keys second)) (reduce set/intersection))
    #{}))

(defn score-iqr
  [school score-key subject]
  (let [{q1 :25th-percentile q3 :75th-percentile} (scores school score-key)]
    (- (min (subject q3) (score-key {:sat-scores 800.0 :act-scores 36.0}))
       (max (subject q1) (score-key {:sat-scores 200.0 :act-scores 1.0})))))

(defn score-distribution
  [school score-key]
  (reduce (fn [{q1 :25th-percentile q3 :75th-percentile :as acc} subject]
            (let [iqr (score-iqr school score-key subject)]
              (-> acc
                  (assoc-in [:estimated-min subject] (- (subject q1) (* 1.5 iqr)))
                  (assoc-in [:estimated-max subject] (+ (subject q3) (* 1.5 iqr))))))
          (scores school score-key)
          (reported-subjects school score-key)))

(defn relative-subject-tier
  [school score-key subject score]
  (let [dist (score-distribution school score-key)]
    (cond
      (and (>= score (get-in dist [:estimated-min subject]))
           (< score (get-in dist [:25th-percentile subject]))) :reach
      (and (>= score (get-in dist [:25th-percentile subject]))
           (< score (get-in dist [:midpoint subject])))        :target-reach
      (and (>= score (get-in dist [:midpoint subject]))
           (< score (get-in dist [:75th-percentile subject]))) :target-safety
      (and (>= score (get-in dist [:75th-percentile subject]))
           (< score (get-in dist [:estimated-max subject])))   :saftey
      :else nil)))

(defn subjects-subset?
  [school score-key student-scores]
  (set/subset? (set (keys student-scores))
               (reported-subjects school score-key)))

(defn relative-school-tier
  [school score-key student-scores]
  (reduce (fn [acc [subject score]]
            (assoc acc subject (relative-subject-tier school score-key subject score)))
          {}
          student-scores))

(def tier-score
  {:saftey        1
   :target-safety 2
   :target-reach  3
   :reach         4})

(defn score->tier
  [score]
  (case score
    (1.0 1.5)     :safety
    (2.0 2.5 3.0) :target
    (3.5 4.0)     :reach
    nil))

(defn relative-school-tier-score
  [school score-key student-scores]
  (some->> (relative-school-tier school score-key student-scores)
           (map (fn [[_ tier]] (get tier-score tier)))
           (filter some?)
           (not-empty)
           (common/mean)))

(defn tuition-type
  [{:keys [cost school]} state]
  (if (get-in cost [:tuition :program-year])
    :program-year
    (if (= state (:state school))
      :in-state
      :out-of-state)))

(defn tuition-summary
  [{:keys [cost] :as school} state]
  (let [tuition-type (tuition-type school state)]
    {:tuition-type        (name tuition-type)
     :tuition             (get-in cost [:tuition tuition-type])
     :state               (get-in school [:school :state])}))


(def family-income-levels
  [:0-30000 :30001-48000 :48001-75000 :75001-110000 :110001-plus])

(defn net-price-summary
  [{:keys [cost]} income-level]
  (let [[price-type avg-net-price] (first (:avg-net-price cost))]
    {:price-type          (when price-type (name price-type))
     :avg-net-price       avg-net-price
     :net-price-at-income (get-in cost [:net-price price-type :by-income-level income-level])}))

(defn median-sat-score
  [school]
  (when-let [{:keys [critical-reading math]} (get-in school [:admissions :sat-scores :midpoint])]
    (+ critical-reading math)))

(defn median-score
  [school score-key]
  (score-key {:sat-scores (median-sat-score school)
              :act-scores (get-in school [:admissions :act-scores :midpoint :cumulative])}))

(defn summary
  [school score-key student-scores student-state income-level]
  (merge
   {:name (get-in school [:school :name])
    :size (get-in school [:student :size])
    :mean-earnings (get-in school [:earnings :8-yrs-after-entry :mean-earnings])
    :median-earnings (get-in school [:earnings :8-yrs-after-entry :median-earnings])
    :tier (relative-school-tier-score school score-key student-scores)
    (common/prefix-k :mean score-key) (get-in school [:admissions score-key :average :overall])
    (common/prefix-k :median score-key) (median-score school score-key)}
   (tuition-summary school student-state)
   (net-price-summary school income-level)))

(defn applicable-summaries
  [score-key student-scores student-state income-level schools]
  (->> schools
       (filter (fn [s] (subjects-subset? s score-key student-scores)))
       (map #(summary % score-key student-scores student-state income-level))
       (filter :tier)))

(defn safe-div
  ([x y]
   (safe-div x y 3))
  ([x y places]
  (when (and (number? x) (number? y))
    (common/round (/ x y) places))))

(defn roi
  [summary cost-key]
  (let [prefix (get {:tuition :median :net-price-at-income :mean} cost-key)]
    (safe-div (get summary (common/prefix-k prefix :earnings))
              (get summary cost-key))))

(defn point-per-dollar
  [summary cost-key score-key]
  (let [prefix (get {:tuition :median :net-price-at-income :mean} cost-key)]
    (safe-div (get summary (common/prefix-k prefix score-key))
              (get summary cost-key))))

(defn report-keys
  [cost-key score-key]
  (get {:tuition [:name :state :size :tier  :tuition-type :tuition
                  :median-earnings (common/prefix-k :median score-key)
                  :roi :point-per-dollar]
        :net-price-at-income [:name :state :size :tier :tuition-type :price-type :net-price-at-income
                              :mean-earnings (common/prefix-k :mean score-key)
                              :roi :point-per-dollar]}
       cost-key))

(defn augment-summaries
  [score-key cost-key summaries]
  (->> summaries
       (filter (fn [{:keys [tuition net-price-at-income tuition-type price-type]}]
                 (get {:tuition tuition
                       :net-price-at-income (and net-price-at-income
                                                 (or (= tuition-type "in-state")
                                                     (= price-type "private")))}
                      cost-key)))
       (map (fn [summary]
              (-> summary
                  (assoc :roi              (roi summary cost-key)
                         :point-per-dollar (point-per-dollar summary cost-key score-key))
                  (select-keys (report-keys cost-key score-key)))))))

(def states
  #{"AL" "AK" "AZ" "AR" "CA" "CO" "CT" "DE" "FL" "GA" "HI" "ID" "IL" "IN" "IA" "KS"
    "KY" "LA" "ME" "MD" "MA" "MI" "MN" "MS" "MO" "MT" "NE" "NV" "NH" "NJ" "NM" "NY"
    "NC" "ND" "OH" "OK" "OR" "PA" "RI" "SC" "SD" "TN" "TX" "UT" "VT" "VA" "WA" "WV"
    "WI" "WY" "DC" "PR"})

(def midwest #{"IL" "WI" "IN" "MI" "IA" "OH" "MN" "MO"})

(def average-student-sat {:critical-reading 531 :math 528})
(def high-student-sat {:critical-reading 680 :math 720})
