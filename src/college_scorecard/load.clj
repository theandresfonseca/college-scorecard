(ns college-scorecard.load
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [college-scorecard.common :as common]
            [dk.ative.docjure.spreadsheet :as xl])
  (:import (java.util.zip ZipFile)))

;; I/O

(defn csv-data->maps
  [csv-data]
  (mapv zipmap
        (->> (first csv-data) (map keyword) repeat)
        (rest csv-data)))
 
(defn entry
  "given a zip (of a single file), returns underlying entry"
  [file-name]
  (->> file-name
       (new ZipFile)
       (.entries)
       (enumeration-seq)
       (some (fn [entry] (.getName entry)))))

(defn read-zip
  [file-name]
  (let [z (new ZipFile file-name)]
    (with-open [rdr (->> file-name entry (.getEntry z) (.getInputStream z) io/reader)]
      (csv-data->maps (csv/read-csv rdr)))))

;; Load Scorecard

(def dictionary-columns
  {:A :description
   :B :category
   :C :name
   :D :type
   :E :id
   :F :value
   :G :label
   :H :source
   :I :notes})

(defn dictionary []
  (->> (xl/load-workbook "data/CollegeScorecardDataDictionary.xlsx")
       (xl/select-sheet  "institution_data_dictionary")
       (xl/select-columns dictionary-columns)
       (rest)))

(defn name->path
  [s]
  (->> (string/split s #"\.")
       (map #(string/replace % #"_" "-"))
       (mapv keyword)))

(defn nesting-instructions
  [dictionary]
  (->> dictionary
       (filter :name)
       (map (fn [{:keys [id category name type]}]
              [(keyword id) {:path (cons (keyword category) (name->path name))
                             :parse-fn (case type
                                         "integer" common/parse-int
                                         "float" common/parse-double
                                         common/not-unknown)}]))
       (into {})))

(defn clean-school
  [nesting-instructions school]
  (reduce (fn [acc [id {:keys [path parse-fn]}]]
            (if-let [v (parse-fn (get school id))]
              (assoc-in acc path v)
              acc))
          {}
          nesting-instructions))

(defn schools
  "~255 MiB"
  ([]
   (schools (nesting-instructions (dictionary))))
  ([nesting-instructions]
   (->> (read-zip "data/most-recent-cohorts-all-data-elements-1.csv.zip")
        (mapv (partial clean-school nesting-instructions))
        (time))))
