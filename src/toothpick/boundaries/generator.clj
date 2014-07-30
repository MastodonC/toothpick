(ns toothpick.boundaries.generator
  (:require [clojure.java.io :as io]
            [toothpick.boundaries :as tpb]
            [clojure-csv.core :as csv]))

(def borough-code-columns [:e_code :la_name :cycling_weekly :cycling_rank :walking_thriceweekly :walking_rank :weekly_greenspace_visits :greenspace_rank :hospital_experience_score :hospital_rank :pct_canseegp :gp_rank :dentists_per_thousand :dentists_rank :overall_rank])

(defn boundaries->geojson-file [borough-codes scores output]
  (with-open [codes (io/reader borough-codes)]
    (spit output (->> codes
                      line-seq
                      (map (partial get scores))
                      tpb/geojson-for))))

(defn load-scores [s]
  (into {} (for [[e-code & _ :as r] (rest (csv/parse-csv (slurp s)))]
             [e-code (zipmap borough-code-columns r)])))

#_ (boundaries->geojson-file "output/borough-codes/small" (load-scores (slurp "datasets/borough_scores.csv")) "output/boundaries.geo.json")
