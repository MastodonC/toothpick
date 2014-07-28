(ns toothpick.analysis
  (:require [cascalog.api :refer :all]
            [cascalog.more-taps :refer (hfs-delimited)]
            [clojure.string :as s]
            ))


(defmapfn split [line]
  "reads in a line of string and splits it by regex"
  (s/split line #"[\[\]\\\(\),.)\s]+"))

(defn scrub-text [s]
  "trim open whitespaces and lower case"
  ((comp s/trim s/lower-case) s))


;; Date,time,"Modelled Wind Direction",status/unit,"Modelled Wind Speed",status/unit,"Modelled Temperature",status/unit,"Nitric oxide",status/unit,"Nitrogen dioxide",status/unit,"Nitrogen oxides as nitrogen dioxide",status/unit,"Non-volatile PM2.5 (Hourly measured)",status/unit,"PM2.5 particulate matter (Hourly measured)",status/unit,"Sulphur dioxide",status/unit,"Volatile PM2.5 (Hourly measured)",status/unit

(defn has-n-columns [n line]
  (= n (count line )))

(defn borough-gen [raw-in]
  (<- [?date ?time ?no2]
      (raw-in :>
              ?date ?time
              ?wind-direction ?wind-direction-unit
              ?wind-speed ?wind-speed-unit
              ?temperature ?temperature-unit
              ?nitric-oxide ?nitric-oxide_unit
              ?no2 ?no2-unit
              ?nitrogen-oxides-as-no2 ?nitrogen-oxides-as-no2-unit
              ?nv-pm25-hourly ?nv-pm25-hourly-unit
              ?pm25-particulate-matter-hourly ?pm25-particulate-matter-hourly-unit
              ?so2 ?so2-unit
              ?v-pm25-hourly ?v-pm25-hourly-unit)

      (:trap (hfs-textline "output/trap" :sinkmode :update))))

(defn go []
  (let [borough (hfs-delimited "/home/neale/workspace/toothpick/datasets/uk-air/lb-bexley-2.csv")
        trap (hfs-delimited "output/trap")]
    (borough-gen borough)))
