(ns toothpick.analysis
  (:require [cascalog.api :refer :all]
            [cascalog.more-taps :refer (hfs-delimited)]
            [clojure.string :as s]
            [clj-time.format :as tf]
            [clojure-csv.core :as csv]
            [clojure.tools.logging :as log]))

(def date-time-formatter (tf/formatter "dd-MM-yyyy hh:mm"))

(defmapfn split [line n]
  "reads in a line of string and splits it. truncates/pads to n cells."
  (let [cells (take n (first (csv/parse-csv line )))
        cn     (count cells)]
    (concat cells (take (- n cn) (repeat nil)))))

;; 01-01-2014,00:15
(defn parse-date-time [date time]
  (tf/parse date-time-formatter (str date " " time)))

;; Date,time,"Modelled Wind Direction",status/unit,"Modelled Wind Speed",status/unit,"Modelled Temperature",status/unit,"Nitric oxide",status/unit,"Nitrogen dioxide",status/unit,"Nitrogen oxides as nitrogen dioxide",status/unit,"Non-volatile PM2.5 (Hourly measured)",status/unit,"PM2.5 particulate matter (Hourly measured)",status/unit,"Sulphur dioxide",status/unit,"Volatile PM2.5 (Hourly measured)",status/unit

(defn has-n-columns [n line]
  (= n (count line )))

(defn borough-gen [raw-in]
  (<- [?date-time ?no2]
      (raw-in :> ?line)
      (split ?line 22 :>
              ?date-dirty ?time-dirty
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
      (parse-date-time ?date-dirty ?time-dirty :> ?date-time)
      (:trap (hfs-textline "output/trap" :sinkmode :update))))


(defn epraccur-file [raw-in trap]
  (<- [?practice ?post-code]
      (raw-in ?line)
      (split ?line 20 :#> 20 {0 ?practice 9 ?post-code})))

(defn practice->borough [epraccur postcodes]
  (<- [?practice ?borough-code]
      (epraccur :> ?practice ?post-code)
      (postcodes :> ?post-code ?borough-code _)))

(defn postcode-file [raw-in trap]
  (<- [?pcd7 ?pcd8 ?pcds
       ?par11cd ?par11nm ?par11nmw
       ?wd11cd ?wd11nm ?wd11nmw
       ?lad11cd ?lad11nm ?lad11nmw]
      (raw-in ?line)
      (split ?line 12 :>
              ?pcd7 ?pcd8 ?pcds
              ?par11cd ?par11nm ?par11nmw
              ?wd11cd ?wd11nm ?wd11nmw
              ?lad11cd ?lad11nm ?lad11nmw)
      (:trap trap)))

(defn postcode->borough [in]
  (<- [?postcode ?borough-code ?borough]
      (in :#> 12 {1 ?postcode-dirty
                  9 ?borough-code
                  10 ?borough-dirty})
      (identity ?postcode-dirty :> ?postcode)
      (identity ?borough-dirty :> ?borough)))

(defn go-pc-borough []
  (let [postcodes (hfs-textline "/home/neale/workspace/toothpick/datasets/postcode-to-local-authority.csv")
        output    (hfs-delimited "output/pc-borough" :sinkmode :replace)
        trap      (hfs-delimited "output/trap" :sinkmode :replace)]
    (?- (stdout) (postcode->borough (postcode-file postcodes trap)))))

(defn go-practices []
  (let [epraccur  (hfs-textline "/home/neale/workspace/toothpick/datasets/epraccur.csv")
        postcodes (hfs-textline "/home/neale/workspace/toothpick/datasets/postcode-to-local-authority.csv")
        output    (hfs-delimited "output/practices" :sinkmode :replace)
        trap      (hfs-delimited "output/trap" :sinkmode :replace)]
    (?- output (practice->borough (epraccur-file epraccur trap)
                                    (postcode->borough (postcode-file postcodes trap))))))
(defn go-epraccur []
  (let [epraccur  (hfs-textline "/home/neale/workspace/toothpick/datasets/epraccur-small.csv")
        output    (hfs-delimited "output/practices" :sinkmode :replace)
        trap      (hfs-delimited "output/trap" :sinkmode :replace)]
    (?- (stdout) (epraccur-file epraccur trap))))

(defn go []
  (let [borough (hfs-delimited "/home/neale/workspace/toothpick/datasets/uk-air/lb-bexley-2.csv")
        output (hfs-delimited "output/borough" :sinkmode :replace)
        trap (hfs-delimited "output/trap" :sinkmode :replace)]
    (?- output (borough-gen borough))))
