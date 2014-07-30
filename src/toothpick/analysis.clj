(ns toothpick.analysis
  (:require [cascalog.api :refer :all]
            [cascalog.more-taps :refer (hfs-delimited)]
            [clojure.string :as s]
            [clj-time.format :as tf]
            [clojure-csv.core :as csv]
            [clojure.tools.logging :as log]))

(def date-time-formatter (tf/formatter "dd-MM-yyyy hh:mm"))

;; Started with http://stackoverflow.com/questions/164979/uk-postcode-regex-comprehensive
;;  + I added a group round the last pattern.
;;  + I made most groups non-capturing.
;;  + I dropped the GIR match
(def uk-postcode-regex #"(?:((?:[A-Z-[QVX]][0-9][0-9]?)|(?:(?:[A-Z-[QVX]][A-Z-[IJZ]][0-9][0-9]?)|(?:(?:[A-Z-[QVX]][0-9][A-HJKSTUW])|(?:[A-Z-[QVX]][A-Z-[IJZ]][0-9][ABEHMNPRVWXY]))))\s+([0-9][A-Z-[CIKMOV]]{2}))")

(defmapfn split [line n]
  "reads in a line of string and splits it. truncates/pads to n cells. Replaces \\N in a cell with empty string."
  (let [cells (->> (first (csv/parse-csv line))
                   (take n)
                   (map #(s/replace % #"\\N" "")))
        cn    (count cells)]
    (concat cells (take (- n cn) (repeat nil)))))

(defmapfn normalise-postcode [s]
  (when-let [[prefix suffix] (next (re-matches uk-postcode-regex s))]
    (.toUpperCase (format "%-4s%s" prefix suffix))))

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
              ?nitric-oxide ?nitric-oxide-unit
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
      (split ?line 20 :#> 20 {0 ?practice 9 ?post-code-dirty})
      (normalise-postcode ?post-code-dirty :> ?post-code)))

(defn practice->borough [epraccur postcodes]
  (<- [?practice ?borough-code]
      (epraccur :> ?practice ?post-code)
      (postcodes :> ?post-code ?borough-code)))

(defn hospital->borough [hospitals postcodes]
  (<- [?hospital-code ?hospital-name ?borough-code]
      (hospitals :#> 6 {0 ?hospital-code
                        1 ?hospital-name
                        5 ?post-code})
      (postcodes :> ?post-code ?borough-code)))

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

(defn codepoint-file [raw-in trap]
  (<- [?Postcode ?Positional_quality_indicator ?Eastings ?Northings
       ?Country_code ?NHS_regional_HA_code ?NHS_HA_code ?Admin_county_code
       ?Admin_district_code ?Admin_ward_code]
      (raw-in ?line)
      (split ?line 10 :>
             ?Postcode ?Positional_quality_indicator ?Eastings ?Northings
             ?Country_code ?NHS_regional_HA_code ?NHS_HA_code ?Admin_county_code
             ?Admin_district_code ?Admin_ward_code)
      (:trap trap)))

(defn hospital-file [raw-in trap]
  (<- [?code ?site-name ?ccg-name ?pct-likely ?pct-unlikely ?post-code]
      (raw-in ?line)
      (split ?line 6 :> ?code ?site-name ?ccg-name ?pct-likely ?pct-unlikely ?post-code-dirty)
      (normalise-postcode ?post-code-dirty :> ?post-code)))

(defn toothpick-file [raw-in trap]
  (<- [?practice-id ?nhsfees ?post-code ?num-practitioners ?average-review-score-of-clinic]
      (raw-in ?line)
      (split ?line 5 :> ?practice-id ?nhsfees ?post-code-dirty ?num-practitioners ?average-review-score-of-clinic)
      (normalise-postcode ?post-code-dirty :> ?post-code)))

(defn postcode->borough [in]
  (<- [?post-code ?borough-code]
      (in :#> 10 {0 ?post-code
                  8 ?borough-code})))

(defn toothpick->borough [toothpick postcodes]
  (<- [?practice-id  ?borough-code]
      (toothpick :#> 5 {0 ?practice-id
                        2 ?post-code})
      (postcodes :> ?post-code ?borough-code)))

(defn distinct-boroughs [postcodes]
  (<- [?borough-code]
      (postcodes :#>  10 {8 ?borough-code})
      (:distinct ?borough-code)))

(defn go-pc-borough []
  (let [postcodes (hfs-textline "datasets/postcode-to-local-authority.csv")
        output    (hfs-delimited "output/pc-borough" :sinkmode :replace)
        trap      (hfs-delimited "output/trap" :sinkmode :replace)]
    (?- (stdout) (postcode->borough (postcode-file postcodes trap)))))

(defn go-practices []
  (let [epraccur  (hfs-textline "datasets/epraccur.csv")
        postcodes (hfs-textline "datasets/codepoint-postcodes.csv" :skip-header? true)
        output    (hfs-delimited "output/practices" :sinkmode :replace)
        trap      (hfs-delimited "output/trap" :sinkmode :replace)]
    (?- output (practice->borough (epraccur-file epraccur trap)
                                  (postcode->borough (codepoint-file postcodes trap))))))

(defn go-hospitals []
  (let [postcodes (hfs-textline "datasets/codepoint-postcodes.csv")
        hospitals (hfs-textline "datasets/hospital-locations.csv")
        output    (hfs-delimited "output/hospitals" :sinkmode :replace)
        trap      (hfs-delimited "output/trap" :sinkmode :replace)]
    (?- output (hospital->borough (hospital-file hospitals trap)
                                  (postcode->borough (codepoint-file postcodes trap))))))

(defn go-toothpick []
  (let [postcodes (hfs-textline "datasets/codepoint-postcodes.csv")
        toothpick (hfs-textline "datasets/toothpick.csv")
        output    (hfs-delimited "output/toothpick" :sinkmode :replace)
        trap      (hfs-delimited "output/trap" :sinkmode :replace)]
    (?- output (toothpick->borough (toothpick-file toothpick trap)
                                    (postcode->borough (codepoint-file postcodes trap))))))
(defn go-epraccur []
  (let [epraccur  (hfs-textline "datasets/epraccur-small.csv")
        output    (hfs-delimited "output/practices" :sinkmode :replace)
        trap      (hfs-delimited "output/trap" :sinkmode :replace)]
    (?- (stdout) (epraccur-file epraccur trap))))

(defn go-postcodes []
  (let [postcodes (hfs-textline "datasets/codepoint-postcodes-small.csv")
        output    (hfs-delimited "output/practices" :sinkmode :replace :write-header? true)
        trap      (hfs-delimited "output/trap" :sinkmode :replace)]
    (?- (stdout) (postcode->borough (codepoint-file postcodes trap)))))

(defn go-borough-codes []
  (let [postcodes (hfs-textline "datasets/codepoint-postcodes.csv" :skip-header? true)
        output    (hfs-delimited "output/borough-codes" :sinkmode :replace)
        trap      (hfs-delimited "output/trap" :sinkmode :replace)]
    (?- output (distinct-boroughs (codepoint-file postcodes trap)))))
