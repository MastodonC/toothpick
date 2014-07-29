(ns toothpick.boundaries
  (:require [clj-http.client :as http]
            [clojure.string :as str]
            [cheshire.core :as json]
            [clojure.tools.logging :as log]
            ))

(defn get-borough-data* [uri]
  (http/get uri {:as :json}))

(def get-borough-data get-borough-data*)

(defn ->coordinate-pairs [s]
  (println "AAA:" (type s))
  (if s
    (let [s' (if (vector? s) (first s) s)]
      (->> (str/split s' #"\s+")
           (map #(Double. %))
           (partition 2)
           (map vec)))
    []))

(defn ->statistical-geog-url [e-code]
  (format "http://statistics.data.gov.uk/doc/statistical-geography/%s.json" e-code))

(defn data-for [e-code]
  (println "retrieving data for " e-code)
  (when-let [data (-> e-code
                      ->statistical-geog-url
                      get-borough-data
                      (get-in [:body :result :primaryTopic]))]
    (println "returning data for " e-code)
    {:name     (:officialname data)
     :e-code   (:label data)
     :coordinates (->coordinate-pairs (:hasExteriorLatLongPolygon data))}))

(defn ->geojson-feature [{:keys [name e-code coordinates]}]
  {:type "Feature"
   :geometry {:type "LineString"
              :coordinates coordinates}
   :properties {:name name
                :e-code e-code}})

(defn geojson-for [es]
  (json/encode  {:type "FeatureCollection"
                 :features (map ->geojson-feature (map data-for es))}))
