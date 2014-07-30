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
  (if s
    (let [s' (if (vector? s) (first s) s)]
      (->> (str/split s' #"\s+")
           (map #(Double. %))
           (partition 2)
           (map vec)))
    []))

(defn ->statistical-geog-url [e-code]
  (format "http://statistics.data.gov.uk/doc/statistical-geography/%s.json" e-code))

(defn merge-geojson [{:keys [e_code] :as m}]
  (println "retrieving data for " e_code)
  (if e_code
    (when-let [data (-> e_code
                        ->statistical-geog-url
                        get-borough-data
                        (get-in [:body :result :primaryTopic]))]
      (println "returning data for " e_code)
      {:properties (-> m
                       (dissoc :e-code)
                       (assoc :name     (:officialname data)
                              :e-code   (:label data)))
       :coordinates (->coordinate-pairs (:hasExteriorLatLongPolygon data))})
    (println "ERR: " m)
    ))

(defn ->geojson-feature [{:keys [properties coordinates]}]
  {:type "Feature"
   :geometry {:type "LineString"
              :coordinates coordinates}
   :properties properties})

(defn geojson-for [xs]
  (json/encode  {:type "FeatureCollection"
                 :features (map ->geojson-feature (map merge-geojson xs))}))
