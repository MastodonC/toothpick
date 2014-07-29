(ns toothpick.boundaries
  (:require [clj-http.client :as http]
            [clojure.string :as str]
            [cheshire.core :as json]
            ))

(defn get-statistics* [uri]
  (http/get uri {:as :json}))

(def get-statistics (memoize get-statistics*))

(defn ->coordinate-pairs [s]
  (->> (str/split s #"\s+")
       (map #(Double. %))
       (partition 2)
       (map vec)))

(defn data-for [e-code]
  (let [data (-> (format "http://statistics.data.gov.uk/doc/statistical-geography/%s.json" e-code)
                  get-statistics
                  (get-in [:body :result :primaryTopic]))]
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
