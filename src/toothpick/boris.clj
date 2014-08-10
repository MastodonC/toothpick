(ns toothpick.boris
  (require [clj-http.client :as http]
           [clojure.java.io :as io]
           [clj-xpath.core :refer ($x $x:text xml->doc)]
           [toothpick.postcode :refer (lat-long->postcode)]
           [clojure-csv.core :as csv]))

(defn rename-key [k]
  (case k
    :long :lng
    k))

(defn station->map [s]
  (into {} (for [k [:name :lat :long :nbBikes :nbEmptyDocks :nbDocks]]
             [(rename-key k) ($x:text (name k) s)])))

(defn boris-file []
  (slurp "file:///home/neale/workspace/toothpick/datasets/boris-bikes.xml"))

(defn boris-web-retrieve []
  (:body (http/get "http://www.tfl.gov.uk/tfl/syndication/feeds/cycle-hire/livecyclehireupdates.xml")))

(defn ->map [n] (let [m (station->map n)]
            (assoc-in m [:postcode] (lat-long->postcode m))))

(def extract-columns (juxt :name :postcode :nbBikes :nbDocks))

(defn live-cycle-hire [s]
  (with-open [out (io/writer "/home/neale/workspace/toothpick/datasets/boris-stations.csv")]
    (.write out (csv/write-csv [["name" "postcode" "nbBikes" "nbDocks"]]))
    (doseq [station ($x "//stations/*" (xml->doc s))]
      (let [data (-> station
                     ->map
                     extract-columns
                     vector
                     csv/write-csv)]
        (println "data:" data)
        (.write out data)))))
