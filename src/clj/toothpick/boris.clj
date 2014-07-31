(ns toothpick.boris
  (require [clj-http.client :as http]
           [clojure.java.io :as io]
           [clj-xpath.core :refer ($x $x:text xml->doc)]
           [toothpick.postcode :refer (lat-long->postcode)]))

(defn rename-key [k]
  (case k
    :long :lng
    k))

(defn station->map [s]
  (into {} (for [k [:name :lat :long :nbBikes :nbEmptyDocks :nbDocks]]
             [(rename-key k) ($x:text (name k) s)])))

(defn boris-file []
  (io/input-stream "file:///home/neale/workspace/toothpick/datasets/boris-bikes.xml"))

(defn boris-web-retrieve []
  (:body (http/get "http://www.tfl.gov.uk/tfl/syndication/feeds/cycle-hire/livecyclehireupdates.xml")))

(defn live-cycle-hire [s]
  (with-open [in s]
    (map
     (fn [n] (let [m (station->map n)]
              ;; (assoc-in m [:postcode] (lat-long->postcode m))
              m
              ))
     ($x "//stations/*" (xml->doc in)))))
