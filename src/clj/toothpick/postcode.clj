(ns toothpick.postcode
  (:require [clj-http.client :as http]))

(defn- lat-long->postcode-info* [{:keys [lat lng]}]
  (assert lat "latitude must be supplied")
  (assert lng "longitude must be supplied")
  (-> (format "http://uk-postcodes.com/latlng/%s,%s.json" lat lng)
      (http/get {:as :json})
      :body))

(def lat-long->postcode-info (memoize #'lat-long->postcode-info*))

(defn lat-long->postcode [m]
  (-> (lat-long->postcode-info m)
      (get-in [:postcode])))

(defn lat-long->council [m]
  (-> (lat-long->postcode-info m)
      (get-in [:administrative])))
