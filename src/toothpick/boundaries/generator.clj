(ns toothpick.boundaries.generator
  (:require [clojure.java.io :as io]
            [toothpick.boundaries :as tpb]
            [clojure-csv.core :as csv]))

(defn boundaries->geojson-file [borough-codes output]
  (with-open [codes (io/reader borough-codes)]
    (spit output (->> codes
                      line-seq
                      tpb/geojson-for))))

#_(boundaries->geojson-file "output/borough-codes/part-00000" "output/boundaries.geo.json")
