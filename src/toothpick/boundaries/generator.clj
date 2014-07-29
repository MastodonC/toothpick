(ns toothpick.boundaries.generator
  (:require [clojure.java.io :as io]
            [toothpick.boundaries :as tpb]))

(defn boundaries->geojson-file [borough-codes output]
  (with-open [codes (io/reader borough-codes)]
    (spit output (tpb/geojson-for (line-seq codes)))))

#_ (boundaries->geojson-file "output/borough-codes/part-00000" "output/boundaries.geo.json")
