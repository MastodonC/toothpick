(ns toothpick.toothpick
  (:require [clj-http.client :as http]
            [clojure.string :as str]
            )
  )

(def api-key "4c5724d7")

(def api-base "https://developers.toothpick.com/api/")


(defn request* [key & [params]]
  (http/get (str api-base (name key) "/" (str/join \/ params)) {:query-params {:apikey api-key}
                                       :debug true
                                       :as :json}))

(defn languages []
  (request* :languages))

(defn reasons []
  (request* :reasons))

(defn provider [{:keys [provider-id]}]
  (request* :provider [provider-id]))

(defn practice [{:keys [practice-id]}]
  (request* :practice [practice-id]))

;; /practice-calendar/{practiceId}/payment/{payment}/reason/{reason}
(defn practice-calendar [{:keys [practice-id payment reason]}

                         ]

  )
