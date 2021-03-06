(defproject toothpick "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [cascalog "2.1.1"]
                 [clj-http "0.9.2"]
                 [org.clojure/data.xml "0.0.7"]
                 [com.github.kyleburton/clj-xpath "1.4.3"]
                 [clj-time "0.8.0"]
                 [camel-snake-kebab "0.2.0"]
                 [clojure-csv "2.0.1"]]

  :source-paths ["src/clj"]

  :profiles { :dev {:dependencies [[org.apache.hadoop/hadoop-core "1.1.2"]]}}

  :jvm-opts ["-Xms768m" "-Xmx4G"])
