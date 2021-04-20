(ns pdf-parse.test-utils
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(def test-dir "resources/tests/")

(defn ->url [rel-path]
  (->> rel-path 
       (str test-dir)
       io/file
       .getCanonicalPath
       (str "file:///")))

(defn load-expected [edn-file]
  (-> edn-file
      ->url
      slurp
      edn/read-string))
