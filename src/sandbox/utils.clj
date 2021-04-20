(ns sandbox.utils
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))


(defn get-pdfs-in-dir [dir]
  (->> dir
       io/as-file
       file-seq
       rest
       (map (comp str #(.toURL %)))
       (filter #(s/ends-with? % ".pdf"))))

(defn token->bounds [{:keys [x width y height page-number id]}]
  {:x0 x :x1 (+ x width) :y0 (- y height) :y1 y :page-number page-number :id id})

(def annotated-dir "resources/output")