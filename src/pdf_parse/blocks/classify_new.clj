(ns pdf-parse.blocks.classify-new
  (:require [pdf-parse.common :as cmn]
            [clojure.string :as s]
            [pdf-parse.blocks.classify :as cl]))


;(defn labels-down? [{{:keys [num-blocks-directly-below gap-below
;                             blocks-directly-below]} :features :as block}]
;  (and
;    (labelish? block)
;    (pos? num-blocks-directly-below)
;    (< gap-below 50)
;    (let [{{:keys [num-datapoints num-english-words]} :features} (first blocks-directly-below)]
;      (and (pos? num-datapoints) (>= num-datapoints num-english-words)))))
;


(defn labelish? [{{:keys [num-tokens num-english-words]} :features}]
  (and
    (pos? num-tokens)
    (< num-tokens 20)
    (pos? num-english-words)))

(defn labels-right? [{{:keys [num-blocks-right gap-right
                              blocks-right text]} :features
                      tokens :tokens :as block}]
  (and
    (labelish? block)
    (pos? num-blocks-right)
    (< gap-right 200)
    (or (re-matches #".*\:\s*" text)
        (let [{btokens :tokens} (first blocks-right)]
          (and
            (or
              (every? :bold tokens)
              (every? :italic tokens))
            (not (or
                   (every? :bold btokens)
                   (every? :italic btokens))))))))


(defn dependent-block? [{{:keys [data-ratio num-english-words keyword-start?]} :features
                         :as block}]
  (and
    (>= data-ratio 0.48)
    (< num-english-words 4)
    (not (cl/page-footer? block))
    (not keyword-start?)
    ))


(defn classify [block]
  (cond
    (dependent-block? block) :table
    (labels-right? block) :key))
