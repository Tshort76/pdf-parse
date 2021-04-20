(ns sandbox.trainer
  (:require [pdf-parse.core :as core]
            [pdf-parse.blocks.ml-features :as mlf]
            [clojure.string :as s]
            [pdf-parse.common :as cmn]
            [pdf-parse.tokens.pdf-extractor :as pe]
            [pdf-parse.annotations :as a]))



(defn parse-oracle-doc [pdf-url]
  (let [cname->class (zipmap (vals a/oracle-block-colors) (keys a/oracle-block-colors))
        color->class (zipmap (map vec (vals a/COLORS)) (map cname->class (keys a/COLORS)))]
    (map #(assoc % :id (mlf/block-id %)
                   :page-number (:page-number %)
                   :class (color->class (:color %))) (pe/extract-annotations pdf-url))))


(defn manual-box-label->block-labels [pdf-url]
  (let [manual-boxes (parse-oracle-doc pdf-url)
        blocks (->> pdf-url core/build-pages (mapcat (comp :blocks core/parse-page)))]
    (mapcat (fn [{:keys [page-number class] :as x}] (->> blocks
                                                         (filter #(and (= page-number (:page-number %)) (cmn/within? x %)))
                                                         (map #(assoc {} :class class
                                                                         :text (get-in % [:features :text])
                                                                         :filename (last (s/split pdf-url #"/"))
                                                                         :id (mlf/block-id %))))) manual-boxes)))


(defn blocks-as-feature-vectors [pdf-url]
  (->> pdf-url
       core/build-pages
       (mapcat (comp (partial map #(merge (select-keys % [:x0 :x1 :y0 :y1 :page-number])
                                          (mlf/ml-vectorize %)))
                     :blocks core/parse-page))))

(defn block-truth-data [raw-pdf-url oracle-pdf-url]
  (->> (blocks-as-feature-vectors raw-pdf-url)
       (concat (manual-box-label->block-labels oracle-pdf-url))
       (filter :id)
       (group-by :id)
       (map (comp (partial apply merge) second))))



;random forests ....
;TODO assign to multiple categories
(defn blocks-as-classified-features [pdf-url]
  (->> pdf-url
       core/build-pages
       (mapcat (comp (partial map #(merge (select-keys % [:x0 :x1 :y0 :y1 :page-number :class])
                                          {:filename (last (s/split pdf-url #"/"))}
                                          (mlf/rand-forest-features %)))
                     :blocks core/parse-page))))


(comment
  
  (require '[clojure.java.io :as io])

  (let [pdf (io/resource "edgar.pdf")
        chunk-dimension 5]
    (->> pdf
         core/build-pages
         (mapcat (comp (partial is/segments->chunks chunk-dimension) :segments core/parse-page))
         (a/annotate {:pdf-url pdf :output-directory u/annotated-dir})
         dorun
         ))


  ;block-oracle-2
  (spit "blocks.edn" (pr-str (concat (map blocks-as-classified-features (u/get-pdfs-in-dir "resources/control_2/blocks")))))


  ;build oracle data set
  (let [base-dir "resources/control_2/blocks"
        oracle-pdfs (u/get-pdfs-in-dir (str base-dir "oracle_9_17_2018"))
        raw-pdfs (u/get-pdfs-in-dir (str base-dir "raw"))]
    (map block-truth-data (sort raw-pdfs) (sort oracle-pdfs)))




  )