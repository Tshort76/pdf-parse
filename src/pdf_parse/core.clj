(ns pdf-parse.core
  "A collection of high level functions for transforming and
  annotating PDF documents"
  (:require
   [pdf-parse.tokens.pdf-extractor :as pe]
   [pdf-parse.tokens.core :as pd]
   [pdf-parse.components.core :as cmps]
   [pdf-parse.utilities :as utils]
   [pdf-parse.annotations :as ann]
   [pdf-parse.blocks.new-features :as f]
   [pdf-parse.blocks.segments :as bs]
   [pdf-parse.blocks.core :as bc]
   [plumbing.core :refer [fnk]]
   [plumbing.graph :as graph]
   [pdf-parse.blocks.classify :as cl]
   [pdf-parse.blocks.classify-new :as cl-new]
   [pdf-parse.components.columnar :as col]
   [pdf-parse.tokens.graphics :as g])
  (:import (org.apache.pdfbox.text PDFTextStripper)))

(def pdf->text
  (partial pe/process-pdf-document
           (fn [pdf-doc & [[start-page end-page]]]
             (let [stripper (cond-> (PDFTextStripper.)
                                    start-page (doto (.setStartPage start-page))
                                    end-page (doto (.setEndPage end-page)))]
               (.getText stripper pdf-doc)))))

(defn pages-of-words [pdf-path & [page-bounds]]
  (->> page-bounds
       (pe/extract-text-positions pdf-path)
       pd/text-positions->pages-of-tokens))


(defn- pdf->pages-of-lines [pdf-path & [page-bounds]]
  (map utils/create-lines (pages-of-words pdf-path page-bounds)))


(def page-parser
  {:tokens          (fnk [text-positions] (pd/page->token-stream text-positions))
   :ann-tokens      (fnk [tokens] (map utils/token->ann-format tokens))
   :visual-features (fnk [tokens] (->> tokens utils/create-lines
                                       (col/intertext-boundaries 4)
                                       (map #(assoc % :class :visual-feature :boundary-axis :x))))
   :segments        (fnk [tokens {graphics []} visual-features]
                         (bs/compose-segments tokens (concat visual-features graphics)))
   :blocks          (fnk [segments {graphics []} x0 x1 y0 y1]
                      (->> (bc/compose-blocks segments graphics)
                           (f/enfeature {:x0 x0 :x1 x1 :y0 y0 :y1 y1})
                           (map cl/add-class)))
   :exp-blocks      (fnk [segments {graphics []} x0 x1 y0 y1]
                      (->> (bc/compose-blocks segments graphics)
                           (f/enfeature {:x0 x0 :x1 x1 :y0 y0 :y1 y1})
                           (map #(assoc % :class (cl-new/classify %)))))
   ;:bit-masked-segments (fnk [segments] (is/segments->chunks 5 segments))
   :components      (fnk [tokens blocks]
                      (cmps/->components tokens blocks))})

(def parse-page (graph/lazy-compile page-parser))

(defn meta-by-page [[page-min page-max] raw-page-characteristics]
  ;; page bounds are zero indexed, page numbers 1 indexed
  (->> raw-page-characteristics
       (filter (fn [{:keys [page-number]}]
                 (and (>= (dec page-number) (or page-min 0)) (< (dec page-number) (or page-max 9999)))))))

(defn graphics-by-page [raw-graphics]
  (->> raw-graphics
       g/explode-graphics
       (group-by :page-number)
       (map (fn [[pg data]]
              {:page-number pg :graphics data}))))

(defn text-positions-by-page [raw-text]
  (->> raw-text
       (map (fn [{:keys [page-number x y] :as tp}]
              (assoc tp :id (str page-number "_" (int x) "_" (int y)))))
       (group-by :page-number)
       (keep (fn [[pg data]]
               (when (seq data)
                 {:page-number pg :text-positions data})))))


(defn build-pages
  ([pdf-url] (build-pages nil pdf-url))
  ([page-bounds pdf-url]
   (let [{:keys [graphics page-meta text]} (pe/extract-core-properties pdf-url page-bounds)]
     (->> (concat (when graphics (graphics-by-page graphics))
                  (meta-by-page page-bounds page-meta)
                  (text-positions-by-page text))
          (group-by :page-number)
          (map (comp (partial apply merge) second))
          (sort-by :page-number)))))

(defn- block-transforms [pdf-path {:keys [page-bounds] :as opts}]
  (->> (pages-of-words pdf-path page-bounds)
       (mapcat #(parse-page % opts))))

(defn transform
  "Transforms pages in the range [(first page-bounds) , (last page-bounds))
   of the input pdf (input as a url string or File object) into the desired output format.
   Supported formats include tokens, segments, blocks, plain-text, pages-of-lines, and components (default)."
  [pdf-path & [{:keys [page-bounds format] :as opts}]]
  (case (keyword format)
    :plain-text (pdf->text pdf-path page-bounds)
    :pages-of-lines (pdf->pages-of-lines pdf-path page-bounds)
    (->> pdf-path
         (build-pages page-bounds)
         (mapcat (comp format parse-page)))))


(defn annotate-components
  "Creates a modified copy of a pdf (specified as a url string) with
   colored boxes superimposed on it to delimit components.  A box's
   color corresponds to the type of the associated component.  The entire
   pdf will be copied, although only pages in the range
   [(first page-bounds) , (last page-bounds)) will be annotated."
  [^String pdf-url & [{:keys [output-directory page-bounds]}]]
  (->> (transform pdf-url {:page-bounds page-bounds :format :components})
       (ann/annotate {:pdf-url        pdf-url :output-directory output-directory
                      :table-columns? true :superscripts? true})
       dorun))


(defn annotate-blocks
  "Creates a modified copy of a pdf (specified as a url string) with
   colored boxes superimposed on it to delimit blocks.  A box's
   color corresponds to predicted class of the associated block.
   The entire pdf will be copied, although only pages in the range
   [(first page-bounds) , (last page-bounds)) will be annotated."
  [^String pdf-url & [{:keys [output-directory page-bounds]}]]
  (->> (transform pdf-url {:page-bounds page-bounds :format :blocks})
       (ann/annotate {:pdf-url pdf-url :output-directory output-directory})
       dorun))


(defn annotate-graphics [^String pdf-url & [{:keys [output-directory]}]]
  (->> pdf-url
       pe/extract-graphics
       (ann/annotate {:pdf-url pdf-url :output-directory output-directory})
       dorun))
