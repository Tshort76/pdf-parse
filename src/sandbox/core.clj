(ns sandbox.core
  (:require [pdf-parse.tokens.pdf-extractor :as pe]
            [pdf-parse.annotations :as a]
            [sandbox.utils :as u]
            [pdf-parse.utilities :as utils]
            [pdf-parse.tokens.graphics :as g]
            [pdf-parse.common :as ptc]
            [pdf-parse.core :as core]
            [taoensso.timbre :as t]
            [clojure.java.io :as io]))

(t/merge-config! {:level        :trace  ; element of : #{:trace :debug :info :warn :error :fatal :report}

                  ;:ns-whitelist  ["pdf-parse.components.tables"] #_["my-app.foo-ns"]
                  :ns-blacklist ["org.apache.pdfbox.*"] #_["taoensso.*"]
                  })


(defn annotate-it [pdf-url & [{:keys [out level] :as opts}]]
  (let [pdf-url (.toString pdf-url)]
    (a/annotate {:pdf-url pdf-url :output-directory (or out u/annotated-dir)}
                (->> pdf-url
                     core/build-pages
                     (mapcat (comp level core/parse-page)))))) ;comp level to get a flat seq of all the blocks or segments


(defn annotate-features [pdf-url & [out-dir]]
  (let [graphics (->> pdf-url pe/extract-graphics g/explode-graphics)
        features (->> pdf-url
                      (core/build-pages [0 100])
                      (mapcat (comp :visual-features core/parse-page)))]
    (a/annotate {:pdf-url pdf-url :output-directory (or out-dir u/annotated-dir)}
                (concat graphics features))))

;assumes that batch-folder is in the pdf_parsing directory
(defn annotate-batch [batch-folder & [level oracle?]]
  (let [base-dir (.toString (io/resource batch-folder))]
    (->> (str base-dir "raw")
         u/get-pdfs-in-dir
         (map #(do (println "processing: " %)
                   (if (= level :boundaries)
                     (annotate-features % (str base-dir "boundaries"))
                     (annotate-it % {:out (str base-dir (if oracle? "oracle" (name level))) :level level}))))
         dorun)))


(defn classify-text [txt]
  (some (fn [[label rgx]]
          (when (re-matches rgx txt) label)) ptc/token-types)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Repl snippets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment


  (annotate-batch "control_1" :tokens)
  (annotate-batch "control_1" :segments)


  (annotate-batch "control_2" :ann-tokens)
  (annotate-batch "control_2" :segments)
  (annotate-batch "control_2" :blocks)
  (annotate-batch "control_2" :components)
  (annotate-batch "control_2" :bit-masked-segments)
  (annotate-batch "control_2" :exp-blocks)
  (annotate-batch "control_2" :boundaries)


  (annotate-batch "blackrock" :segments)
  (annotate-batch "blackrock" :blocks)
  (annotate-batch "aig_1" :features)

  ;annotate tokens example
  (let [pdf (io/resource "control_2/raw/a150c74d7a9d72d4c69ad8a3f5084fcf.pdf")]
    (->> (core/transform pdf {:format :tokens})
         (map (comp #(assoc % :class (classify-text (:text %))) utils/token->ann-format))
         (a/annotate {:pdf-url pdf :output-directory u/annotated-dir})
         dorun))

  ;parse segments
  (->> (io/resource "control_2/raw/floating_table_gnma1.pdf")
       core/build-pages
       (mapcat (comp :tokens core/parse-page))
       #_(map #(dissoc % :tokens)))

  ;annotate single doc
  (let [pdf (io/resource "control_2/raw/13034AFJ4.pdf")]
    (->> pdf
         core/build-pages
         (mapcat (comp :exp-blocks core/parse-page))
         (a/annotate {:pdf-url pdf :output-directory u/annotated-dir})
         dorun))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;    More specialized stuff    ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;annotate arbitrary chunks of the pdf
  (a/annotate {:pdf-url (io/resource "control_2/raw/54811A4J9.pdf") :output-directory u/annotated-dir}
              (let [step (int (/ 800 3))]
                (for [x (range 0 800 step)
                      y (range 0 800 step)]
                  {:page-number 1 :x0 x :x1 (+ x step) :y0 y :y1 (+ y step)})))


  ;get page characteristics
  (->> (io/resource "control_2/raw/54811A4J9.pdf")
       core/build-pages
       (map #(dissoc % :text-positions)))



  )