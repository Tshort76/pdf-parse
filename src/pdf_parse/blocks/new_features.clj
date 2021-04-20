(ns pdf-parse.blocks.new-features
  "Functions to determine feature values and to associate blocks with their feature values"
  (:require [pdf-parse.utilities :as utils]
            [pdf-parse.common :as cmn]
            [clojure.string :as s]
            [plumbing.core :refer [fnk]]
            [plumbing.graph :as graph]))

(def footnote #"(?:[*+†]|\(\d{1,2}\))\s*[a-zA-Z]+.*")
(def word-like #"[^\d]*[aeiouyAEIOUY]+[^\d]*")
(def MAX-GAP 1000)


;can blocks have a more raw representation?
(def internal-features-graph
  {:lines                 (fnk [tokens] (utils/create-lines tokens))
   :text-list             (fnk [tokens] (map :text tokens))
   :text                  (fnk [text-list] (s/join " " text-list))

   :num-tokens            (fnk [tokens] (max 1 (count (remove (comp #{:symbol} :class) tokens))))
   ;:horizontal-alignment (fnk [page-x0 page-x1 x0 x1]
   ;                        (cond
   ;                          (cmn/centered? page-x0 page-x1 {:x0 x0 :x1 x1}) :center
   ;                          (utils/within-x? 25 x0 page-x0) :left
   ;                          :else :float))
   :dist-from-left        (fnk [page-x0 x0] (- x0 page-x0))
   :dist-from-right       (fnk [page-x1 x1] (- page-x1 x1))
   :width                 (fnk [x1 x0] (int (- x1 x0)))
   :height                (fnk [y0 y1] (int (- y1 y0)))
   :bold-ratio            (fnk [tokens] (/ (count (filter :bold? tokens)) (count tokens)))
   :italic-ratio          (fnk [tokens] (/ (count (filter :italic? tokens)) (count tokens)))
   :all-caps-ratio        (fnk [text-list]
                            (/ (count (filter (partial re-matches utils/all-caps) text-list)) (count text-list)))
   :ends-with-period?     (fnk [text-list] (boolean (re-matches utils/sentence-ending (last text-list))))
   :num-english-words     (fnk [tokens]
                            ;(->> tokens (remove :superscript?) (map :text) (filter (partial re-matches word-like)) count)
                            (->> tokens (remove :superscript?) (filter (comp #{:word} :class)) count))
   :num-datapoints        (fnk [tokens]
                            ;(->> tokens (remove :superscript?) (map :text) (filter (partial re-find #"[$%\d]")) count)
                            (->> tokens (remove :superscript?) (filter (comp #{:data :numeric :enum :date} :class)) count))
   :num-lines             (fnk [tokens] (:sum (reduce (fn [{:keys [sum prev-y]} {y :y}]
                                                        {:sum    (+ sum (if (> (- y prev-y) 4) 1 0))
                                                         :prev-y y})
                                                      {:sum 1 :prev-y 1000} (sort-by :y tokens))))
   :num-sentences         (fnk [tokens] (->> tokens
                                             (remove :superscript?)
                                             (map :text)
                                             (filter (partial re-matches utils/sentence-ending))
                                             count))
   :keyword-start?        (fnk [text-list] (boolean (some (partial re-matches utils/delimited) (take 10 text-list))))
   :itemized-start?       (fnk [text tokens]
                            (boolean (or (:superscript? (first tokens)) ;line starts with a superscript
                                         (re-matches footnote text))))

   ;composed features
   :data-ratio            (fnk [num-datapoints num-tokens] (/ num-datapoints num-tokens))
   :word-ratio            (fnk [num-english-words num-tokens] (/ num-english-words num-tokens))
   :tokens-per-line       (fnk [num-tokens num-lines] (/ num-tokens num-lines))
   :avg-word-cnt-per-line (fnk [num-english-words num-lines] (/ num-english-words num-lines))
   :avg-data-cnt-per-line (fnk [num-datapoints num-lines] (/ num-datapoints num-lines))

   })

(def internal-features (graph/compile internal-features-graph))


(defn x-aligned-bits? [{page-x0 :x0 page-x1 :x1}
                       {ax0 :x0 ax1 :x1}
                       {bx0 :x0 bx1 :x1}]
  (let [acenter (/ (+ ax0 ax1) 2)
        bcenter (/ (+ bx0 bx1) 2)
        page-width (- page-x1 page-x0)]
    (and                                                    ;we are not interested in full page sections
      (< (/ (- ax1 ax0) page-width) 0.5)
      (< (/ (- bx1 bx0) page-width) 0.5)
      (or
        (utils/within-x? 10 ax0 bx0)
        (utils/within-x? 10 ax1 bx1)
        (utils/within-x? 10 acenter bcenter)))))


(defn context-features [other-blocks {:keys [x0 x1 y0 y1] :as block}]
  (let [page-coords {:x0 (apply min (map :x0 other-blocks))
                     :x1 (apply max (map :x1 other-blocks))}
        relatives (map #(assoc % :relative (cmn/relative-to block %)) other-blocks)
        format (fn [items] (->> items (sort-by (partial utils/euc-distance block)) (take 2)))
        directly-above (->> relatives
                            (filter (comp #(= % #{:above}) :relative))
                            reverse
                            (take-while (partial x-aligned-bits? page-coords block))
                            format)
        directly-below (->> relatives
                            (filter (comp #(= % #{:below}) :relative))
                            (take-while (partial x-aligned-bits? page-coords block))
                            format)
        to-right (format (filter (comp #(= % #{:right}) :relative) relatives))
        to-left (format (filter (comp #(= % #{:left}) :relative) relatives))]
    {:num-blocks-above          (count (filter (comp :above :relative) relatives))
     :num-blocks-directly-above (count directly-above)
     :num-blocks-below          (count (filter (comp :below :relative) relatives))
     :num-blocks-directly-below (count directly-below)
     :num-blocks-right          (count to-right)
     :num-blocks-left           (count to-left)

     :gap-right                 (if (seq to-right) (- (:x0 (first to-right)) x1) MAX-GAP)
     :gap-left                  (if (seq to-left) (- x0 (:x1 (first to-left))) MAX-GAP)
     :gap-above                 (if (seq directly-above) (- y0 (:y1 (first directly-above))) MAX-GAP)
     :gap-below                 (if (seq directly-below) (- (:y0 (first directly-below)) y1) MAX-GAP)

     :blocks-directly-below (map :idx directly-below)
     :blocks-directly-above (map :idx directly-above)
     :blocks-left (map :idx to-left)
     :blocks-right (map :idx to-right)}))


(defn- core-features [{:keys [context-features internal-features]}]
  (merge internal-features (dissoc context-features :blocks-right :blocks-left :blocks-directly-above :blocks-directly-below)))


(defn enfeature [{:keys [x0 x1] :as page-meta} blocks]
  (let [idx-blocks (map (fn [i blk] (assoc blk :idx i)) (range) blocks)
        block-vec (->> idx-blocks
                       (map (comp
                              #(update % :internal-features dissoc :text-list :lines)
                              #(assoc % :context-features (context-features idx-blocks %))
                              #(assoc % :internal-features (internal-features (assoc % :page-x0 x0
                                                                                       :page-x1 x1))))))
        ;lookup (mapv core-features block-vec)
        lookup (mapv #(assoc % :features (core-features %)) block-vec)]
    (map (fn [{:keys [context-features internal-features] :as blk}]
           (-> blk
               (dissoc :context-features :internal-features :idx)
               (assoc :features (-> context-features
                                    (update :blocks-directly-above (partial map lookup))
                                    (update :blocks-directly-below (partial map lookup))
                                    (update :blocks-right (partial map lookup))
                                    (update :blocks-left (partial map lookup))
                                    (merge internal-features)))))
         block-vec)))
