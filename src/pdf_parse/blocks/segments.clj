(ns pdf-parse.blocks.segments
  (:require [plumbing.core :refer [fnk]]
            [plumbing.graph :as graph]
            [clojure.string :as s]
            [pdf-parse.common :as cmn]
            [pdf-parse.utilities :as utils]))

;TODO handle this (breaks non-overlapping rectangle assumption ...)
; (i)    (A) Government Rating Agency of any state of the
; payment of which is secured by an



(def data-rgx #".*(?:\d|[?$%]).*")
(def word-rgx #"(?:(?:[a-zA-Z]*[aeiou][a-z]*)|(?:[aiouAIOU][a-z]?))\,?\.?")
(def loose-word-rgx #"\S?[a-zA-Z]*[aeiouAEIOU][a-zA-Z]*\S?")
(def font-noise #"[A-Z]+[+]|[Ii][Tt][Aa][Ll][iI][cC][sS]?|[=,-.]")
(def footnote #"(?:[*+†]|\(\d{1,2}\))")


(defn font-clean [font]
  (s/replace font font-noise ""))

(defn new-font? [{font2 :font font-size2 :font-size} {:keys [font font-size]}]
  (or (not= (font-clean font) (font-clean font2))
      (not= font-size font-size2)))

(defn horizontal-gap? [{x0 :x w0 :width t0 :text y0 :y fsize1 :font-size :as word1}
                       {x1 :x w1 :width t1 :text y1 :y fsize2 :font-size :as word2}]
  (>= (- x1 (+ x0 w0))                                      ;gap on right
      (* (cond
           (and (new-font? word1 word2)                   ;between words with differing font and y vals (side by side components!)
                (> (Math/abs (- y0 y1)) 0.5))
           2.0
           (or (s/ends-with? t0 ",")
               (s/starts-with? t1 ",")
               (= "$" t0))
           5.5

           (re-matches #"[a-zA-Z].*\..{0,3}" t0)                                  ;sentence endings are problematic
           5.0
           :else 3.5)
         (min (/ w0 (max 1 (count t0)))
              (/ w1 (max 1 (count t1)))))))

(defn superscript-joined? [segment-tokens tokens]
  (let [{:keys [x width] ss1 :superscript? :as a} (last segment-tokens)
        {x-b :x b-width :width ss2 :superscript? :as b} (first tokens)]
    (or
      (and ss1 (if-let [{left-x :x left-w :width} (last (butlast segment-tokens))]
                 (< (- x-b (+ x width)) (- x (+ left-w left-x)))
                 true))                                     ;nothing is on the left, so close to something on the right
      (and ss2 (if-let [{right-x :x} (second tokens)]
                 (< (- x-b (+ x width)) (- right-x (+ x-b b-width)))
                 true)))))


(defn line-break? [{fsize1 :font-size ss1 :superscript? :as a}
                   {fsize2 :font-size ss2 :superscript? :as b}]
  (or (horizontal-gap? a b)
      (and
        (not (or ss1 ss2))
        (< (/ (min fsize1 fsize2) (max fsize1 fsize2)) 0.75))))

(defn boundary-between? [boundaries {:keys [x width y class text]} {right-token-x :x right-token-class :class}]
  (some (fn [{:keys [x0 x1 y0 y1 boundary-axis] boundary-class :class}]
          (and (or
                 (= boundary-class :graphic)
                 (not (and (= :word class) (= right-token-class :word))) ;for visual feature boundaries
                 (s/ends-with? text ":"))
               (= :x boundary-axis)                         ;The boundary separates along the x axis
               (>= (inc x0) (+ x width))                          ;bound starts after the end of the left token (wiggle room of 1)
               (>= (inc right-token-x) x1)                        ;bound ends before the start of the right token (wiggle room of 1)
               (utils/between? (dec y) y0 y1)))                  ;bound is in horizontal alignment
        boundaries))

(def word-rgx #"[^\d]*[aeiouyAEIOUY]+[^\d]*")

(def segment-decor-graph
  {:text-seq          (fnk [tokens] (mapv :text tokens))
   :text              (fnk [text-seq] (s/join " " text-seq))
   :height            (fnk [y0 y1] (- y1 y0))
   :punctuation-cnt   (fnk [text] (count (re-seq #"[;!.,?'\"]" text)))
   :numeric-cnt       (fnk [text-seq] (count (filter (partial re-find #"\d") text-seq)))
   :all-caps-cnt      (fnk [text] (count (re-seq #"[A-Z]{2,}" text)))
   :word-cnt          (fnk [text-seq] (count (filter (partial re-matches word-rgx) text-seq)))
   :token-cnt         (fnk [tokens] (count tokens))
   :delimiter-ending? (fnk [text-seq] (boolean (re-matches #".*:\s*" (last text-seq))))
   :class             (fnk [numeric-cnt word-cnt]
                        (cond
                          (and (> word-cnt numeric-cnt) (pos? word-cnt)) :text
                          (pos? numeric-cnt) :data))
   })

(def decorate (graph/compile segment-decor-graph))

(defn decorate-segment [tokens]
  (-> tokens
      cmn/boundaries-of
      (assoc :tokens tokens)
      ((juxt identity decorate))
      ((partial apply merge))))

(defn compose-segments [tokens & [visual-boundaries]]
  (->> tokens
       (utils/cpartition-when :current
                              (fn [part stream]
                                (let [{ax :x :as tkn-a} (last part)
                                      {bx :x :as tkn-b} (first stream)]
                                  (or (> ax bx)
                                      (utils/new-line? tkn-a tkn-b)
                                      (and
                                        (line-break? tkn-a tkn-b)
                                        (not (superscript-joined? part stream)))
                                      (boundary-between? visual-boundaries tkn-a tkn-b)))))
       (map decorate-segment)))
