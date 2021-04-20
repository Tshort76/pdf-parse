(ns pdf-parse.components.tables
  "Functions for composing table components from featured blocks."
  (:require [clojure.string :as s]
            [clojure.set :as cset]
            [pdf-parse.utilities :as utils]
            [pdf-parse.components.columnar :as cols]
            [pdf-parse.common :as cmn]
            [pdf-parse.components.parse :as prs]
            [taoensso.timbre :as t]))

(defn remove-dot-padding [word]
  (let [word-text (s/trim (:text word)) dot-cnt (count (re-find #"[.]{2,}" word-text))]
    (if (pos? dot-cnt)
      (cond
        (re-matches #"[.]{2,}" word-text)
        (assoc word :width 0 :text "")
        (re-matches #".+[.]{2,}" word-text)
        (assoc word :text (s/replace word-text #"[.]{2,}" " ") :width (- (:width word) (* 3 dot-cnt))) ;dot-width = 3
        (re-matches #"[.]{2,}.+" word-text)
        (assoc word :text (s/replace word-text #"[.]{2,}" " ") :x (+ (:x word) (* 3 dot-cnt)) :width (- (:width word) (* 3 dot-cnt)))
        :else
        word)
      word)))


(defn clean-up [row-data]
  (->> row-data
       (remove #(re-matches #"[.$]*" (s/replace (:text %) #"\s+" ""))) ;remove whitespaces, single periods, and $'s
       (map remove-dot-padding)))

(defn summary-row? [block stream]
  (some->> stream
           (filter #(= #{:right} (cmn/relative-to block %)))
           not-empty
           (map (comp
                  #(select-keys % [:num-datapoints :num-tokens])
                  :features))
           (apply merge-with +)
           ((fn [{:keys [num-datapoints num-tokens]}]
              (if (and num-tokens (pos? num-tokens))
                (>= (/ num-datapoints num-tokens) 0.5))))))

(def page-footer #"(?:[Pp][aAgGeE]{0,3}\s*[.]?\s*\d.*)|[-]?\s*\d{0,3}\s*[-]?")
(def footnote #"(?:[*+†]|\(\d{1,2}\))\s*[a-zA-Z]+.*")

(defn page-footer? [{:keys [tokens]} stream]
  (and
    (re-matches page-footer (s/join " " (map :text tokens)))
    (<= (count stream) 2)))

(defn table-footer? [{:keys [tokens]}]
  (let [as-text (s/join " " (map :text tokens))]
    (or
      (re-matches utils/dash-line as-text)                  ;____ or ----- line
      (:superscript? (first tokens))                        ;line starts with a superscript
      (re-matches footnote as-text))))                      ;lines starts with what should be a superscript


(defn tokens->column-bounds [tokens]
  (->> tokens utils/create-lines prs/column-bounds))


(defn ruins-columns? [{tbl-tokens :tokens} {blk-tokens :tokens}]
  (if (and (seq tbl-tokens) (seq blk-tokens))
    (let [empty-col-pixels (->> tbl-tokens tokens->column-bounds cols/columns-as-pixel-range)
          blk-occupied-pixels (->> blk-tokens
                                   (sort-by :x)
                                   (map (fn [{:keys [x width]}] {:x0 (int x) :x1 (int (+ x width))}))
                                   cols/columns-as-pixel-range)
          overlap (cset/intersection empty-col-pixels blk-occupied-pixels)]
      (> (/ (count overlap)
            (max (count empty-col-pixels) 1)) 0.2))))


(defn indirectly-ruins-columns? [tbl candidate-block all-blocks]
  (let [hypothesis-bounds (utils/expand-bounds tbl candidate-block)]
    (when
      (ruins-columns? tbl (->> all-blocks
                               (filter #(and
                                          (= #{:below} (cmn/relative-to tbl %))
                                          (cmn/within? hypothesis-bounds %)))
                               (mapcat :tokens)
                               (assoc {} :tokens)))
      (t/trace "invalid extension of form: ruins-table-columns\n"
               "<table> : " (pr-str (->> tbl :tokens (map :text))) "\n"
               "<block> : " (pr-str (map (partial :text) (:tokens candidate-block))))
      true)))


(defn grouping? [table
                 {{:keys [num-tokens
                          num-lines]} :features
                  tokens              :tokens
                  :as                 this}
                 stream]
  (if num-tokens
    (and
      (< num-tokens 8)
      (= 1 num-lines)
      (re-matches utils/header-regex (s/join " " (map :text tokens)))
      (some #(and
               (#{:data-cell :table-column} (:class %))
               (= #{:below} (cmn/relative-to table %))
               (utils/within-x? 25 (:y0 %) (:y1 table))) stream)
      ;data below it and within table bounds
      (not (indirectly-ruins-columns? table this stream)))))


(defn table-block? [component [nxt-block & stream]]
  (or
    (cmn/data-column? nxt-block)
    (grouping? component nxt-block stream)
    (summary-row? nxt-block stream)))


(defn terminate-down? [component stream]
  (let [nxt-block (first stream)
        tracer (fn [name form]
                 (when form
                   (t/trace "invalid extension of form: " name "\n"
                            "<table> : " (pr-str (->> component :tokens (map :text) reverse)) "\n"
                            "<block> : " (pr-str (map (partial :text) (:tokens nxt-block))))
                   form))]
    (or
      (tracer :page-footer (page-footer? nxt-block stream))
      (tracer :table-footer (table-footer? nxt-block))
      (tracer :not-tabular (not (table-block? component stream)))
      (tracer :too-far-down
              (> (- (:y0 nxt-block) (or (:y1 component) (->> component :blocks (map :y0) (apply max)))) 50)))))


(defn ->table [words coords]
  (merge {:type :table
          :vals (prs/parse-table (->> coords (cmn/words-in-box words) clean-up utils/create-lines))}
         (dissoc coords :tokens)))


(defn add-to-table [table {:keys [page-number tokens] :as blk}]
  (-> table
      (assoc :page-number page-number)
      (utils/expand-bounds blk)
      (update :blocks #(conj % blk))
      (update :tokens #(concat % tokens))))


(defn headers-within-columns [{:keys [tokens] :as tbl} all-blocks]
  (let [block-as-lines (utils/create-lines tokens)]
    (when-let [index (cmn/header-index block-as-lines)]
      (let [head-block (->> block-as-lines (take (inc index)) flatten cmn/boundaries-of)
            {:keys [x0 x1 y0]} (some->> all-blocks
                                        (filter (fn [blk]
                                                  (and
                                                    (= #{} (cmn/relative-to tbl blk)) ;partially inline with other header row
                                                    (let [rel (cmn/relative-to head-block blk)]
                                                      (or (= #{:right} rel) (= #{:left} rel))))))
                                        (mapcat :tokens)
                                        cmn/boundaries-of)
            _ (t/trace "Found headers within the columns: " (pr-str (map :text tokens)))]
        {:y0 (apply min (or y0 1000) (map (fn [{:keys [y height]}] (- y height)) tokens))
         :y1 (apply max (map :y (nth block-as-lines index)))
         :x0 (or x0 1000) :x1 (or x1 0)}))))


(defn header-like? [table-tokens candidate-blocks]
  (and
    (>= (/ (count (filter identity (map utils/label-like? candidate-blocks)))
           (max (count candidate-blocks) 1))
        0.5)
    (cmn/header-index (utils/create-lines (apply concat table-tokens (map :tokens candidate-blocks))))))


(defn units-row? [blocks]
  (and
    (every? (fn [{:keys [tokens]}]
              (re-matches #"\(.*|\[.*" (s/join (map :text tokens)))) blocks)
    (do (t/trace "Units-only row found in table") true)))


(defn headers-above [{:keys [tokens y0] :as tbl} all-blocks]
  (let [above-blocks (filter (fn [blk]
                               (= #{:above} (cmn/relative-to tbl blk))) all-blocks)
        lowest-above (some->> above-blocks not-empty (apply max-key :y1))
        row-above (filter #(#{#{:right} #{:left} #{}} (cmn/relative-to lowest-above %)) above-blocks)
        above-headers (if (units-row? row-above)
                        (let [nxt-lowest (some->> above-blocks not-empty
                                                  (filter #(:above (cmn/relative-to lowest-above %)))
                                                  (apply max-key :y1))]
                          (filter #(let [rel (cmn/relative-to nxt-lowest %)]
                                     (#{#{:right} #{:left} #{}} rel)) above-blocks))
                        row-above)]
    (when (and (header-like? tokens above-headers)
               (utils/within-x? 2 (count (tokens->column-bounds tokens))
                                (count (tokens->column-bounds (mapcat :tokens above-headers))))
               (< (- y0 (apply max (map :y1 above-headers))) 50))
      (t/trace "Found headers Above : " (pr-str (map (comp (partial map :text) :tokens) above-headers)))
      (reduce add-to-table {:x0 1000 :x1 0 :y0 1000 :y1 0} above-headers))))


(defn headers-inline
  "To deal with a collection of blocks which contain both headers and data AND just data"
  [{:keys [tokens] :as tbl} all-blocks]
  (let [tbl-xys (->> tokens (map (fn [{:keys [x0 y0]}] (str x0 "_" y0))) (into #{}))
        inline-candidates (filter
                            (fn [{bx0 :x0 by0 :y0 :as blk}]
                              (and
                                (= #{} (cmn/relative-to tbl blk))
                                (not (tbl-xys (str bx0 "_" by0))))) all-blocks)]
    (when (header-like? tokens inline-candidates)
      (t/trace "Found headers inline with some table columns")
      (reduce add-to-table {:x0 1000 :x1 0 :y0 1000 :y1 0} inline-candidates))))


(defn grab-headers [all-blocks {:keys [tokens] :as tbl}]
  (or
    (t/trace "Looking at headers for: " (pr-str (map :text tokens)))
    (headers-within-columns tbl all-blocks)
    (headers-inline tbl all-blocks)
    (headers-above tbl all-blocks)
    (t/trace "No Headers found!")))


;awkward since some functions need to look ahead in the block stream ...
(defn extend-down [table blocks]
  (let [blocks-below (some->> blocks
                              (filter #(= #{:below} (cmn/relative-to table %)))
                              not-empty
                              (sort-by :y0))]
    (->> blocks-below
         (reduce (fn [{:keys [cmpnt stream]} block]
                   (if (or (terminate-down? cmpnt stream)
                           (indirectly-ruins-columns? cmpnt block blocks))
                     (reduced {:cmpnt cmpnt})
                     {:cmpnt  (add-to-table cmpnt block)
                      :stream (rest stream)}))
                 {:cmpnt  table
                  :stream blocks-below})
         :cmpnt)))

(defn extend-horizontally [direction all-blocks {:keys [x0] :as table}]
  (let [close? (fn [tbl col] (< (case direction
                                  :left (- (:x0 tbl) (:x1 col))
                                  :right (- (:x0 col) (:x1 tbl))) 200))]
    (->> all-blocks
         (filter (comp #{#{direction}} (partial cmn/relative-to table)))
         (sort-by #(Math/abs (- (:x0 %) x0)))
         (reduce (fn [tbl {:keys [class] :as col}]
                   (if (and
                         (let [font-mode #(some->> % :content (remove :superscript?) (map :font-size) not-empty frequencies (apply max-key second) first)
                               font-1 (->> tbl :content first font-mode)
                               font-2 (font-mode col)]
                           (or (not font-1) (not font-2) (= font-1 font-2)))
                         (close? tbl col)
                         (#{:column-n-header :table-column :data-cell :enum} class))
                     (add-to-table tbl col)
                     (reduced tbl)))
                 table))))

(defn blocks->table-boundary [all-blocks]
  (let [uniques-only (fn [x] (->> x
                                  (sort-by (fn [{:keys [x0 x1 y0 y1]}]
                                             (* -1.0 (- x1 x0) (- y1 y0)))) ;by area
                                  (reduce (fn [uniques tbl]
                                            (if (some #(cmn/within? % tbl) uniques)
                                              uniques
                                              (conj uniques tbl))) [])))]
    (some->> all-blocks
             (filter (comp #{:column-n-header :table-column :data-cell} :class))
             (reduce (fn [tables cand]
                       (if (some (comp #(and % (% cand)) :blocks) tables)
                         tables
                         (conj tables
                               (->> cand
                                    (add-to-table {:x0 1000 :x1 0 :y0 1000 :y1 0 :blocks #{}})
                                    (extend-horizontally :right all-blocks)
                                    (extend-horizontally :left all-blocks))))) [])

             (filter #(> (count (:blocks %)) 1))
             uniques-only
             (keep (fn [{:keys [blocks y1] :as tbl}]
                     (when-let [headers (grab-headers all-blocks tbl)]
                       (assoc (utils/expand-bounds tbl headers) :y1 y1 :tokens (mapcat :tokens blocks)))))
             not-empty
             (map (comp
                    #(dissoc % :blocks)
                    #(extend-down % all-blocks)))
             uniques-only)))


(defn sane-table? [blocks {vals :vals :as table}]
  (and
    (>= (count vals) 2)
    (not (some #(let [intersection (cmn/overlap-ratio table %)]
                  (when (and (> intersection 0.0) (< intersection 0.9))
                    (t/trace "Insane Table: " (->> vals flatten (map :text) pr-str) "\n"
                             "Overlaps block: " (pr-str (map :text (:tokens %))))
                    true)) blocks))))


(defn ->standard-tables [blocks page]
  (->> blocks
       blocks->table-boundary
       (map (partial ->table page))
       (filter (partial sane-table? blocks))))