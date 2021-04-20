(ns pdf-parse.components.text
  "Functions for converting featured blocks into text and labels-with-text components"
  (:require [clojure.string :as s]
            [pdf-parse.common :as cmn]
            [pdf-parse.utilities :as utils]))

;TODO text fragmented (from odd boundary separating blocks and indentation)

(defn create-text-lines [token-stream]
  (let [token-stream (sort-by :y token-stream)]
    (map (partial sort-by :x)
         (reduce (fn [lines {txt :text :as datum}]
                   (let [{ptxt :text :as p-datum} (last (peek lines))]
                     (if (utils/new-line? (first (peek lines)) datum)
                       (if (re-matches #"[a-zA-Z]+[-]" ptxt)
                         (conj (pop lines) (conj (pop (peek lines)) (assoc p-datum :text (str (s/replace ptxt "-" "") txt))))
                         (if (= ptxt "-")
                           (let [last-line (peek lines)
                                 new-tkn (-> 2 (take-last last-line) first (update :text #(str % txt)))]
                             (conj (pop lines) (conj (into [] (drop-last 2 last-line)) new-tkn)))
                           (conj lines [datum])))
                       (conj (pop lines) (conj (peek lines) datum)))))
                 [[(first token-stream)]]
                 (rest token-stream)))))


(defn ->text [{:keys [tokens] :as blk}]
  (merge {:type :text
          :vals (create-text-lines tokens)}
         (dissoc blk :tokens :features :type :blocks)))

(def label-line #"(?:[a-zA-Z]+\s){0,5}[a-zA-Z]+\s*(?:[-+:]|[.]{2,})\s*")

(defn delimited-line? [words]
  (->> words (map :text) (s/join " ") (re-matches label-line)))

(defn key-column? [{words :tokens :as block}]
  (let [lines (utils/create-lines words)]
    (or
      (>= (/ (count (filter delimited-line? lines))
             (count lines))
          0.5)
      (utils/label-ish? block))))

(defn determine-vals [blocks {kc-x1 :x1 :as key-column}]
  (let [on-right (->> blocks
                       (filter #(= #{:right} (cmn/relative-to key-column %))))
        {x0 :x0 :as nearest} (->> on-right (sort-by :x0) first)
        neighbors (if nearest (->> blocks
                                   cmn/sort-blocks
                                   (drop-while #(not (:below (cmn/relative-to nearest %))))
                                   (take-while (fn [{tx0 :x0 words :tokens}]
                                                 (and (neg? (- x0 tx0 5)) ; horizontal indentation is at least as great as nearest's
                                                      (>= (-> nearest :tokens first :font-size)
                                                         (-> words first :font-size)))))))]
    (if (and nearest (< (- x0 kc-x1) 150)) (concat on-right neighbors))))

(defn label-vals-bounds [blocks keys-column]
  (if-let [vals (determine-vals blocks keys-column)]
    (reduce utils/expand-bounds keys-column vals)))

(defn ->labels-with-data
  "Merge components of labels with components of values.  Current implementation could be made
  more efficient with a little effort"
  [blocks _]
  (->> blocks
       (filter key-column?)
       (keep (partial label-vals-bounds blocks))
       (reduce (fn [boxes box]
                 (if (some #(cmn/within? % box) boxes)
                   boxes
                   (conj boxes box))) [])
       (reduce (fn [components box]
                 (conj components (-> box
                                      (dissoc :features :tokens)
                                      (assoc :type :text
                                             :vals (->> blocks
                                                        (filter (partial cmn/within? box))
                                                        (mapcat :tokens)
                                                        utils/create-lines))))) [])))
