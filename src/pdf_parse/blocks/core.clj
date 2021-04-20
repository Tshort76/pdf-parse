(ns pdf-parse.blocks.core
  "Functions for grouping words into semantic chunks, where whitespace and
  font changes are used to infer boundaries."
  (:require [pdf-parse.utilities :as utils]
            [pdf-parse.common :as cmn]
            [clojure.set :as s]))

(defn vertically-near? [{:keys [y1 height]}
                        {by0 :y0 b-height :height}]
  (< (- by0 y1) (* 0.75 (min height b-height))))

(defn weird-change? [{page-x0 :x0 page-x1 :x1} {:keys [tokens x0] :as seg1} {bx0 :x0 b-tokens :tokens :as seg2}]
  (or
    (and (cmn/centered? page-x0 page-x1 seg1) (not (cmn/centered? page-x0 page-x1 seg2)) (not (utils/within-x? 20 x0 bx0)))
    (not (and (seq (s/intersection (set (map :font tokens)) (set (map :font b-tokens))))
              (seq (s/intersection (set (map :font-size tokens)) (set (map :font-size b-tokens))))))))

(defn horizontally-aligned? [{:keys [x0 x1]}
                             {bx0 :x0 bx1 :x1}]
  (or
    (utils/within-x? 5 x0 bx0)                              ;left aligned
    (utils/within-x? 5 x1 bx1)                              ;right aligned
    (utils/within-x? 5 (/ (+ x0 x1) 2) (/ (+ bx0 bx1) 2)))) ;midpoint aligned

;TODO use relative-to ?
(defn y-boundary-between? [graphics {ay0 :y0 ax0 :x0 ax1 :x1} {by1 :y1}]
  (some
    (fn [{:keys [x0 x1 y0 y1 boundary-axis]}]
      (and (= :y boundary-axis)                             ;boundary separates along the y axis
           (>= y0 (inc ay0))                                ;boundary is below top segment
           (>= (dec by1) y1)                                ;boundary is above bottom segment
           (>= (- (min ax1 x1) (max ax0 x0)) 4.0)           ;sufficient overlap in horizontal direction
           (> (/ (- x1 x0) (- ax1 ax0)) 0.5)))              ;boundary is large relative to the segment (so as to ignore underlined words in sentences)
    graphics))

(defn filter-candidates [segment other-segments page-dimensions graphics blocks]
  (:candidates
    (reduce (fn [{:keys [candidates] :as box} seg]
              (if (and (= #{:below} (cmn/relative-to box seg))
                       (vertically-near? (last candidates) seg)
                       (not (y-boundary-between? graphics (last candidates) seg))
                       (or
                         (:superscript? seg)
                         (and
                           (not (weird-change? page-dimensions (last candidates) seg))
                           (horizontally-aligned? (last candidates) seg))))

                ;now check if adding this segment would lead to merge side-by-side segments
                (let [{cands :candidates :as hypothesis} (-> box (utils/expand-bounds seg) (update :candidates conj seg))]
                  (if (some (fn [seg-x] (and (> (cmn/overlap-ratio hypothesis seg-x) 0.75) ;within block
                                             (not (some (partial = seg-x) cands)))) ;but not a candidate
                            (concat other-segments blocks))
                    (reduced box)
                    hypothesis))
                box))
            (assoc segment :candidates [segment])
            other-segments)))


(defn group-vertically [segment other-segments other-blocks graphics page-dimensions]
  (let [calc-diff #(- (:y0 (second %)) (:y1 (first %)))
        pairs (->> other-blocks
                   (filter-candidates segment other-segments page-dimensions graphics)
                   (partition 2 1))]
    (case (count pairs)
      0 [segment]
      1 (first pairs)
      (:block (reduce (fn [{:keys [last-diff block]} pair]  ;group by magnitude of separation
                        (let [diff (calc-diff pair)]
                          (cond
                            (or                             ;same block
                              (< diff 0.5)                  ;deal with division by 0
                              (< (Math/abs (- 1.0 (/ last-diff diff))) 0.5) ;close as a percent of last diff
                              (utils/within-x? 2 last-diff diff))
                            {:last-diff diff :block (conj block (second pair))}
                            (> diff last-diff)
                            (reduced {:block block})
                            :else (reduced {:block (butlast block)}))))
                      {:last-diff (calc-diff (first pairs))
                       :block     (into [] (first pairs))}
                      (rest pairs))))))


(def format-block
  (comp
    #(select-keys % [:x0 :x1 :y0 :y1 :page-number :tokens])
    (partial reduce (fn [res {:keys [tokens] :as seg}]
                      (-> res
                          (utils/expand-bounds seg)
                          (update :tokens #(concat % tokens)))))))


(defn compose-blocks [segments graphics]
  (let [page-dimensions {:x0 (apply min (map :x0 segments))
                         :x1 (apply max (map :x1 segments))}]
    (loop [blocks []
           remaining segments]
      (if-not (seq remaining)
        blocks
        (let [block (group-vertically (first remaining) (rest remaining) blocks graphics page-dimensions)]
          (recur (conj blocks (format-block block)) (remove (into #{} block) remaining)))))))