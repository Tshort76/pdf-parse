(ns pdf-parse.common
  "A set of helper functions for composing and assessing blocks and components"
  (:require [pdf-parse.utilities :as utils]
            [clojure.string :as s]))

(defn centered? [page-x0 page-x1 {:keys [x0 x1]}]
  (and (> x0 (+ 50 page-x0))                                ;not just left aligned and spanning entire page
       (utils/within-x? 10 (/ (+ x0 x1) 2.0) (/ (+ page-x0 page-x1) 2.0))))

(defn overlap-ratio
  "Calculates how much overlap there is between two boxes, as a percentage of the area of the second box."
  [{tx0 :x0 tx1 :x1 ty0 :y0 ty1 :y1}
   {:keys [x0 x1 y0 y1] :as box-b}]
  (let [area-of (fn [{:keys [x0 x1 y0 y1]}]
                  (when (and (> x1 x0) (> y1 y0))
                    (* (- x1 x0) (- y1 y0))))]
    (if-let [intersect (area-of {:x0 (max tx0 x0) :x1 (min tx1 x1)
                                 :y0 (max ty0 y0) :y1 (min y1 ty1)})]
      (/ intersect (area-of box-b))
      0.0)))

(defn relative-to
  "Returns the position of second box relative to the first box.  Result is a set that contains
  those of the 4 directions are applicable to the relationship. The empty set means that the boxes overlap."
  ([b1 b2] (relative-to 3 b1 b2))
  ([pad {:keys [x0 x1 y0 y1]} {sx0 :x0 sx1 :x1 sy0 :y0 sy1 :y1}]
   (cond-> #{}
           (< (- sy1 pad) y0) (conj :above)
           (> (+ sy0 pad) y1) (conj :below)
           (< (- sx1 pad) x0) (conj :left)
           (> (+ sx0 pad) x1) (conj :right))))


(defn within?
  "Convenience function, calls relative-to and checks for the empty set"
  [a b] (empty? (relative-to 0 a b)))

(defn in-box? [box {:keys [y x]}] (within? box {:x0 x :x1 x :y0 y :y1 y}))

(defn words-in-box [words box-coords]
  (filter (partial in-box? box-coords) words))

(defn boundaries-of [block]
  (reduce (fn [{:keys [x0 x1 y0 y1]}
               {xw :x ww :width yw :y hw :height page :page-number}]
            {:x0          (min x0 xw)
             :x1          (max x1 (+ xw ww))
             :y0          (min y0 (- yw hw))
             :y1          (max y1 yw)
             :page-number page})
          {:x0 1000 :x1 0 :y0 1000 :y1 0} block))

(defn sort-blocks [blocks]
  (if-let [blocks (not-empty (sort-by :y0 blocks))]
    (->> (reduce (fn [lines block]
                   (if (:below (relative-to (first (peek lines)) block))
                     (conj lines [block])
                     (conj (pop lines) (conj (peek lines) block))))
                 [[(first blocks)]]
                 (rest blocks))
         (map (partial sort-by :x0))
         (apply concat))
    blocks))

(defn data-column? [{{:keys [num-datapoints num-tokens
                             num-lines]} :features}]
  (if (and num-tokens (pos? num-tokens))
    (and (>= (/ num-datapoints num-tokens) 0.5)
         (< (/ num-tokens num-lines) 6))))

(def token-types
  {:word    #"[a-zA-Z]{2,}.*"
   :numeric #"[$]?\s*(?:\d{1,3}[,])*\d*[.]?\d+\s*[%]?"
   :date    #"\d{1,2}/\d{1,2}/\d{2,4}|[a-zA-Z]{3,}\s*[.]?\s*\d{1,2}\s*[,]?\s*\d{2,4}"})

(defn tokens-type [tokens]
  (let [tkn (s/join " " (map :text tokens))]
    (reduce #(if (re-matches (get token-types %2) tkn)
               (conj %1 %2)
               %1) #{} (keys token-types))))

(defn header-index [lines]
  (->> lines
       (map (comp
              (fn [{:keys [word] :as freqs}]
                (/ (or word 0) (max 1 (reduce #(+ %1 (second %2)) 0 freqs))))
              frequencies
              (partial mapcat (comp tokens-type vector))))
       (take-while #(> % 0.5))
       (#(if (pos? (count %)) (dec (count %))))))           ;zero based index

(defn data-and-labels? [{{:keys [num-tokens
                                 num-lines]} :features
                         content             :content}]
  (and
    (< num-tokens 30)
    (< (/ num-tokens num-lines) 8)
    (->> (utils/create-lines content)
         (keep #(let [txt (s/join " " (map :text %))]
                  (cond
                    (re-matches utils/datapoint txt) :data
                    (re-matches utils/header-regex txt) :header)))
         ((fn [labels] (and
                         (-> labels first (= :header))
                         (-> labels last (= :data))))))))
