(ns pdf-parse.utilities
  "Utility functions that are used at various points in the transformation processes."
  (:require [clojure.string :as s]))

(def dummy-token {:y 1.0 :width 1.0 :font "Pseudo-text" :id "0_0_0" :font-size 0.0 :page-number 1 :x 1.0 :height 1.0 :text ""})

(def punctuation #"['\")(:,;*-.]+")
(def eng-wordy #"[a-zA-Z]*[aeiou][a-z]*")
(def all-caps #"[A-Z]+")
(def datapoint #".*(?:\d|[A-Z]{2,}).*|[B-Z]|[-]|[A-Z]+[/-][A-Z]+")
(def sentence-ending #".*[\da-zA-Z\)][.!?][\"\'\)]?")
(def header-regex #".*[a-zA-Z]{2,}.*[^.!?]\s*[\"\'\)]?")
(def dash-line #"\s*(?:[-]{3,}|[_]{3,})\s*")
;(def delimited #".*(?:[:-]|[.]{2,})")
(def delimited #".*\:")

(defn gap? [{x0 :x w0 :width t0 :text}
            {x1 :x w1 :width t1 :text}]
  (>= (- x1 (+ x0 w0))                                      ;gap on right
      (* 2.25 (min (/ w0 (max 1 (count t0)))
                   (/ w1 (max 1 (count t1)))))))

(defn between?
  "Is a between b and c, inclusive of both bounds?"
  [a b c] (and (>= a b) (<= a c)))


(defn partition-when
  "Partitions seq-data by creating a new partition whenever predicate returns true.
  Predicate should expect two arguments, the previous datum and the current datum."
  [predicate seq-data]
  (reduce (fn [partitions datum]
            (if-let [prev-datum (peek (peek partitions))]
              (if (predicate prev-datum datum)
                (conj partitions [datum])
                (conj (pop partitions) (conj (peek partitions) datum)))
              (conj partitions [datum])))
          [] seq-data))


(defn cpartition-when
  "Partitions seq-data by creating a new partition whenever predicate returns true.
  Predicate should expect two arguments, the current partition and the remaining stream of data.
  Note that the current partition will be a vector, so the most recently item added with be available at (last X).

  Examples:

    (utils/cpartition-when :current (fn [p stream] (> (reduce + p) (first stream))) [1 2 3 6 10 11 22])
      => [[1 2 3 6] [10 11 22]]

    (utils/cpartition-when (fn [p stream] (>= (last p) (apply max stream))) [1 2 3 6 22 11 2] )
      => [[1 2 3 6 22] [11] [2]]

  "
  ([predicate seq-data]
   (cpartition-when :current predicate seq-data))
  ([context-level predicate seq-data]
   (loop [partitions []
          stream seq-data]
     (if (empty? stream)
       partitions
       (if (or (empty? partitions)
               (predicate (case context-level
                            :all partitions
                            (peek partitions)) stream))
         (recur (conj partitions [(first stream)]) (rest stream))
         (recur (conj (pop partitions) (conj (peek partitions) (first stream))) (rest stream)))))))



(defn label-ish? [{{:keys [num-tokens
                           num-lines
                           bold-ratio]} :features
                   tokens             :tokens}]
  (and
    (pos? num-tokens)
    (< (/ num-tokens num-lines) 5)
    (> bold-ratio 0.75)
    (re-matches header-regex (s/join " " (map :text tokens)))))

(defn label-like? [{{:keys [num-tokens
                            num-lines]} :features
                    tokens             :tokens}]
  (and
    (< num-tokens 14)
    (< (/ num-tokens num-lines) 8)
    (re-matches header-regex (s/join " " (map :text tokens)))))

(defn expand-bounds [component {:keys [x0 x1 y0 y1]}]
  (-> component
      (update :y0 (partial min y0))
      (update :y1 (partial max y1))
      (update :x0 (partial min x0))
      (update :x1 (partial max x1))))

(defn within-x? [x a b]
  (<= (Math/abs (- a b)) x))

(defn new-line? [{prev-y :y px :x pw :width p-ss? :superscript? ph :height}
                 {y :y x :x ss? :superscript? h :height}]
  (cond
    (and p-ss? ss? (not (within-x? 2 prev-y y))) true
    p-ss? (> (- y h prev-y ph) (* -1 (/ ph 2)))
    (and ss? (< (- x (+ px pw)) 3)) (not (between? y (- prev-y ph) prev-y))
    :else (> (- y h prev-y) (* -1 (/ h 2)))))

(defn asci-line? [{prev-y :y} {y :y t :text}]
  (and (pos? (- y 1 prev-y))
       (= "_" t)))

(defn create-lines [positional-data]
  (->> positional-data
       (sort-by :y)
       (cpartition-when (fn [[line-start] [nxt]]
                          (or (new-line? line-start nxt)
                              (asci-line? line-start nxt))))
       (map (partial sort-by :x))))


(defn token->ann-format [{:keys [x width y height] :as token}]
  (assoc token :x0 x :x1 (+ x width) :y0 (- y height) :y1 y))

(defn add-tabs [tokens]
  (->> tokens
       (partition-when gap?)
       (map (comp
              (partial s/join " ")
              (partial map :text)))
       (s/join "\t")))

(defn tokens->text [tokens]
  (->> tokens
       create-lines
       (map add-tabs)
       (s/join "\n")))

(defn infer-column-boundaries [{vals :vals}]
  (->> vals
       (apply mapv vector)
       (map
         (comp
           #(hash-map :x0 (apply min (map :x %))
                      :y0 (apply min (map (fn [{:keys [y height]}] (- y height)) %))
                      :x1 (apply max (map (fn [{:keys [x width]}] (+ x width)) %))
                      :y1 (apply max (map :y %))
                      :page-number (:page-number (first %))
                      :type :table-column)
           (partial remove #(= "0_0_0" (:id %)))
           flatten))
       (concat (let [headers (->> (get vals 0) flatten (remove #(= "0_0_0" (:id %))))
                     pg (->> headers first :page-number)
                     y1 (->> headers (map :y) (apply max))]
                 [{:x0 (apply min (map :x headers))
                   :x1 (apply max (map (fn [{:keys [x width]}] (+ x width)) headers))
                   :y0 y1 :y1 y1 :page-number pg :type :table-column}]))))


(defn euc-distance
  "Calculates the euclidean distance between the centers of two rectangles"
  [{:keys [x0 x1 y0 y1]}
   {bx0 :x0 bx1 :x1 by0 :y0 by1 :y1}]
  (let [p1 [(/ (+ x1 x0) 2) (/ (+ y1 y0) 2)]
        p2 [(/ (+ bx1 bx0) 2) (/ (+ by1 by0) 2)]]
    (Math/sqrt (+ (Math/pow (- (first p1) (first p2)) 2)
                  (Math/pow (- (second p1) (second p2)) 2)))))

(defn IoU
  "Calculates the Intersection over Union (ratio of intersecting area to the total area) for two boxes."
  [{tx0 :x0 tx1 :x1 ty0 :y0 ty1 :y1 :as box-a}
   {:keys [x0 x1 y0 y1] :as box-b}]
  (let [area-of (fn [{:keys [x0 x1 y0 y1]}]
                  (when (and (> x1 x0) (> y1 y0))
                    (* (- x1 x0) (- y1 y0))))]
    (if-let [intersect (area-of {:x0 (max tx0 x0) :x1 (min tx1 x1)
                                 :y0 (max ty0 y0) :y1 (min y1 ty1)})]
      (/ intersect
         (- (+ (area-of box-a) (area-of box-b)) intersect))
      0.0)))