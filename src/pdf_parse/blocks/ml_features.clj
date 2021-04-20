(ns pdf-parse.blocks.ml-features
  (:require [clojure.string :as s]))


(def vowel? #{\a \e \i \o \u \y})
(def consonant? #{\q \w \r \t \p \s \d \f \g \h \j \k \l \z \x \c \v \b \n \m})
(def digit? #{\1 \2 \3 \4 \5 \6 \7 \8 \9 \0})
(def other? identity)


(defn token-summary [string]
  (->> string
       s/lower-case
       (take 10)
       (map (apply some-fn (map (partial apply (fn [class pred] #(when (pred %) class))) [[[1 0 0 0] vowel?]
                                                                                          [[0 1 0 0] consonant?]
                                                                                          [[0 0 1 0] digit?]
                                                                                          [[0 0 0 1] other?]])))
       (#(concat % (repeat 10 [0 0 0 0]))) ;pad to make fixed width
       (take 10)))



;TODO can we ensure that this stays up to date with the graph above ... at compile or test time maybe...
;TODO maybe make each element a vector, where the second element is the default value
;TODO would it make sense to have an exists? slot for each context item so that we can fire on that?
(def features-seq "ensure consistent ordering for the sake of vector construction"
  [[:avg-data-cnt-per-line 0]
   [:num-datapoints 0]
   [:num-sentences 0]
   [:all-caps-ratio 0]
   [:data-ratio 0]
   [:width 0]
   [:bold-ratio 0]
   [:avg-word-cnt-per-line 0]
   [:horizontal-alignment [0 0]]
   [:keyword-start? 0]
   [:num-tokens 0]
   [:num-lines 0]
   [:ends-with-period? 0]
   [:itemized-start? 0]
   [:tokens-per-line 0]
   [:num-english-words 0]
   [:italic-ratio 0]
   [:word-ratio 0]
   [:height 0]
   [:first-token-summary (token-summary "")]

   [:num-blocks-above 0]
   [:num-blocks-directly-above 0]
   [:num-blocks-directly-below 0]
   [:num-blocks-below 0]
   [:num-blocks-right 0]
   [:num-blocks-left 0]

   [:gap-right 0]
   [:gap-left 0]
   [:gap-above 0]
   [:gap-below 0]])

(defn core-vectorize [features]
  (let [feature-order (map first features-seq)]
    (map #(get {false 0 true 1 nil 0} % %)
         (-> features
             (update :horizontal-alignment {:left [1 0] :center [0 1] :float [0 0]})
             (map feature-order)
             flatten))))

(defn context-vectors
  "Ensures consistent features vector length by ensuring a consistent number of
  neighbors.  Adds all zero vectors when context is missing."
  [context-window-size context]
  (->> (repeat context-window-size (flatten (map second features-seq)))
       (concat (map core-vectorize context))
       (take context-window-size)
       (apply concat)))

(defn block-id [{:keys [page-number x0 y0]}]
  (when (and x0 y0)
    (str page-number "_" (int y0) "_" (int x0))))

(defn ml-vectorize [{{:keys [blocks-directly-below blocks-directly-above
                             blocks-left blocks-right] :as features} :features :as block}]
  {:id (block-id block)
   :feature-vec (concat (core-vectorize features)
                        (context-vectors 2 blocks-directly-above)
                        (context-vectors 2 blocks-directly-below)
                        (context-vectors 2 blocks-left)
                        (context-vectors 2 blocks-right))
   :text (:text features)})




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      Random Forest       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn core-feats [features]
  (->> (dissoc features :blocks-directly-below :blocks-directly-above
               :blocks-left :blocks-right :text :lines :text-list)
       (map (fn [[k v]] [k (get {false 0 true 1 nil 0} v v)]))
       (into {})))

(defn contextual-feats
  "Ensures consistent features vector length by ensuring a consistent number of
  neighbors.  Adds all zero vectors when context is missing."
  [key-prefix core-features window-size context]
  (->> core-features
       (map (fn [[k _]] [k 0]))
       (into {})
       (repeat window-size)
       (concat (map core-feats context))
       (take window-size)
       (map (fn [idx features]
              (into {} (map (fn [[k v]] [(keyword (s/join "-" [key-prefix idx (name k)])) v]) features))) (range))
       (apply merge)))

(defn rand-forest-features [{{:keys [blocks-directly-below blocks-directly-above
                                     blocks-left blocks-right] :as features} :features :as block}]
  (let [core (core-feats features)]
    {:id          (block-id block)
     :feature-vec (-> core
                      (merge (contextual-feats "above" core 2 blocks-directly-above))
                      (merge (contextual-feats "below" core 2 blocks-directly-below))
                      (merge (contextual-feats "left" core 2 blocks-left))
                      (merge (contextual-feats "right" core 2 blocks-right)))
     :text        (:text features)})

  )


