(ns pdf-parse.spec
  "Contains the spec definitions for pdf-parse data structures."
  (:require [clojure.spec.alpha :as s]))

(def coordinate? (s/and number? #(> 1000 % 0)))
(def pos-num? (s/and number? pos?))

;page-num_x-coord-of-left_y-coord-of-bottom
(s/def ::id string?)
(s/def ::text string?)
(s/def ::bold? boolean?)
(s/def ::italic? boolean?)
(s/def ::superscript? boolean?)
(s/def ::font-size (s/and number? (comp not neg?)))
(s/def ::x coordinate?)
(s/def ::y coordinate?)
(s/def ::height pos-num?)
(s/def ::width pos-num?)
(s/def ::page-number nat-int?)

(s/def ::x0 coordinate?)
(s/def ::x1 coordinate?)
(s/def ::y0 coordinate?)
(s/def ::y1 coordinate?)

(s/def ::boundaries
  (s/and (s/keys :req-un [::x0 ::x1 ::y0 ::y1 ::page-number])
         (fn [{:keys [x0 x1 y0 y1]}]
           (and (< y0 y1) (< x0 x1)))))

(s/def ::raw-token
  (s/keys :req-un [::text ::bold? ::font-size ::italic? ::x ::y ::height ::width ::page-number]))

(s/def ::token
  (s/keys :req-un [::text ::font-size ::id ::height ::width ::x ::y ::page-number]
          :opt-un [::bold? ::italic? ::superscript?]))

(s/def ::tokens (s/coll-of ::raw-token))

(s/def ::block
  (s/merge ::boundaries (s/keys :req-un [::features ::tokens])))

;;;;;;;;;;;;;;;;;;;;;;
;;   Components     ;;
;;;;;;;;;;;;;;;;;;;;;;
(s/def :table/type #{:table "table"})
(s/def :table/vals (s/coll-of (s/coll-of (s/coll-of ::token))))
(s/def :component/table
  (s/merge ::boundaries (s/keys :req-un [:table/type :table/vals])))

(s/def :text/type #{:text "text"})
(s/def :text/vals (s/coll-of (s/coll-of ::token)))
(s/def :component/text
  (s/merge ::boundaries (s/keys :req-un [:text/type :text/vals])))

(s/def ::components (s/coll-of (s/or :table :component/table
                                    :text :component/text) :max-count 10))

(def components? (partial s/explain ::components))
;(gen/sample (s/gen ::components))

;; Generating samples
;(-> ::component s/gen gen/sample pp/pprint)
;(-> ::xrange s/gen gen/sample pp/pprint)
