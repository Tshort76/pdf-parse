(ns pdf-parse.tokens.core
  "Functions for converting a stream of character positions into
  a sequence of pages, where a page is a sequence of word positions."
  (:require [pdf-parse.utilities :as utils]
            [clojure.string :as s]
            [clojure.java.io :as io]
            [clojure.edn :as edn]))

(defn- paginate [char-positionals]
  (->> char-positionals
       (group-by :page-number)
       (sort-by key)
       (map second)))

(defonce short-word-set (->> "short-words.edn" io/resource slurp edn/read-string))

(defn classify-text [^String text]
  (when (seq text)
    (cond
      (re-matches #"(?:[*+†]|\(\d\))" text) :enum
      (re-matches #"\d+" (s/replace text #"[.,$%()]" "")) :numeric
      (or
        (re-matches #"\d{1,2}/\d{1,2}/\d{2,4}" text)
        #_(#{"Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sept" "Oct" "Nov" "Dec"
             "January" "February" "March" "April" "June" "July" "August" "September" "October" "November" "December"}
            (s/replace text #"[.'\"()]" ""))) :date
      (re-matches #"[%$*£&#.]" text) :symbol
      (or
        ;all caps, num characters less than 4 or some symbols in it
        (and
          (not (re-find #"[a-z]" text))
          (not (short-word-set (s/lower-case text)))
          (< (/ (count (re-seq #"[A-Z]" text))              ;num upper case chars per symbol
                (inc (count (re-seq #"[-/]" text)))) 4))
        (re-find #"\d" text)) :data
      (let [lwr-text (s/lower-case text)]
        (and
          (re-find #"[aeiouy]" lwr-text)
          (->> (s/replace lwr-text #"['\"(),;:]" "")
               (re-matches #"[a-z]+[.-]?")))) :word)))

(defn duplicate-char? [{x0 :x y0 :y text0 :text}
                       {:keys [x y text]}]
  (and
    (= text0 text)
    (< (Math/abs (- y y0)) 2)
    (neg? (- x x0))))

(defn same-word? [{x0 :x w0 :width y0 :y font-size0 :font-size cg :char-gap}
                  {:keys [x y font-size]}]
  (and
    ;(= font-size0 font-size)
    (<= (- x (+ w0 x0) 1) (or cg 0))
    (> x x0)
    (< (Math/abs (- y0 y)) 4)))

;is the superscript on the right of the modified term
(defn super-scripted-right? [{y0 :y font-size0 :font-size :as word}
                              {:keys [y font-size] :as char}]
  (and
    (not (utils/gap? word char))
    (< font-size font-size0)
    (pos? (- y0 y 1))))

(defn super-scripted-left? [words {:keys [y height font-size horizontal-bar?]}]
  (let [{ss-y :y ss-size :font-size ss-text :text
         ss-hbar? :horizontal-bar? :as ss-word} (peek words)]
    (and
      (not horizontal-bar?)
      (not ss-hbar?)
      (> font-size ss-size)
      (utils/between? ss-y (- y height height) (+ y height))
      (if-let [p-word (peek (pop words))]
        (or (utils/gap? p-word ss-word) (utils/new-line? p-word ss-word))
        true)
      (< (count ss-text) 4))))


(defn super-script? [{y0 :y font-size0 :font-size supe? :superscript?}
                       {:keys [y font-size]}]
  (and
    supe?
    (> font-size font-size0)
    (pos? (- y y0 1))))

(def whitespace #"\s*")

(defn build-tokens [chars]
  (reduce (fn [words {:keys [x width text] :as char}]
            (let [{wx :x wt :text cg :char-gap wwidth :width :as word} (peek words)]
              (cond
                (or (duplicate-char? word char)
                    (re-matches whitespace text)) words
                (super-scripted-right? word char) (conj words (assoc char :superscript? true))
                (super-scripted-left? words char) (conj (pop words)
                                                        (assoc word :superscript? true)
                                                        char)
                (super-script? word char) (conj words char)
                (same-word? word char) (conj (pop words)
                                             (assoc word :width (- (+ x width) wx)
                                                         :char-gap (/ (+ (or cg 0) (Math/abs (- x (+ wx wwidth))))
                                                                      (if (<= (count (:text word)) 2) 1 2))
                                                         :text (str wt text)))
                :else (conj words char))))
          [(first chars)]
          (rest chars)))

(def sort-tokens (comp (partial apply concat) utils/create-lines))

(defn page->token-stream [page]
  (->> page
       sort-tokens
       (drop-while #(re-matches whitespace (:text %)))
       build-tokens
       rseq
       (reduce (fn [words {:keys [y font-size] :as curr-word}]
                 (let [{wy :y wfont-size :font-size ss? :superscript?} (peek words)]
                   (conj words (if (and ss?
                                        (utils/within-x? 1 wy y)
                                        (= font-size wfont-size))
                                 (assoc curr-word :superscript? true)
                                 curr-word)))) [])
       rseq
       sort-tokens
       (map #(assoc % :class (classify-text (:text %))))))

(defn build-id [{:keys [page-number x y]}]
  (str page-number "_" (int x) "_" (int y)))

(defn text-positions->pages-of-tokens [text-positions]
  (->> text-positions
       (map #(assoc % :id (build-id %)))
       paginate
       (map page->token-stream)
       (filter first)))
