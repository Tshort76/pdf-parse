(ns pdf-parse.non-tabular
  (:require
    [clojure.test :refer [is deftest testing]]
    [pdf-parse.test-utils :as u]
    [pdf-parse.core :as pdfx]
    [clojure.string :as s]))

(defn comp->text [{vals :vals}]
  (s/join " " (map #(s/join " " (map :text %)) vals)))

(deftest superscript-1
  (testing "superscripts in line do not result in a line break"
    (let [bond-lines  (->> {:format :components}
                           (pdfx/transform (u/->url "broken-bondlines.pdf"))
                           (filter #(= :text (:type %)))
                           (map comp->text)
                           (drop-while #(not (s/starts-with? % "$5,000,000"))))]
      (is (= (first bond-lines) "$5,000,000 7.00% Term Bond Due August 15, 2027, Priced to Yield 4.85% (2) - CUSIP (1) Suffix: AB8"))
      (is (= (second bond-lines) "$22,500,000 7.25% Term Bond Due August 15, 2038, Priced to Yield 5.95% (3) - CUSIP (1) Suffix: AC6")))))

(deftest labeled-sections-joined
  (testing "join components which consist of modifier and modified data"
    (->> (pdfx/transform (u/->url "no-colons.pdf") {:format :components})
         (some #(and (= :text (:type %))
                     (comp (partial s/includes? "Bonds Ratings: S&P AA+ Fitch AA+") comp->text)))
         is)))
