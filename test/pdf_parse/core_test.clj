(ns pdf-parse.core-test
  (:require
   [pdf-parse.core :as pdf]
   [clojure.test :refer [is deftest testing]]
   [pdf-parse.test-utils :as u]
   [clojure.string :as cs]))


(deftest basic-blocks-check
  (testing "Table parsed correctly"
    (let [blocks (pdf/transform (u/->url "nutrition.pdf") {:format :blocks})]
      (is (= 14 (count blocks)))
      (is (-> blocks last :class (= :page-footer)))
      (is (some (comp (partial re-matches #"[*]\s+Percent.*")
                      (partial cs/join " ")
                      (partial map :text)
                      :tokens) blocks) "join footnote block"))))


(deftest basic-components-check
  (testing "Table parsed correctly"
    (let [components (pdf/transform (u/->url "nutrition.pdf") {:format :components})
          table (first (filter (comp #(= :table %) :type) components))]
      (is table "Find the table")
      (is (-> table :vals first count (= 3)) "Correct column count")
      (is (-> table :vals count (= 6)) "Correct row count")
      (is (= 9 (count components)) "Correct number of components found"))))