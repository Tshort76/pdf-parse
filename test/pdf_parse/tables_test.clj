(ns pdf-parse.tables-test
  (:require
   [clojure.test :refer [is deftest testing]]
   [pdf-parse.test-utils :as u]
   [pdf-parse.core :as pdfx]
   [clojure.string :as cs]))

;enumerate because the build machine has lots of files in the directory
(def test-files ["date_col_header"
                 "floating_table_fnma"
                 "headless_col"
                 "miss_half_row"
                 "sup_script2"
                 "sup_script_cols"
                 "exclude-footer"
                 "6bb83fa0016c5964960386114eee3167"
                 "26078ef4b91742ee7f8f570c50c3dd59"
                 "dcffd9abc30b3bba3e902a8c4bfcbc24"
                 "e42e3241d68d8277cdb9e2cb3e4b8737"
                 "43947e6fb15f197465a76e0f8162486e"
                 "b6e830ef4a2268660e12a5bf6cd11821"
                 "df6f6bdb4996271db86298569e8081cf"
                 "f92db828a4a26e07c2aa458715416828"
                 "1de268a7d431f3981a9f0e42a713b85b"
                 "c90e5ccc80752c982d5290746c80de6c"
                 "6525a551cd79cbda9bc0a22d89bd3bff"

                 "3b9ed589e1f1a7fd2cae2e0a90a965c6"
                 "81be9db629bc25007deeeb099718735b"
                 ;"033540634cdf29a2e297e440752e4466"  ;header is just messed up in this one
                 "a8b5c61f38c61efe03f158902a1244c5"
                 "d2ca6a93c360a6bc701528cb0b14b3ac"

                 "6450db8ad5fcc13510385c3bfe0f6206"
                 "74e3eced86fdabe41396f321f6a78482"

                 "d32bb584295403849dd4e43206096061"
                 "ed77a8b2720f0b52cbaad8e883487589"
                 "f0cce3b422e92d53e640ca1f87244aef"

                 ;Mandatory Redemption Schedule layouts
                 "562199DE6"
                 "page-footer-in-table"
                 "54811A4J9"
                 "crammed_tables"
                 "little-tables"
                 "mand_redemp2"
                 "single-line-tables-with-titles"
                 ;"title-in-table3"  ;header is slightly messed up in this one
                 "title-table-side-by-side"
                 "unique-table-layout"

                 "bond_lines_under1"
                 "bond_lines_under2"
                 "bond_lines_under3"  ;TODO fix
                 "d5e39b0f282a2359e5daffd12232f27f"
                 ;"1045ac0581bcbc22f1328d2cd6002b7d"
                 ])

(defn make-pretty [tbl]
  (-> tbl
      (update :vals
              (partial mapv (partial mapv (comp
                                            (partial cs/join " ")
                                            (partial map :text)))))
      (update :x0 int)
      (update :x1 int)
      (update :y0 int)
      (update :y1 int)))

(defn compare-vals
  "if all values are equal, return true
  otherwise, return the unequal values"
  [{tbl1 :vals} {tbl2 :vals}]
  (is (not
        (->> (map (partial map #(hash-map :old %1 :new %2 :equal? (= %1 %2))) tbl1 tbl2)
             flatten
             (remove :equal?)
             (map #(select-keys % [:old :new]))
             not-empty))))

(defn test-file [^String filename]
  (testing filename
    (let [expect (:tables (u/load-expected (str filename ".edn")))
          tbls (->> {:format :components}
                    (pdfx/transform (u/->url (str filename ".pdf")))
                    (filter #(= :table (:type %))))
          pretty (map make-pretty tbls)]
      (is (= (count expect) (count tbls)) "table count")
      (dorun (map compare-vals expect pretty)))))

;use to update the .edn file for a doc
(defn store-expected [^String filename]
  (->> {:format :components}
       (pdfx/transform (u/->url (str filename ".pdf")))
       (filter #(= :table (:type %)))
       (mapv make-pretty)
       (assoc {} :tables)
       (spit (str u/test-dir filename ".edn"))))

(defn annotate-test-pdfs [dir]
  (map
    #(do (println "Annotating: " %) (-> % (str ".pdf") u/->url (pdfx/annotate-components {:output-directory dir})))
    test-files))

(deftest table-regressions?
  (testing "A set of pdf tables for parsing regressions"
    (dorun (map test-file test-files))))