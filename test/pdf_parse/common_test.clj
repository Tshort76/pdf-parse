(ns pdf-parse.common-test
  (:require
    [pdf-parse.common :as c]
    [clojure.test :refer [deftest is testing]]))

(deftest relative-to-test
  (testing "Overlap is correctly determined based on bounds"
    (is (empty? (c/relative-to {:x0 0 :x1 10 :y0 0 :y1 10}
                               {:x0 5 :x1 10 :y0 0 :y1 10})))

    (is (empty? (c/relative-to {:x0 0 :x1 10 :y0 5 :y1 10}
                               {:x0 0 :x1 10 :y0 0 :y1 10})))

    (is (empty? (c/relative-to {:x0 10 :x1 20 :y0 10 :y1 20}
                               {:x0 10 :x1 20 :y0 10 :y1 20})) "same box is within itself")

    (is (empty? (c/relative-to {:x0 0 :x1 10 :y0 0 :y1 10}
                               {:x0 5 :x1 15 :y0 0 :y1 10})) "overlaps but not inside")

    (is (= #{:right} (c/relative-to {:x0 0 :x1 10 :y0 0 :y1 10}
                                    {:x0 30 :x1 40 :y0 0 :y1 10})) "No overlap, far to right")

    (is (= #{:below} (c/relative-to {:x0 0 :x1 10 :y0 0 :y1 10}
                                    {:x0 0 :x1 10 :y0 10 :y1 20})) "only borders touch")

    (is (= #{:below :right} (c/relative-to {:x0 0 :x1 10 :y0 0 :y1 10}
                                           {:x0 10 :x1 20 :y0 10 :y1 20})) "only borders touch 2")

    (is (= #{:below} (c/relative-to {:x0 0 :x1 10 :y0 0 :y1 10}
                                    {:x0 0 :x1 10 :y0 8 :y1 20})) "small overlap relative to block size")

    (is (= #{:left} (c/relative-to {:x0 392 :x1 417 :y0 141 :y1 152}
                                   {:x0 301 :x1 338 :y0 128 :y1 152})) "larger block alongside small one")

    (is (= #{:right} (c/relative-to {:x0 301 :x1 338 :y0 128 :y1 152}
                                    {:x0 392 :x1 417 :y0 141 :y1 152})) "small block alongside large one, transitivity")))
