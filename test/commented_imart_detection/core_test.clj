(ns commented-imart-detection.core-test
  (:require [clojure.test :refer :all]
            [commented-imart-detection.core :refer :all]))

(deftest test-analyse-1
  (is (= (reduce analyse [(type-n "")] "aaaaaaaa")
         [{:type :normal :data "aaaaaaaa"}] )))

(deftest test-analyse-2
  (is (= (reduce analyse [(type-n "")] "aaaaa<!--aaa-->")
         [{:type :normal :data "aaaaa"}
          {:type :comment :data "<!--aaa-->"}
          {:type :normal :data ""} ] )))

(deftest test-analyze-line-1
  (is (= (analyze-line :comment "abcde")
         [{:type :comment :data "abcde"}])))

(deftest test-analyze-line-2
  (is (= (analyze-line :comment "abc-->de")
         [{:type :comment :data "abc-->"} {:type :normal :data "de"}])))

(deftest test-analyze-line-3
  (is (= (analyze-line :comment "abc<!--de-->fg")
         [{:type :comment :data "abc<!--de-->"} {:type :normal :data "fg"}])))

(deftest test-extract-html-comment-1
  (is (= (->> ["<!--abc"
               "def<!--"
               "ghi-->jkl"
               "mno<!--pqr"
               "stu-->vwx"]
              (add-line-number)
              (analyze-html)
              (flatten-analyzed-items)
              (filter is-c?))
         [{:type :comment :data "<!--abc" :line-number 1}
          {:type :comment :data "def<!--" :line-number 2}
          {:type :comment :data "ghi-->"  :line-number 3}
          {:type :comment :data "<!--pqr" :line-number 4}
          {:type :comment :data "stu-->"  :line-number 5}])))

(deftest test-extract-imart-comment-1
  (is (= (->> ["<!--abc<imart type=\"string\" />"
               "def<!--"
               "ghi-->jkl<imart>"
               "mno<!--pqr"
               "st</imart>u-->v<imart>wx"]
              (extract-imart-comment))
         [{:type :comment :data "<!--abc<imart type=\"string\" />" :line-number 1}
          {:type :comment :data "st</imart>u-->"  :line-number 5}])))
