(ns commented-imart-detection.core-test
  (:require [clojure.test :refer :all]
            [commented-imart-detection.core :refer :all]))

(deftest test-parse-1
  (is (= (reduce parse [(type-n "")] "aaaaaaaa")
         [{:type :normal :data "aaaaaaaa"}] )))

(deftest test-parse-2
  (is (= (reduce parse [(type-n "")] "aaaaa<!--aaa-->")
         [{:type :normal :data "aaaaa"}
          {:type :comment :data "<!--aaa-->"}
          {:type :normal :data ""} ] )))

(deftest test-parse-line-1
  (is (= (parse-line :comment "abcde")
         [{:type :comment :data "abcde"}])))

(deftest test-parse-line-2
  (is (= (parse-line :comment "abc-->de")
         [{:type :comment :data "abc-->"} {:type :normal :data "de"}])))

(deftest test-parse-line-3
  (is (= (parse-line :comment "abc<!--de-->fg")
         [{:type :comment :data "abc<!--de-->"} {:type :normal :data "fg"}])))

(deftest test-extract-html-comment-1
  (is (= (extract-html-comment ["<!--abc"
                               "def<!--"
                               "ghi-->jkl"
                               "mno<!--pqr"
                               "stu-->vwx"])
         [{:type :comment :data "<!--abc" :line-number 1}
          {:type :comment :data "def<!--" :line-number 2}
          {:type :comment :data "ghi-->"  :line-number 3}
          {:type :comment :data "<!--pqr" :line-number 4}
          {:type :comment :data "stu-->"  :line-number 5}])))

(deftest test-extract-imart-comment-1
  (is (= (extract-imart-comment ["<!--abc<imart type=\"string\" />"
                               "def<!--"
                               "ghi-->jkl<imart>"
                               "mno<!--pqr"
                               "st</imart>u-->v<imart>wx"])
         [{:type :comment :data "<!--abc<imart type=\"string\" />" :line-number 1}
          {:type :comment :data "st</imart>u-->"  :line-number 5}])))
