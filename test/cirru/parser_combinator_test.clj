(ns cirru.parser-combinator-test
  (:require [clojure.test :refer :all]
            [cirru.parser-combinator :refer :all]))

(deftest parse-token-test
  (testing "testing parse-token at \"aa\""
    (let
      [result (parse-token (assoc initial-state :code "aa"))]
      (are [keyname value] (= (keyname result) value)
        :failed false
        :value "aa"
        :code ""))))

(deftest parse-item-test
  (testing "testing parse-item at \"aa\""
    (let
      [result (parse-item (assoc initial-state :code "aa"))]
      (are [keyname value] (= (keyname result) value)
        :failed false
        :value "aa"
        :code ""))))

(deftest parse-string-test
  (testing "testing parse-string at \"\"aa\"\""
    (let
      [result (parse-string (assoc initial-state :code "\"aa\""))]
      (are [keyname value] (= (keyname result) value)
        :failed false
        :value "aa"
        :code ""))))
