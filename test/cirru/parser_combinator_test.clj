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

(deftest parse-expression-test
  (testing "testing parse-expression at \"\"aa\"\""
    (let
      [result (parse-expression (assoc initial-state :code "(a (b))"))]
      (are [keyname value] (= (keyname result) value)
        :failed false
        :value (list "a" (list "b"))
        :code ""))))

(deftest parse-block-line-test
  (testing "testing parse-block-line at \"a\n  b\n  c\""
    (let
      [result (parse-block-line (assoc initial-state :code "a\n  b\n  c"))]
      (are [keyname value] (= (keyname result) value)
        :failed false
        :value (list "a" (list "b") (list "c"))
        :code ""))))

(deftest parse-program-test
  (testing "testing parse-program at \"a\nb\n  c\nd\""
    (let
      [result (parse-program (assoc initial-state :code "a\nb\n  c\nd"))]
      (are [keyname value] (= (keyname result) value)
        :failed false
        :value (list ("a") (list "b" (list "c")) (list "d"))
        :code ""))))
