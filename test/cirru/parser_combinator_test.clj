(ns cirru.parser-combinator-test
  (:require [clojure.test :refer :all]
            [cirru.parser-combinator :refer :all]
            [cheshire.core :as cheshire]))

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
        :value (list (list "a") (list "b" (list "c")) (list "d"))
        :code ""))))

(defn parse-file [x]
  (let
    [file (str "cirru/" x ".cirru")]
    (:value (pare (slurp file)))))

(defn parse-json [x]
  (let
    [file (str "ast/" x ".json")]
    (into [] (cheshire/parse-string (slurp file)))))

(deftest pare-comma
  (testing "pare line"
    (is (= (parse-file "line") (parse-json "line")))))

(deftest pare-demo
  (testing "pare demo"
    (is (= (parse-file "demo") (parse-json "demo")))))

(deftest pare-folding
  (testing "pare folding"
    (is (= (parse-file "folding") (parse-json "folding")))))

(deftest pare-html
  (testing "pare html"
    (is (= (parse-file "html") (parse-json "html")))))

(deftest pare-indent
  (testing "pare indent"
    (is (= (parse-file "indent") (parse-json "indent")))))

(deftest parse-indent-test
  (testing "testing parse-indent-test at nested indent"
    (let
      [result (parse-program
        (assoc initial-state :code "a\n    b\n  c" :original-code "a\n    b\n  c"))]
      (are [keyname value] (= (keyname result) value)
        :failed false
        :value (list (list "a" (list (list "b")) (list "c")))
        :code ""))))

(deftest pare-line
  (testing "pare line"
    (is (= (parse-file "line") (parse-json "line")))))

(deftest pare-parentheses
  (testing "pare parentheses"
    (is (= (parse-file "parentheses") (parse-json "parentheses")))))

(deftest pare-quote
  (testing "pare quote"
    (is (= (parse-file "quote") (parse-json "quote")))))

(deftest pare-spaces
  (testing "pare spaces"
    (is (= (parse-file "spaces") (parse-json "spaces")))))

(deftest pare-unfolding
  (testing "pare unfolding"
    (is (= (parse-file "unfolding") (parse-json "unfolding")))))
