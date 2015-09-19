(ns cirru.parser-combinator)

; values

(def open-paren "(")
(def close-paren ")")
(def whitespace " ")
(def line-break "\n")
(def double-quote "\"")
(def specials-in-token "() \n\t")
(def specials-in-string "\"\\\n")

; utils

(defn match-first [state character]
  (= (subs (:code state) 0 1) character))

(defn fail [state msg]
  (assoc state :failed true :msg msg))

; combinators

(defn helper-or [state parsers]
  (if (> (count parsers) 0)
    (let
      [parser (first parsers)
        result (parser state)]
      (if (:failed result)
        (recur state (rest parsers))
        result))
    (fail state "no parser is successful")))

(defn combine-or [& parsers]
  (fn [state]
    (helper-or state parsers)))

(defn helper-chain [state parsers]
  (if (> (count parsers) 0)
    (let
      [parser (first parsers)
        result (parser state)]
      (if (:failed result)
        (fail result "failed apply chaining")
        (recur state (rest parser))))
    state))

(defn combine-chain [parsers]
  (fn [state]
    (helper-chain state parsers)))

(defn helper-many [state parser counter]
  (let
    [result (parser state)]
    (if (:failed parser)
      (if (> counter 0) state
        (fail state "matching 0 times"))
      (recur result parser (+ counter 1)))))

(defn combine-many [parser]
  (fn [state]
    (helper-many state parser 0)))

(defn combine-not [parser]
  (fn [state]
    (let
      [result (parser state)]
      (if (:failed result)
        (assoc result :failed false :msg "recovered in not")
        (fail result "should not be this")))))

(defn combine-optional [parser]
  (fn [state]
    (let
      [result (parser state)]
      (if (:failed result) state result))))

(defn combine-peek [parser]
  (fn [state]
    (let
      [result (parser state)]
      (if (:failed result)
        (failed state "peek failed")
        state))))

; parsers

(defn parse-quote [state]
  (if (match-first state double-quote)
    (assoc state
      :code (subs (:code state) 1)
      :x (+ (:x state) 1)
      :value nil)
    (fail state "failed parsing double quote")))

(defn parse-whitespace [state]
  (if (match-first state whitespace)
    (assoc state
      :code (subs (:code state) 1)
      :x (+ (:x state) 1)
      :value nil)
    (fail state "failed parsing whitespace")))

(defn parse-newline [state]
  (if (match-first state line-break)
    (assoc state
      :code (subs (:code state) 1)
      :y (+ (:y state) 1)
      :x 1
      :value nil)
    (fail state "failed parsing newline")))

(defn parse-open-parent [state]
  (if (match-first state open-paren)
    (assoc state
      :code (subs (:code state) 1)
      :y (+ (:y state) 1)
      :x 1
      :value nil)
    (fail state "failed parsing open paren")))

(defn parse-close-paren [state]
  (if (match-first state close-paren)
    (assoc state
      :code (subs (:code state) 1)
      :y (+ (:y state) 1)
      :x 1
      :value nil)
    (fail state "failed parsing close paren")))

(defn parse-special-in-token [state]
  (let
    [code-first (subs (:code state) 0 1)
      code-rest (subs (:code state) 1)
      result (assoc state :code code-rest:value code-first)]
    (if
      (.contains specials-in-token code-first)
      result
      (fail state "found no special in token"))))

(defn parse-special-in-string [state]
  (let
    [code-first (subs (:code state) 0 1)
      code-rest (subs (:code state) 1)
      result (assoc state :code code-rest:value code-first)]
    (if
      (.contains specials-in-string code-first)
      result
      (fail state "found no special in string"))))

; generated parser

(def parse-string
  (combine-chain parse-quote
    (combine-many (combine-or parse-string-chars parse-escaped))
    parse-quote))

(def parse-token-end
  (combine-peek
    (combine-or parse-whitespace parse-close-paren parse-newline)))

(def parse-escaped
  (combine-or parse-escaped-newline parse-escaped-tab
    parse-escaped-double-quote
    parse-escaped-backslash))

(def parse-token (combine-many (combine-not parse-special-token)))

(defn parse-expression [])

(defn parse-indentation [])

(defn parse-unindentation [])

(defn parse-program [state])

; exposed methods

(defn parse [code filename]
  (let
    [initial {:code code
      :x 1 :y 1 :value nil :msg ""
      :filename filename
      :indentation 0
      :failed false}]
    (parse-program initial)))
