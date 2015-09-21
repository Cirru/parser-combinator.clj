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

(defn combine-times [parser n]
  (combine-chain (repeat n parser)))

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

(defn combine-star [parser]
  (combine-optional (combine-many parser)))

(defn combine-peek [parser]
  (fn [state]
    (let
      [result (parser state)]
      (if (:failed result)
        (failed state "peek failed")
        state))))

(defn combine-value [parser handler]
  (fn [state]
    (let
      [result (parser state)]
      (assoc result :value
        (handler (:value result))))))

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

(defn parse-open-paren [state]
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

(defn parse-eof [state]
  (if (= (:code state) "")
    state
    (fail state "expected EOF")))

(defn parse-indent [state]
  (let
    [result (parse-indentation state)]
    (if
      (> (:value result) (:indentation result))
      (assoc state
        :indentation (+ (:indentation result) 1))
      (fail state "no indent"))))

(defn parse-unindent [state]
  (let
    [result (parse-indentation state)]
    (if
      (< (:value result) (:indentation result))
      (assoc state
        :indentation (- (:indentation result) 1))
      (fail state "no unindent"))))

(defn parse-align [state]
  (let
    [result (parse-indentation state)]
    (if
      (= (:value result) (:indentation state))
      result
      (fail state "not aligned"))))

(defn parse-indentation [state]
  (combine-value
    (combine-chain
      (combine-value parse-line-breaks (fn [value] nil))
      (combine-value (combine-many parse-two-blanks)
        (fn [value] (count value))))
    (fn [value] (first value))))

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

(def parse-expression
  (combine-chain parse-open-paren
    (combine-many parse-item)
    parse-close-paren))

(def parse-empty-line
  (combine-chain parse-newline
    (combine-star parse-whitespace)
    (combine-peek (combine-or parse-newline parse-eof))))

(def parse-line-eof
  (combine-chain
    (combine-star parse-whitespace)
    (combine-star parse-empty-line)
    parse-eof))

(def parse-two-blanks
  (combine-value
    (combine-twice parse-whitespace 2)
    (fn [value] 1)))

(def parse-line-breaks
  (combine-chain
    (combine-star parse-empty-line)
    parse-newline))

(def parse-item
  (combine-or parse-token parse-string parse-expression
    parse-block))

(def parse-block
  (combine-chain
    parse-item
    (combine-many
      (combine-or parse-item parse-line-breaks))
    parse-block-end))

(def parse-block-end)

(def parse-program
  (combine-many (combine-or parse-line-breaks parse-block)))

; exposed methods

(defn parse [code filename]
  (let
    [initial {:code code
      :x 1 :y 1 :value nil :msg ""
      :filename filename
      :indentation 0
      :failed false}]
    (parse-program initial)))
