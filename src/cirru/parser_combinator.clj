(ns cirru.parser-combinator)

; values

(def open-paren "(")
(def close-paren ")")
(def whitespace " ")
(def line-break "\n")
(def double-quote "\"")
(def specials-in-token "() \n\t")
(def specials-in-string "\"\\\n")

; declare

(declare parse-indentation)
(declare parse-line-breaks)
(declare parse-two-blanks)
(declare parse-escaped)
(declare parse-expression)
(declare parse-item)
(declare parse-block)

; utils

(defn match-first [state character]
  (= (subs (:code state) 0 1) character))

(defn fail [state msg]
  (assoc state :failed true :msg msg))

; combinators

(defn helper-or [state parsers]
  (println "helper-or" state)
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
    (println "combine-or" state)
    (helper-or state parsers)))

(defn helper-chain [state parsers]
  (println "helper-chain" state)
  (if (> (count parsers) 0)
    (let
      [parser (first parsers)
        result (parser state)]
      (if (:failed result)
        (fail state "failed apply chaining")
        (recur result (rest parsers))))
    state))

(defn combine-chain [& parsers]
  (fn [state]
    (println "combine-chain" state)
    (helper-chain state parsers)))

(defn combine-times [parser n]
  (combine-chain (repeat n parser)))

(defn helper-many [state parser counter]
  (println "helper-many" state counter)
  (let
    [result (parser state)]
    (if (:failed result)
      (if (> counter 0) state
        (fail state "matching 0 times"))
      (recur result parser (+ counter 1)))))

(defn combine-many [parser]
  (fn [state]
    (println "combine-many" state)
    (helper-many state parser 0)))

(defn combine-not [parser]
  (fn [state]
    (println "combine-not" state)
    (let
      [result (parser state)]
      (if (:failed result)
        (assoc result :failed false :msg "recovered in not")
        (fail result "should not be this")))))

(defn combine-optional [parser]
  (fn [state]
    (println "combine-optional" state)
    (let
      [result (parser state)]
      (if (:failed result) state result))))

(defn combine-star [parser]
  (combine-optional (combine-many parser)))

(defn combine-peek [parser]
  (fn [state]
    (println "combine-peek" state)
    (let
      [result (parser state)]
      (if (:failed result)
        (fail state "peek failed")
        state))))

(defn combine-value [parser handler]
  (fn [state]
    (println "combine-value" state)
    (let
      [result (parser state)]
      (assoc result :value
        (handler (:value result))))))

(defn helper-alternate [state parser-1 parser-2 counter]
  (println "helper-alternate" state)
  (let
    [result (parser-1 state)]
    (if (:failed result)
      (if (> counter 0) state
        (fail state "not matching alternate rule"))
      (helper-alternate result parser-2 parser-1 (+ counter 1)))))

(defn combine-alternate [parser-1 parser-2]
  (fn [state]
    (helper-alternate state parser-1 parser-2 0)))

; parsers

(defn parse-quote [state]
  (println "parse-quote" state)
  (if (match-first state double-quote)
    (assoc state
      :code (subs (:code state) 1)
      :x (+ (:x state) 1)
      :value nil)
    (fail state "failed parsing double quote")))

(defn parse-whitespace [state]
  (println "parse-whitespace" state)
  (if (match-first state whitespace)
    (assoc state
      :code (subs (:code state) 1)
      :x (+ (:x state) 1)
      :value nil)
    (fail state "failed parsing whitespace")))

(defn parse-newline [state]
  (println "parse-newline" state)
  (if (match-first state line-break)
    (assoc state
      :code (subs (:code state) 1)
      :y (+ (:y state) 1)
      :x 1
      :value nil)
    (fail state "failed parsing newline")))

(defn parse-open-paren [state]
  (println "parse-open-paren" state)
  (if (match-first state open-paren)
    (assoc state
      :code (subs (:code state) 1)
      :y (+ (:y state) 1)
      :x 1
      :value nil)
    (fail state "failed parsing open paren")))

(defn parse-close-paren [state]
  (println "parse-close-paren" state)
  (if (match-first state close-paren)
    (assoc state
      :code (subs (:code state) 1)
      :y (+ (:y state) 1)
      :x 1
      :value nil)
    (fail state "failed parsing close paren")))

(defn parse-special-in-token [state]
  (println "parse-special-in-token" state)
  (if
    (= (:code state) "")
    (fail state "eof for special in token")
    (let
      [code-first (subs (:code state) 0 1)
        code-rest (subs (:code state) 1)
        result (assoc state :code code-rest :value code-first)]
      (if
        (.contains specials-in-token code-first)
        result
        (fail result "found no special in token")))))

(defn parse-special-in-string [state]
  (println "parse-special-in-string" state)
  (let
    [code-first (subs (:code state) 0 1)
      code-rest (subs (:code state) 1)
      result (assoc state :code code-rest :value code-first)]
    (if
      (.contains specials-in-string code-first)
      result
      (fail result "found no special in string"))))

(defn parse-eof [state]
  (println "parse-eof" state)
  (if (= (:code state) "")
    state
    (fail state "expected EOF")))

(defn parse-indent [state]
  (println "parse-indent" state)
  (let
    [result (parse-indentation state)]
    (if
      (> (:value result) (:indentation result))
      (assoc state
        :indentation (+ (:indentation result) 1))
      (fail result "no indent"))))

(defn parse-unindent [state]
  (println "parse-unindent" state)
  (let
    [result (parse-indentation state)]
    (if
      (< (:value result) (:indentation result))
      (assoc state
        :indentation (- (:indentation result) 1))
      (fail result "no unindent"))))

(defn parse-align [state]
  (println "parse-align" state)
  (let
    [result (parse-indentation state)]
    (if
      (= (:value result) (:indentation state))
      result
      (fail result "not aligned"))))

(defn parse-escaped-newline [state]
  (println "parse-escaped-newline" state)
  (if
    (= (subs (:code state) 0 2) "\\n")
    (assoc state :code (:subs (:code state) 2))
    (fail state "no escaped newline")))

(defn parse-escaped-tab [state]
  (println "parse-escaped-tab" state)
  (if
    (= (subs (:code state) 0 2) "\\t")
    (assoc state :code (:subs (:code state) 2))
    (fail state "no escaped tab")))

(defn parse-escaped-double-quote [state]
  (println "parse-escaped-double-quote" state)
  (if
    (= (subs (:code state) 0 2) "\\\"")
    (assoc state :code (:subs (:code state) 2))
    (fail state "no escaped double quote")))

(defn parse-escaped-backslash [state]
  (println "parse-escaped-backslash" state)
  (if
    (= (subs (:code state) 0 2) "\\\\")
    (assoc state :code (:subs (:code state) 2))
    (fail state "no escaped backslash")))

(defn parse-string-char [state]
  (println "parse-string-char" state)
  (if
    (> (count (:code state)) 1)
    ((combine-not parse-special-in-string) state)
    (fail state "no more char for string")))

(defn parse-token-char [state]
  (println "parse-token-char" state)
  (if
    (> (count (:code state)) 1)
    ((combine-not parse-special-in-token) state)
    (fail state "no more char for token")))

; generated parser

(def parse-string
  (combine-chain parse-quote
    (combine-many (combine-or parse-string-char parse-escaped))
    parse-quote))

(def parse-token-end
  (combine-peek
    (combine-or parse-whitespace parse-close-paren parse-newline)))

(def parse-escaped
  (combine-or parse-escaped-newline parse-escaped-tab
    parse-escaped-double-quote
    parse-escaped-backslash))

(def parse-token (combine-many parse-token-char))

(def parse-expression-item
  (combine-or parse-token parse-string parse-expression))

(def parse-expression
  (combine-chain parse-open-paren
    (combine-alternate parse-whitespace parse-item)
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
    (combine-times parse-whitespace 2)
    (fn [value] 1)))

(def parse-line-breaks
  (combine-chain
    (combine-star parse-empty-line)
    parse-newline))

(def parse-item
  (combine-or parse-token parse-string parse-expression))

(def parse-indentation
  (combine-value
    (combine-chain
      (combine-value parse-line-breaks (fn [value] nil))
      (combine-value (combine-many parse-two-blanks)
        (fn [value] (count value))))
    (fn [value] (first value))))

(def parse-inner-block
  (combine-chain parse-indent
    (combine-alternate parse-block parse-align)
    parse-unindent))

(def parse-block-line
  (combine-chain
    (combine-alternate parse-item parse-whitespace)
    (combine-optional parse-inner-block)))

(def parse-block
  (combine-chain
    (combine-alternate parse-block-line parse-align)))

(def parse-program
  (combine-chain
    (combine-optional parse-line-breaks)
    (combine-alternate parse-block parse-align)
    parse-line-eof))

; exposed methods

(defn parse [code]
  (let
    [initial {:code code
      :value nil :msg ""
      :indentation 0
      :failed false}]
    (parse-program initial)))
