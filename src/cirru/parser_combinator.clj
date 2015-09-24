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

(defn tabs [state]
  (assoc state :tab (str "  " (:tab state))))

(defn untabs [state]
  (assoc state :tab (subs (:tab state) 2)))

(defn log [name state]
  (println (:tab state) name (:code state) (:value state)))

; combinators

(defn helper-or [state parsers]
  (log "helper-or" state)
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
    (log "combine-or" state)
    (untabs (helper-or (tabs state) parsers))))

(defn helper-chain [state parsers]
  (log "helper-chain" state)
  (if (> (count parsers) 0)
    (let
      [parser (first parsers)
        result (parser state)]
      (if (:failed result)
        (fail state "failed apply chaining")
        (recur
          (assoc result :value
            (conj (into [] (:value state)) (:value result)))
          (rest parsers))))
    state))

(defn combine-chain [& parsers]
  (fn [state]
    (log "combine-chain" state)
    (untabs (helper-chain (assoc (tabs state) :value []) parsers))))

(defn combine-times [parser n]
  (fn [state]
    (let
      [method (combine-chain (repeat n parser))]
      (untabs (method (tabs state))))))

(defn helper-many [state parser counter]
  (log "helper-many" state)
  (let
    [result (parser state)]
    (if (:failed result)
      (if (> counter 0) state
        (fail state "matching 0 times"))
      (recur
        (assoc result :value
          (conj (into [] (:value state)) (:value result)))
        parser (+ counter 1)))))

(defn combine-many [parser]
  (fn [state]
    (log "combine-many" state)
    (untabs (helper-many (assoc (tabs state) :value (list)) parser 0))))

(defn combine-not [parser]
  (fn [state]
    (log "combine-not" state)
    (let
      [result (parser (tabs state))]
      (untabs (if (:failed result)
        (assoc result :failed false :msg "recovered in not")
        (fail result "should not be this"))))))

(defn combine-optional [parser]
  (fn [state]
    (log "combine-optional" state)
    (let
      [result (parser (tabs state))]
      (untabs (if (:failed result) state result)))))

(defn combine-star [parser]
  (fn [state]
    (log "combine-star" state)
    (let
      [method (combine-optional (combine-many parser))]
      (untabs (method (tabs state))))))

(defn combine-peek [parser]
  (fn [state]
    (log "combine-peek" state)
    (let
      [result (parser (tabs state))]
      (untabs (if (:failed result)
        (fail state "peek failed")
        state)))))

(defn combine-value [parser handler]
  (fn [state]
    (log "combine-value" state)
    (let
      [result (parser state)]
      (assoc result :value
        (handler (:value result))))))

(defn helper-alternate [state parser-1 parser-2 counter]
  (log "helper-alternate" state)
  (let
    [result (parser-1 state)]
    (if (:failed result)
      (if (> counter 0) state
        (fail state "not matching alternate rule"))
      (helper-alternate
        (assoc result :value
          (conj (into [] (:value state)) (:value result)))
        parser-2 parser-1 (+ counter 1)))))

(defn combine-alternate [parser-1 parser-2]
  (fn [state]
    (untabs (helper-alternate
      (assoc (tabs state) :value (list)) parser-1 parser-2 0))))

; parsers

(defn parse-quote [state]
  (log "parse-quote" state)
  (if (match-first state double-quote)
    (assoc state
      :code (subs (:code state) 1)
      :x (+ (:x state) 1)
      :value nil)
    (fail state "failed parsing double quote")))

(defn parse-whitespace [state]
  (log "parse-whitespace" state)
  (if (match-first state whitespace)
    (assoc state
      :code (subs (:code state) 1)
      :x (+ (:x state) 1)
      :value nil)
    (fail state "failed parsing whitespace")))

(defn parse-newline [state]
  (log "parse-newline" state)
  (if (match-first state line-break)
    (assoc state
      :code (subs (:code state) 1)
      :y (+ (:y state) 1)
      :x 1
      :value nil)
    (fail state "failed parsing newline")))

(defn parse-open-paren [state]
  (log "parse-open-paren" state)
  (if (match-first state open-paren)
    (assoc state
      :code (subs (:code state) 1)
      :y (+ (:y state) 1)
      :x 1
      :value nil)
    (fail state "failed parsing open paren")))

(defn parse-close-paren [state]
  (log "parse-close-paren" state)
  (if (match-first state close-paren)
    (assoc state
      :code (subs (:code state) 1)
      :y (+ (:y state) 1)
      :x 1
      :value nil)
    (fail state "failed parsing close paren")))

(defn parse-special-in-token [state]
  (log "parse-special-in-token" state)
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
  (log "parse-special-in-string" state)
  (let
    [code-first (subs (:code state) 0 1)
      code-rest (subs (:code state) 1)
      result (assoc state :code code-rest :value code-first)]
    (if
      (.contains specials-in-string code-first)
      result
      (fail result "found no special in string"))))

(defn parse-eof [state]
  (log "parse-eof" state)
  (if (= (:code state) "")
    (assoc state :value nil)
    (fail state "expected EOF")))

(defn parse-indent [state]
  (log "parse-indent" state)
  (let
    [result (parse-indentation state)]
    (println "debugging" (:value result))
    (if
      (> (:value result) (:indentation result))
      (assoc state
        :indentation (+ (:indentation result) 1))
      (fail result "no indent"))))

(defn parse-unindent [state]
  (log "parse-unindent" state)
  (let
    [result (parse-indentation state)]
    (if
      (< (:value result) (:indentation result))
      (assoc state
        :indentation (- (:indentation result) 1))
      (fail result "no unindent"))))

(defn parse-align [state]
  (log "parse-align" state)
  (let
    [result (parse-indentation state)]
    (if
      (= (:value result) (:indentation state))
      result
      (fail result "not aligned"))))

(defn parse-escaped-newline [state]
  (log "parse-escaped-newline" state)
  (if
    (= (subs (:code state) 0 2) "\\n")
    (assoc state :code (:subs (:code state) 2))
    (fail state "no escaped newline")))

(defn parse-escaped-tab [state]
  (log "parse-escaped-tab" state)
  (if
    (= (subs (:code state) 0 2) "\\t")
    (assoc state :code (:subs (:code state) 2))
    (fail state "no escaped tab")))

(defn parse-escaped-double-quote [state]
  (log "parse-escaped-double-quote" state)
  (if
    (= (subs (:code state) 0 2) "\\\"")
    (assoc state :code (:subs (:code state) 2))
    (fail state "no escaped double quote")))

(defn parse-escaped-backslash [state]
  (log "parse-escaped-backslash" state)
  (if
    (= (subs (:code state) 0 2) "\\\\")
    (assoc state :code (:subs (:code state) 2))
    (fail state "no escaped backslash")))

(defn parse-string-char [state]
  (log "parse-string-char" state)
  (if
    (> (count (:code state)) 1)
    ((combine-not parse-special-in-string) state)
    (fail state "no more char for string")))

(defn parse-token-char [state]
  (log "parse-token-char" state)
  (if
    (> (count (:code state)) 1)
    ((combine-not parse-special-in-token) state)
    (fail state "no more char for token")))

; generated parser

(defn parse-string [state]
  (log "parse-string" state)
  (let
    [method (combine-chain parse-quote
      (combine-many (combine-or parse-string-char parse-escaped))
      parse-quote)]
    (untabs (method (tabs state)))))

(defn parse-token-end [state]
  (log "parse-token-end" state)
  (let
    [method (combine-peek
      (combine-or parse-whitespace parse-close-paren parse-newline))]
    (untabs (method (tabs state)))))

(defn parse-escaped [state]
  (log "parse-escaped" state)
  (let
    [method (combine-or parse-escaped-newline parse-escaped-tab
      parse-escaped-double-quote
      parse-escaped-backslash)]
    (untabs (method (tabs state)))))

(defn parse-token [state]
  (log "parse-token" state)
  (let
    [method (combine-many parse-token-char)]
    (untabs (method (tabs state)))))

(defn parse-expression [state]
  (log "parse-expression" state)
  (let
    [method (combine-chain parse-open-paren
      (combine-alternate parse-whitespace parse-item)
      parse-close-paren)]
    (untabs (method (tabs state)))))

(defn parse-empty-line [state]
  (log "parse-empty-line" state)
  (let
    [method (combine-chain parse-newline
      (combine-star parse-whitespace)
      (combine-peek (combine-or parse-newline parse-eof)))]
    (untabs (method (tabs state)))))

(defn parse-line-eof [state]
  (log "parse-line-eof" state)
  (let
    [method (combine-chain
      (combine-star parse-whitespace)
      (combine-star parse-empty-line)
      parse-eof)]
    (untabs (method (tabs state)))))

(defn parse-two-blanks [state]
  (log "parse-two-blanks" state)
  (let
    [method (combine-value
      (combine-times parse-whitespace 2)
      (fn [value] 1))]
    (untabs (method (tabs state)))))

(defn parse-line-breaks [state]
  (log "parse-line-breaks" state)
  (let
    [method (combine-chain
      (combine-star parse-empty-line)
      parse-newline)]
    (untabs (method (tabs state)))))

(defn parse-item [state]
  (log "parse-item" state)
  (let
    [method (combine-or parse-token parse-string parse-expression)]
    (untabs (method (tabs state)))))

(defn parse-indentation [state]
  (log "parse-indentation" state)
  (let
    [method
      (combine-value
        (combine-chain
          (combine-value parse-line-breaks (fn [value] nil))
          (combine-value (combine-many parse-two-blanks)
            (fn [value] (count value))))
        (fn [value] (println "debug indentation" value) (first value)))]
    (untabs (method (tabs state)))))

(defn parse-inner-block [state]
  (log "parse-inner-block" state)
  (let
    [method (combine-chain parse-indent
      (combine-alternate parse-block parse-align)
      parse-unindent)]
    (untabs (method (tabs state)))))

(defn parse-block-line [state]
  (log "parse-block-line" state)
  (let
    [method (combine-chain
      (combine-alternate parse-item parse-whitespace)
      (combine-optional parse-inner-block))]
    (untabs (method (tabs state)))))

(defn parse-block [state]
  (log "parse-block" state)
  (let
    [method (combine-chain
      (combine-alternate parse-block-line parse-align))]
    (untabs (method (tabs state)))))

(defn parse-program [state]
  (log "parse-program" state)
  (let
    [method (combine-chain
      (combine-optional parse-line-breaks)
      (combine-alternate parse-block parse-align)
      parse-line-eof)]
    (untabs (method (tabs state)))))

; exposed methods

(defn parse [code]
  (let
    [initial {:code code
      :value nil :msg ""
      :indentation 0
      :x 1 :y 1
      :tab ""
      :failed false}]
    (parse-program initial)))
