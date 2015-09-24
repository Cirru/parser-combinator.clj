(ns cirru.parser-combinator
  (:use clojure.pprint)
  (:require [clojure.string :as string]))

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
(declare parse-item)
(declare parse-expression)
(declare parse-block)
(declare parse-block-line)

; utils

(defn match-first [state character]
  (= (subs (:code state) 0 1) character))

(defn fail [state msg]
  (assoc state :failed true :msg msg))

(defn tabs [state]
  (assoc state
    :tab (str " ." (:tab state))
    :msg ""))

(defn untabs [state]
  (assoc state :tab (subs (:tab state) 2)))

(defn log [name state]
  (println (:tab state) name
    (with-out-str (write (:code state)))
    (with-out-str (write (:value state)))))

(defn log-nothing [name state] nil)

(defn wrap [label parser]
  (fn [state]
    (println (:tab (tabs state)) label
      (with-out-str (write (:code state)))
      (:indentation state)
      (:value state))
    (let
      [result (parser (tabs state))]
      (println (:tab result) label
        (with-out-str (write (:code result)))
        (:indentation state)
        (with-out-str (write (:value result)))
        (:failed result)
        (with-out-str (write (:msg result))))
      (untabs result))))

(defn just [label parser] parser)

; combinators

(defn helper-or [state parsers]
  (log-nothing "helper-or" state)
  (if (> (count parsers) 0)
    (let
      [parser (first parsers)
        result (parser state)]
      (if (:failed result)
        (recur state (rest parsers))
        result))
    (fail state "no parser is successful")))

(defn combine-or [& parsers]
  (wrap "combine-or"
    (fn [state]
      (helper-or state parsers))))

(defn helper-chain [state parsers]
  (log-nothing "helper-chain" state)
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
  (just "combine-chain"
    (fn [state]
      (helper-chain (assoc state :value []) parsers))))

(defn combine-times [parser n]
  (just "combine-times"
    (fn [state]
      (let
        [method (apply combine-chain (repeat n parser))]
        (method state)))))

(defn helper-many [state parser counter]
  (log-nothing "helper-many" state)
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
  (just "combine-many"
    (fn [state]
      (helper-many (assoc state :value (list)) parser 0))))

(defn combine-not [parser]
  (wrap "combine-not"
    (fn [state]
      (let
        [result (parser state)]
        (if (:failed result)
          (assoc result :failed false :msg "recovered in not")
          (fail result "should not be this"))))))

(defn combine-optional [parser]
  (just "combine-optional"
    (fn [state]
      (let
        [inner-state (assoc state :value nil)
          result (parser inner-state)]
        (if (:failed result) inner-state result)))))

(defn combine-star [parser]
  (wrap "combine-star"
    (fn [state]
      (let
        [method (combine-optional (combine-many parser))]
        (method state)))))

(defn combine-peek [parser]
  (wrap "combine-peek"
    (fn [state]
      (let
        [result (parser state)]
        (if (:failed result)
          (fail state "peek failed")
          state)))))

(defn combine-value [parser handler]
  (just "combine-value"
    (fn [state]
      (let
        [result (parser state)]
        (assoc result :value
          (handler (:value result) (:failed result)))))))

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
  (wrap "combine-alternate"
    (fn [state]
      (helper-alternate
        (assoc state :value (list)) parser-1 parser-2 0))))

; parsers

(def parse-quote
  (just "parse-quote"
    (fn [state]
      (if (> (count (:code state)) 0)
        (if (match-first state double-quote)
          (assoc state
            :code (subs (:code state) 1)
            :x (+ (:x state) 1)
            :value nil)
          (fail state "failed parsing double quote"))
        (fail state "no enough code")))))

(def parse-whitespace
  (just "parse-whitespace"
    (fn [state]
      (if (> (count (:code state)) 0)
        (if (match-first state whitespace)
          (assoc state
            :code (subs (:code state) 1)
            :x (+ (:x state) 1)
            :value nil)
          (fail state "failed parsing whitespace"))
        (fail state "not long enough")))))

(def parse-newline
  (just "parse-newline"
    (fn [state]
      (if (> (count (:code state)) 0)
        (if (match-first state line-break)
          (assoc state
            :code (subs (:code state) 1)
            :y (+ (:y state) 1)
            :x 1
            :value nil)
          (fail state "failed parsing newline"))
        (fail state "not long enough")))))

(def parse-open-paren
  (just "parse-open-paren"
    (fn [state]
      (if (> (count (:code state)) 0)
        (if (match-first state open-paren)
          (assoc state
            :code (subs (:code state) 1)
            :y (+ (:y state) 1)
            :x 1
            :value nil)
          (fail state "failed parsing open paren"))
        (fail state "no enough code")))))

(def parse-close-paren
  (just "parse-close-paren"
    (fn [state]
      (if (> (count (:code state)) 0)
        (if (match-first state close-paren)
          (assoc state
            :code (subs (:code state) 1)
            :y (+ (:y state) 1)
            :x 1
            :value nil)
          (fail state "failed parsing close paren"))
        (fail state "not long enough")))))

(def parse-special-in-token
  (just "parse-special-in-token"
    (fn [state]
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
            (fail result "found no special in token")))))))

(def parse-special-in-string
  (just "parse-special-in-string"
    (fn [state]
      (let
        [code-first (subs (:code state) 0 1)
          code-rest (subs (:code state) 1)
          result (assoc state :code code-rest :value code-first)]
        (if
          (.contains specials-in-string code-first)
          result
          (fail result "found no special in string"))))))

(def parse-eof
  (wrap "parse-eof"
    (fn [state]
      (if (= (:code state) "")
        (assoc state :value nil)
        (fail state "expected EOF")))))

(def parse-indent
  (wrap "parse-indent"
    (fn [state]
      (let
        [result (parse-indentation state)]
        (if
          (> (:value result) (:indentation result))
          (assoc state
            :indentation (+ (:indentation result) 1)
            :value nil)
          (fail result "no indent"))))))

(def parse-unindent
  (wrap "parse-unindent"
    (fn [state]
      (let
        [result (parse-indentation state)]
        (if
          (< (:value result) (:indentation result))
          (assoc state
            :indentation (- (:indentation result) 1)
            :value nil)
          (fail result "no unindent"))))))

(def parse-align
  (wrap "parse-align"
    (fn [state]
      (let
        [result (parse-indentation state)]
        (if
          (= (:value result) (:indentation state))
          (assoc result :value nil)
          (fail result "not aligned"))))))

(def parse-escaped-newline
  (just "parse-escaped-newline"
    (fn [state]
      (if (> (count (:code state)) 1)
        (if
          (= (subs (:code state) 0 2) "\\n")
          (assoc state
            :code (subs (:code state) 2)
            :value "\n")
          (fail state "no escaped newline"))
        (fail state "no enough code")))))

(def parse-escaped-tab
  (just "parse-escaped-tab"
    (fn [state]
      (if (> (count (:code state)) 1)
        (if
          (= (subs (:code state) 0 2) "\\t")
          (assoc state
            :code (subs (:code state) 2)
            :value "\t")
          (fail state "no escaped tab"))
        (fail state "no enough code")))))

(def parse-escaped-double-quote
  (just "parse-escaped-double-quote"
    (fn [state]
      (if (> (count (:code state)) 1)
        (if
          (= (subs (:code state) 0 2) "\\\"")
          (assoc state
            :code (subs (:code state) 2)
            :value "\"")
          (fail state "no escaped double quote"))
        (fail state "no enough code")))))

(def parse-escaped-backslash
  (just "parse-escaped-backslash"
    (fn [state]
      (if (> (count (:code state)) 1)
        (if
          (= (subs (:code state) 0 2) "\\\\")
          (assoc state
            :code (subs (:code state) 2)
            :value "\\")
          (fail state "no escaped backslash"))
        (fail state "no enough code")))))

(def parse-string-char
  (just "parse-string-char"
    (fn [state]
      (if
        (> (count (:code state)) 0)
        ((combine-not parse-special-in-string) state)
        (fail state "no more char for string")))))

(def parse-token-char
  (just "parse-token-char"
    (fn [state]
      (if
        (> (count (:code state)) 0)
        ((combine-not parse-special-in-token) state)
        (fail state "no more char for token")))))

; generated parser

(def parse-token-end
  (just "parse-token-end"
    (combine-value
      (combine-peek
        (combine-or parse-whitespace parse-close-paren parse-newline parse-eof))
      (fn [value is-failed] nil))))

(def parse-escaped
  (just "parse-escaped"
    (combine-or parse-escaped-newline parse-escaped-tab
      parse-escaped-double-quote
      parse-escaped-backslash)))

(def parse-string
  (wrap "parse-string"
    (combine-value
      (combine-chain parse-quote
        (combine-star (combine-or parse-string-char parse-escaped))
        parse-quote)
      (fn [value is-failed]
        (if is-failed nil
          (string/join "" (nth value 1)))))))

(def parse-token
  (just "parse-token"
    (combine-value
      (combine-chain
        (combine-many parse-token-char)
        parse-token-end)
      (fn [value is-failed] (string/join "" (first value))))))

(def parse-item
  (just "parse-item"
    (combine-or parse-token parse-string parse-expression)))

(def parse-expression
  (just "parse-expression"
    (combine-value
      (combine-chain parse-open-paren
        (combine-optional (combine-alternate parse-item parse-whitespace))
        parse-close-paren)
      (fn [value is-failed]
        (if is-failed nil
          (filter some? (nth value 1)))))))

(def parse-empty-line
  (wrap "parse-empty-line"
    (combine-chain parse-newline
      (combine-star parse-whitespace)
      (combine-peek (combine-or parse-newline parse-eof)))))

(def parse-line-eof
  (wrap "parse-line-eof"
    (combine-chain
      (combine-star parse-whitespace)
      (combine-star parse-empty-line)
      parse-eof)))

(def parse-two-blanks
  (wrap "parse-two-blanks"
    (combine-value
      (combine-times parse-whitespace 2)
      (fn [value is-failed] 1))))

(def parse-line-breaks
  (wrap "parse-line-breaks"
    (combine-value
      (combine-chain
        (combine-star parse-empty-line)
        parse-newline)
      (fn [value is-failed] nil))))

(def parse-indentation
  (just "parse-indentation"
    (combine-value
      (combine-chain
        (combine-value parse-line-breaks (fn [value is-failed] nil))
        (combine-value (combine-star parse-two-blanks)
          (fn [value is-failed] (count value))))
      (fn [value is-failed]
        (if is-failed 0 (last value))))))

(def parse-inner-block
  (wrap "parse-inner-block"
    (combine-value
      (combine-chain parse-indent
        (combine-value
          (combine-optional parse-indentation)
          (fn [value is-failed] nil))
        (combine-alternate parse-block-line parse-align)
        parse-unindent)
      (fn [value is-failed]
        (if is-failed nil
          (filter some? (nth value 2)))))))

(def parse-block-line
  (wrap "parse-block-line"
    (combine-value
      (combine-chain
        (combine-alternate parse-item parse-whitespace)
        (combine-optional parse-inner-block))
      (fn [value is-failed]
        (let
          [main (into [] (filter some? (first value)))
            nested (into [] (last value))]
          (if (some? nested)
            (concat main nested)
            main))))))

(def parse-block
  (wrap "parse-block"
    (combine-value
      (combine-alternate parse-block-line parse-align)
      (fn [value is-failed]
        (if is-failed nil
          (filter some? value))))))

(def parse-program
  (wrap "parse-program"
    (combine-chain
      (combine-optional parse-line-breaks)
      (combine-alternate parse-block parse-align)
      parse-line-eof)))

; exposed methods

(def initial-state {
  :code ""
  :indentation 0
  :x 0 :y 0
  :tab ""
  :failed false
  })

(defn parse [code]
  (parse-program (assoc initial-state :code code)))

(defn try [code]
  (parse-token (assoc initial-state :code code)))
