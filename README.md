
# Cirru parser-combinator

Cirru Parser learning from Parser Combinator.

[![Clojars Project](http://clojars.org/cirru/parser-combinator/latest-version.svg)](http://clojars.org/cirru/parser-combinator)

## Usage

```clj
[cirru/parser-combinator "0.0.1"]
```

```clj
(require '[cirru.parser-combinator :as p])

(def result (p/pare "code"))
(if (:failed result)
  nil
  (println (:value result)))
```

## License

Copyright © 2015 jiyinyiyong

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
