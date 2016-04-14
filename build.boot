
(set-env!
 :asset-paths #{"assets"}
 :source-paths #{"test"}
 :resource-paths #{"src"}

 :dev-dependencies '[]
 :dependencies '[[adzerk/boot-cljs          "1.7.170-3"   :scope "provided"]
                 [adzerk/boot-reload        "0.4.6"       :scope "provided"]
                 [mvc-works/boot-html-entry "0.1.1"       :scope "provided"]
                 [cirru/boot-cirru-sepal    "0.1.1"       :scope "provided"]
                 [cheshire                  "5.5.0"       :scope "test"]
                 [org.clojure/clojure       "1.8.0"       :scope "test"]
                 [org.clojure/clojurescript "1.7.228"     :scope "test"]
                 [binaryage/devtools        "0.5.2"       :scope "test"]
                 [adzerk/boot-test          "1.1.1"       :scope "test"]])

(require '[adzerk.boot-test :refer :all])

(def +version+ "0.0.7")

(task-options!
  pom {:project     'cirru/parser-combinator
       :version     +version+
       :description "Cirru Parser learning from Parser Combinator"
       :url         "https://github.com/Cirru/parser-combinator.clj"
       :scm         {:url "https://github.com/Cirru/parser-combinator.clj"}
       :license     {"MIT" "http://opensource.org/licenses/mit-license.php"}})

(set-env! :repositories #(conj % ["clojars" {:url "https://clojars.org/repo/"}]))

(deftask build []
  (comp
   (pom)
   (jar)
   (install)
   (target)))

(deftask deploy []
  (comp
   (build)
   (push :repo "clojars" :gpg-sign (not (.endsWith +version+ "-SNAPSHOT")))))
