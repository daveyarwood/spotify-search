(set-env!
  :dependencies '[[clj-spotify "0.1.7"]
                  ;; clj-spotify has at least one bad ns form, so can't use
                  ;; clojure 1.9
                  [org.clojure/clojure "1.8.0"]]
  :source-paths #{"src"})
