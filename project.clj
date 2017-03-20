(defproject dag_unify "1.5.0-SNAPSHOT"
  :description "Unification of Directed Acyclic Graphs"
  :url "https://github.com/ekoontz/dag-unify"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.494"]]
  :plugins [[cider/cider-nrepl "0.11.0"]
            [lein-doo "0.1.7"]
            [lein-cljsbuild "1.1.5"]
            [s3-wagon-private "1.2.0"]]

  ;; run clojure tests with "lein test"
  ;; run clojurescript tests with "lein doo phantom test once"
  :cljsbuild {:builds [{:id "test"
                        :source-paths ["src" "test"]
                        :compiler {:output-to "out/testable.js"
                                   ;; you must have {:optimizations :whitespace}
                                   ;; to avoid "ReferenceError: Can't find variable: goog"
                                   :optimizations :whitespace}}]}
  ;; deploy with "lein deploy s3"
  :repositories {"s3" {:url "s3p://ekoontz-repo/releases/"
                       :username :env/aws_access_key ;; gets environment variable AWS_ACCESS_KEY
                       :passphrase :env/aws_secret_key}} ;; gets environment variable AWS_SECRET_KEY

  ;; lein doo phantom test once
;  :doo {:paths {:phantom "phantomjs --web-security=false"
;                :slimer "slimerjs --ignore-ssl-errors=true"
;                :karma "karma --port=9881 --no-colors"
;                :rhino "rhino -strict"
;                :node "node --trace-gc --trace-gc-verbose"}}
  ;; commenting out :hooks below because
  ;; on TravisCI:
  ;; ======================================================================
  ;; Testing with Phantom:
  ;; TypeError: 'undefined' is not an object (evaluating 'dag_unify.runner.doo_tests.call')
  ;; Subprocess failed
  ;; The command "lein test; lein doo phantom test once" exited with 1.
  ;;:hooks [leiningen.cljsbuild]
  )
