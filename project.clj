(defproject dag_unify "1.6.3"
  :description "Unification of Directed Acyclic Graphs"
  :url "https://github.com/ekoontz/dag-unify"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]]
  :plugins [[cider/cider-nrepl "0.11.0"]]

  ;; run clojure tests with "lein test"
  ;; deploy with "lein deploy s3"
  :repositories {"s3" {:url "s3p://ekoontz-repo/releases/"
                       :username :env/aws_access_key ;; gets environment variable AWS_ACCESS_KEY
                       :passphrase :env/aws_secret_key}} ;; gets environment variable AWS_SECRET_KEY
)
