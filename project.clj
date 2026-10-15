(defproject dag_unify "1.11.3"
  :description "Unification of Directed Acyclic Graphs"
  :url "https://github.com/ekoontz/dag-unify"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.12.3"]
                 [org.clojure/tools.logging "1.3.0"]]
  :profiles {:dev {:source-paths ["src" "bench"]}}
  :aliases {"bench" ["run" "-m" "dag_unify.benchmark"]})
