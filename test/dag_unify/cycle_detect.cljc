(ns dag_unify.cycle-detect
  (:require #?(:clj [clojure.test :refer [deftest is]])
            #?(:cljs [cljs.test :refer-macros [deftest is]])
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])
            [clojure.string :as string]
            [dag_unify.core :as u
             :refer [assoc-in copy fail? get-in unify unify!]]
            [dag_unify.diagnostics :refer [isomorphic?]]
            [dag_unify.serialization
             :refer [create-path-in deserialize final-reference-of serialize
                     skeletize ref?]])
  (:refer-clojure :exclude [assoc-in get-in]))

(def foo (deserialize [[[] {:a {:b :top}
                            :b {:c :top}}]
                       [[[:b :c]
                         [:a :b]] {:c {:d :top}}]]))

(def bar (deserialize [[[] {:a {:b :top}
                            :b :top}]

                       [[[:a :b]
                         [:b]] :top]]))



(def shared1 (atom {:c :top}))
(def shared2 (atom {:c shared1}))

(def foo2 {:b shared1})
(def bar2 {:b shared2})


(deftest cycle-test
  (is (= 5 6)))
