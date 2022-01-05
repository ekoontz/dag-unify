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

(def foo (deserialize [[[] {:cat :verb,
                            :subcat {:1 {:cat :noun :sem :top}, :2 []},
                            :sem {:subj :top},
                            :reflexive? true}]
                       [[[:sem :subj] [:subcat :1 :sem]] {:ref :top, :subj {:ref :top}}]
                       [[[:subcat :1 :sem :subj :ref] [:sem :subj :subj :ref]
                         [:subcat :1 :sem :ref] [:sem :subj :ref]]
                        #:menard.reflexives{:is-subj true}]]))

(def bar (deserialize [[[] {:cat :neg
                            :inflected? true, :curriculum :menard.nederlands/none,
                            :subcat {:1 {:cat :noun, :sem :top}},
                            :sem :top}]
                       [[[:subcat :1 :sem] [:sem]] :top]]))

