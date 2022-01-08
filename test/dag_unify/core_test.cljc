(ns dag_unify.core-test
  (:require #?(:clj [clojure.test :refer [deftest is]])
            #?(:cljs [cljs.test :refer-macros [deftest is]])
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])
            [clojure.string :as string]
            [dag_unify.core :as u
             :refer [assoc-in copy fail? get-in unify unify!]]
            [dag_unify.diagnostics :refer [fail-path isomorphic?]]
            [dag_unify.serialization
             :refer [create-path-in deserialize final-reference-of serialize
                     skeletize ref?]])
  (:refer-clojure :exclude [assoc-in get-in]))

(deftest simple-unify-test
  (let [result (unify! {:foo 99} {:bar 42})]
    (is (= (:foo result) 99))
    (is (= (:bar result) 42))))

(deftest unify-with-top
  (let [result (reduce unify! [:top {:foo 99} :top {:bar 42}])]
    (is (= (:foo result) 99))
    (is (= (:bar result) 42))))

(deftest unify-unequal-atomic-values
  "Testing that unify(v1,v2)=fail if v1 != v2."
  (let [result (unify! {:foo 42} {:foo 43})]
    (is (fail? result))))

(deftest unify-atomic-values-with-references
  (let [myref (atom :top)
        val 42
        result (unify! myref val)]
    (is (ref? result))
    (is (= @result 42))))

(deftest merging-with-inner-reference-keyset
  (let [fs1 {:b (atom :top)}
        fs2 {:b 42}
        maps (list fs1 fs2)
        result (seq (set (mapcat #'keys maps)))] ;; mapcat->set->seq removes duplicates.
    (is (= result '(:b)))))

(deftest unify-atomic-vals
  (let [result (unify! 5 5)]
    (is (= result 5))))

(deftest unify-atomic-vals-fail
  (let [result (unify! 5 4)]
    (is (= result :fail))))

(deftest maps-unify
  (let [result (unify! {:a 42} {:b 43})]
    (is (= (:a result) 42)
        (= (:b result) 43))))

(deftest unify-override
  (let [result (unify! '{:a 42} '{:a 43})]
    (is (fail? result))))

(deftest map-with-reference
  (let [fs1 {:a (atom 42)}
        fs2 {:b (get fs1 :a)}
        result (unify! fs1 fs2)]
    (is (ref? (:a result)))
    (is (ref? (:b result)))
    (is (= (:a result) (:b result)))))

(deftest unify-two-maps-one-with-references
  "unifying two maps, one with references."
  (let [fs1 {:a (atom :top)}
        fs2 {:b (get fs1 :a)}
        fs3 (unify! fs1 fs2)
        fs4 {:a 42}
        result (unify! fs3 fs4)]
    (is (ref? (:a result)))
    (is (ref? (:b result)))
    (is (= (:a result) (:b result)))
    (is (= @(:a result) 42))))

(deftest unify-two-maps-with-references
  "unifying two maps, both with references, same features"
  (let [fs1 {:a (atom 42)}
        fs2 {:b (get fs1 :a)}
        fs3 (unify! fs1 fs2)
        fs4 {:a (atom 42)}
        fs5 {:b (get fs4 :a)}
        fs6 (unify! fs4 fs5)
        result (unify! fs3 fs6)]
    (is (ref? (:a result)))
    (is (ref? (:b result)))
    (is (= (:a result) (:b result)))
    (is (= @(:a result) 42))))

(deftest skeletize-1
  (let [mymap {:a 42}]
    (is (= (skeletize mymap) mymap))))

(deftest skeletize-2
  (let [ref1 (atom 42)
        mymap {:a 42 :b ref1}]
    (is (= (skeletize mymap) {:a 42 :b :top}))))

(deftest skeletize-3
  (let [ref1 (atom 42)
        ref2 (atom 43)
        mymap {:a ref1 :b ref2}]
    (is (= (skeletize mymap) {:a :top :b :top}))))

                                        ;(if false (deftest pathify-one-atomic-reference
                                        ;  "a map with one atom (42) shared"
                                        ;  (let [ref1 (atom 42)
                                        ;        mymap {:a ref1 :b ref1}
                                        ;        pathify (pathify mymap)]
                                        ;    (is (= pathify '((:a) 42 (:b) 42))))))

;;      (deftest
;;       "unifying two maps, both with references, overlapping features"
;;       (let [fs1 {:a (atom 42)}
;;             fs2 {:b (get fs1 :a)}
;;             fs3 (unify fs1 fs2)
;;             fs4 {:b (atom 42)}
;;             fs5 {:c (get fs4 :b)}
;;             fs6 (unify fs4 fs5)]
;;         (unify fs3 fs6))
;;       (fn [result]
;;         (and (ref? (:a result))
;;              (ref? (:b result))
;;              (ref? (:c result))
;;              (= (:a result) (:b result))
;;              (= (:b result) (:c result))
;;              (= @(:a result) 42))))

(deftest nil-and-top
  ;; ...should return emptylist.
  (is (= nil
         (unify! nil :top))))

(deftest nil-and-anything-except-top
  ;; ...should return :fail.
  (is (fail?
       (unify! nil {:foo 42}))))

(deftest emptylist-and-top
  ;; ...should return emptylist.
  (is (= '()
         (unify! '() :top))))

(deftest emptylist-and-anything-except-top
  ;; ...should return :fail.
  (is (fail?
       (unify! '() {:foo 42}))))

(deftest unify-with-string3
  (let [arg1 {:italiano "gatto"}
        arg2 {:italiano "gatto"}
        result (unify! arg1 arg2)]
    (is (not (fail? result)))
    (is (= (get-in result [:italiano])
           "gatto"))))

(deftest unify-with-string4
  (let [arg1 {:italiano "gatto"}
        arg2 {:italiano "cane"}
        result (unify! arg1 arg2)]
    (is (fail? result))))

(deftest big-example
  (let [be (let [number-agr (atom :top)
                 common {:synsem {:cat :verb}
                         :english {:present {:1sing "am"
                                             :2sing "are"
                                             :3sing "is"
                                             :1plur "are"
                                             :2plur "are"
                                             :3plur "are"}
                                   :past {:1sing "was"
                                          :2sing "were"
                                          :3sing "was"
                                          :1plur "were"
                                          :2plur "were"
                                          :3plur "were"}}}]
             [;; intransitive
              (unify! common
                      {:synsem {:subcat {:1 {:cat :noun}
                                         :2 '()}
                                :sem {:pred :be}}})
              
              ;; be + propernoun, e.g. "I am John"
              (let [subject-verb-agreement
                    (let [infl (atom :top)
                          agr (atom :top)]
                      {:english {:agr agr
                                 :infl infl}
                       :synsem {:infl infl
                                :subcat {:1 {:agr agr}}}})

                    subj-agr (atom :top)
                    infl (atom :top)
                    the-real-subj (atom :top)
                    the-obj (atom :top)]
                (reduce
                 unify!
                 [common
                  subject-verb-agreement
                  {:intransitivize false
                   :transitivize false
                   :synsem {:agr subj-agr
                            :sem {:aspect :progressive
                                  :pred :be-called
                                  :tense :present
                                  :subj the-real-subj
                                  :obj the-obj}
                            :subcat {:1 {:cat :noun
                                         :agr subj-agr
                                         :sem {:pred :name
                                               :subj the-real-subj}
                                         
                                         }
                                     :2 {:cat :noun
                                         :agr subj-agr
                                         :sem the-obj
                                         :propernoun true ;; "I am John"
                                         }
                                     } ;; subcat {
                            } ;; synsem {
                   } ;; end of map
                  ]))])]
    (is (= (count be) 2))
    (is (not (fail? (nth be 0))))
    (is (not (fail? (nth be 1))))))

(deftest prevent-cyclic-graph-1
  (let [foo
        (let [shared (atom :top)]
          {:a shared
           :b shared})
        bar
        (let [shared (atom :top)]
          {:a shared
           :b {:c shared}})]
    (let [result
          (binding [u/exception-if-cycle? true]
            (try (unify! foo bar)
                 (catch Exception e :an-exception-was-thrown)))]
      (is (= :an-exception-was-thrown result)))))

(deftest prevent-cyclic-graph-2
  (let [result
        (binding [u/exception-if-cycle? true]
          (try
            (unify
             (let [shared (atom :top)]
               {:synsem {:subcat {:2 {:sem {:obj shared}}}
                         :sem {:obj shared}}})
             (let [shared (atom :top)]
               {:synsem {:subcat {:2 {:sem shared}}
                         :sem {:obj shared}}}))
            (catch Exception e :an-exception-was-thrown)))]
    (is (= :an-exception-was-thrown result))))

(deftest prevent-cyclic-graph-3
  (let [arg1
        (deserialize
         [[nil {}]
          ['((:sem :obj)
             (:subcat :2 :sem))
           :top]])
        arg2
        (deserialize
         [[nil {}]
          ['((:sem)
             (:subcat :2 :sem)) :top]])
        result
        (binding [u/exception-if-cycle? true]
          (try (unify! arg1 arg2)
               (catch Exception e :an-exception-was-thrown)))]
    (is (= :an-exception-was-thrown result))))

(deftest prevent-cyclic-graph-4
  (let [arg1 (deserialize [[[] {:a {:b :top}
                                :b {:c :top}}]
                           [[[:b :c]
                             [:a :b]] {:c {:d :top}}]])
        arg2 (deserialize [[[] {:a {:b :top}
                                :b :top}]
                           [[[:a :b]
                             [:b]] :top]])]
    (is (= :fail (unify arg1 arg2)))))

(deftest not-found-with-non-existent-path-with-nil
  (is (= ::notfound
         (get-in
          {:a {:b nil}}
          [:a :b :c :d] ::notfound))))

(deftest not-found-with-non-existent-path-with-empty
  (is (= ::notfound
         (get-in
          {:a {:b '()}}
          [:a :b :c :d] ::notfound))))

;; assoc-in-1 and assoc-in-2 test that both:
;;
;; (assoc-in {:a {:b 42}} [:a :c] 43)
;; and
;; (assoc-in {:a {:b 42}} [:a] {:c 43})
;; end up with the same result:
;; {:a {:b 42, :c 43}}

(deftest assoc-in-1
  (let [result (assoc-in {:a {:b 42}} [:a :c] 43)]
    (is (= (get-in result [:a :b]) 42))
    (is (= (get-in result [:a :c]) 43))))

(deftest assoc-in-2
  (let [result (assoc-in {:a {:b 42}} [:a] {:c 43})]
    (is (= (get-in result [:a :b]) 42))
    (is (= (get-in result [:a :c]) 43))))

(deftest complicated-sharing
  (let [a (let [r1 (atom :top)
                r2 (atom :top)]
            {:a r1 :b r2 :c r1})
        b (let [r1 (atom :top)]
            {:a r1 :b r1 :c r1})]
    (is (not (fail? (unify a b))))))

(deftest unify-empty-list
  (let [a '()
        b (atom :top)
        result1 (unify a b)
        result2 (unify b a)]
    (is (not (fail? result1)))
    (is (not (fail? result2)))
    (is (= true (ref? result1)))
    (is (= true (ref? result2)))
    (is (empty? @result1))
    (is (empty? @result2))))

;; for below test, arg1 looks like:
(comment
  {:mod [1]
   :sem {:subj {:mod []}
         :mod [[1] :top]}})
;; 
;; and arg2 looks like:
(comment
  {:mod {:first {:subj [[1] :top]}},
   :sem {:subj {:ref [1]},
         :mod []}})

;; we are testing for fail-path to return:
(comment
  {:fail :fail
   :type :ref
   :path (:sem :mod)
   :arg1 [[[] {:tag 1
               :subj {:tag 1}}]]
   :arg2 [[[] []]]})

(deftest diagnostics
  (let [arg1s [[[]
                {:mod
                 {:subj {:tag 1}}
                 :sem
                 {:mod {:tag 1}}}]
               [[[:mod]
                 [:sem :mod]] {:tag 1}]]
        arg2s [[[]
                {:mod
                 {:subj {:tag 1}}
                 :sem
                 {:mod []
                  :subj
                  {:ref {:tag 1}}}}]
               [[[:mod :subj]
                 [:sem :subj :ref]] {:tag 1}]]
        
        arg1 (dag_unify.serialization/deserialize arg1s)
        arg2 (dag_unify.serialization/deserialize arg2s)]
    (is (= (unify arg1 arg2)
           :fail))))

(deftest subsumes-test
  (is (= true (u/subsumes? {:a 42} {:a 42 :b 43})))
  (is (= false (u/subsumes? {:a 42 :b 43} {:a 42}))))


