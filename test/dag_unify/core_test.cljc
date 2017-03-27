(ns dag_unify.core-test
  (:require #?(:clj [clojure.test :refer [deftest is]])
            #?(:cljs [cljs.test :refer-macros [deftest is]])
            [dag_unify.core :refer [all-refs annotate assoc-in by-rows
                                    create-path-in copy
                                    create-shared-values deserialize
                                    elements fail? find-paths-to-value
                                    get-in gather-annotations get-refs
                                    isomorphic? print-out
                                    recursive-dissoc ref? ref-skel-map
                                    remove-matching-keys serialize
                                    skeletize skels width height unify
                                    unify! width-of-column]])
  (:refer-clojure :exclude [assoc-in get-in resolve]))

;; TODO: add tests for (dag_unify.core/dissoc-paths)
;; TODO: add more tests for (dag_unify.core/isomorphic?)

(defn ser-db [input-map]
  (let [refs (get-refs input-map)
        skels (skels input-map refs)]
    (ref-skel-map input-map)))

(deftest simple-unify-test
  (let [result (unify! {:foo 99} {:bar 42})]
    (is (= (:foo result) 99))
    (is (= (:bar result) 42))))

(deftest unify-with-top
  (let [result (unify! :top {:foo 99} :top {:bar 42})]
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

(deftest unify-with-not
  (let [result (unify! {:foo 42} {:foo {:not 43}})]
    (is (= result {:foo 42}))))

(deftest unify-fail-with-not
  "test atom unifying with ':not' (special feature) (first in list; fail)"
  (let [result (unify! {:not 42} 42)]
    (is (= result :fail))))

(deftest unify-succeed-with-not
  "test atom unifying with ':not' (special feature) (second in list; succeed)"
  (let [result (unify! 42 {:not 43})]
    (is (= result 42))))

(deftest unify-fail-with-not-2
  "test atom unifying with ':not' (special feature) (second in list; fail)"
  (let [result (unify! 42 {:not 42})]
    (is (= result :fail))))

(deftest unify-nested-not
  "test unifying with ':not' (special feature)"
  (let [result (unify! {:foo 42} {:foo {:not 43}})]
    (is (= (get-in result [:foo]) 42))))

(deftest unify-with-not-and-top1
  "unifying {:not X} with :top should return {:not X} if X != top."
  (let [result (unify! {:not 42} :top)]
    (is (= result {:not 42}))))

(deftest unify-with-not-and-top2
  "(reversed argument order as preceding): unifying :top with {:not X} should return {:not X} if X != top."
  (let [result (unify! :top {:not 42})]
    (is (= result {:not 42}))))

(deftest unify-atomic-vals
  (let [result (unify! 5 5)]
    (is (= result 5))))

(deftest unify-atomic-vals-fail
  (let [result (unify! 5 4)]
    (is (= result :fail))))

(deftest maps-unify
  (let [result (unify! '{:a 42} '{:b 43})]
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

(deftest detect-unify-fail
  "test that, (fail? fs) <=> true if at least one of fs's path's value is :fail."
  (let [fs1 {:a 42}
        fs2 {:a :fail}]
    (is (= (fail? fs1) false))
    (is (= (fail? fs2) true))
    (is (= (fail? {:a (atom :fail)}) true))
    (is (= (fail? {:a (atom 42)}) false)))
    (is (= (fail? {:a (atom {:b :fail})}) true)))


;(deftest pathify-no-references
;  "a simple test of pathify with no structure-sharing."
;  (let [mymap {:a {:c 42}, :b {:c 42}, :c 42}
;        pathify (pathify mymap)]
;    (is (= pathify '((:a :c) 42 (:b :c) 42 (:c) 42)))))

(deftest find-paths-to-values-1
  "test path-to-value, which returns a list of all ways of reaching
a given value in a given map."
  (let [ref1 (atom 42)
        mymap {:a ref1 :b ref1}
        ptf (find-paths-to-value mymap ref1 nil)]
    (is (= ptf '((:a)(:b))))))

(deftest find-paths-to-values-2
  "test path-to-value, which returns a list of all ways of reaching
a given value in a given map."
  (let [ref2 (atom 42)
        ref1 (atom {:c ref2})
        mymap {:a ref1
               :b ref1
               :d ref2}
        paths-to-ref1 (find-paths-to-value mymap ref1 nil)]
    (is (= paths-to-ref1 '((:a)(:b))))))

(deftest find-paths-to-values-3
  "test path-to-value, which returns a list of all ways of reaching
a given value in a given map."
  (let [ref2 (atom 42)
        ref1 (atom {:c ref2})
        mymap {:a ref1
               :b ref1
               :d ref2}
        paths-to-ref2 (find-paths-to-value mymap ref2 nil)]
    (is (= paths-to-ref2 '((:a :c)(:b :c)(:d))))))

(deftest all-refs1
  (let [ref1 (atom 42)
        mymap {:a ref1, :b ref1}
        refs (seq (set (flatten (all-refs mymap))))]
    (is (= refs (list ref1)))))

(deftest all-refs2
  (let [ref1 (atom 42)
        ref2 (atom 43)
        mymap {:a ref1, :b ref2}
        refs (seq (set (flatten (all-refs mymap))))]
    (is (or (= refs (list ref1 ref2))
            (= refs (list ref2 ref1))))))

(deftest all-refs3
  (let [ref1 (atom 42)
        ref2 (atom 43)
        mymap {:a ref1 :b {:c ref2}}
        refs (seq (set (flatten (all-refs mymap))))]
    (is (or (= refs (list ref1 ref2))
            (= refs (list ref2 ref1))))))

(deftest all-refs4
  (let [ref1 (atom 42)
        mymap {:a ref1 :b {:c ref1}}
        refs (seq (set (flatten (all-refs mymap))))]
    (is (= refs (list ref1)))))

(deftest all-refs5
  (let [ref2 (atom 42)
        ref1 (atom {:c ref2})
        mymap {:a ref1 :b ref1 :d ref2}
        refs (seq (set (flatten (all-refs mymap))))]
    (is (or (= refs (list ref1 ref2))
            (= refs (list ref2 ref1))))))

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

(deftest ser-db-1
  (let [ref1 (atom 42)
        mymap {:a ref1, :b ref1}
        ser (ser-db mymap)]
    (is (= ser
           {
            {:ref ref1
             :skel 42} '((:a)(:b))}))))

;; TODO: this test is unnecessarily strict: see below for specifics
(deftest ser-db-2
  (let [ref2 (atom 42)
        ref1 (atom {:c ref2})
        mymap {:a ref1 :b ref1 :d ref2}
        ser (ser-db mymap)]
    (is (=
         ser
         {
          {:ref ref1
           ;; TODO: could also be '((:b)(:a)).
           :skel {:c :top}} '((:a)(:b))
          {:ref ref2
           ;; TODO: could also be '((:b :c)(:a :c)(:d))
           ;; (or other possible orderings).
           :skel 42} '((:a :c)(:b :c)(:d))
          }))))

(deftest serialize-1
  (let [ref1 (atom 42)
        mymap {:a ref1, :b ref1}
        ser (serialize mymap)]
    (is (= ser

           '((nil {:b :top :a :top})

             ;; TODO: could be '((:b)(:a))
             (((:a)(:b)) 42))))))

(deftest serialize-2
  (let [ref2 (atom 42)
        ref1 (atom {:c ref2})
        mymap {:a ref1, :b ref1 :d ref2}
        ser (serialize mymap)]
    (is (= ser

           '((nil {:d :top, :b :top, :a :top})

             ;; TODO: could be '((:b)(:a))
             (((:a) (:b)) {:c :top})

             ;; TODO: could be '((:b :c)(:a c)..etc
             (((:a :c) (:b :c) (:d)) 42))))))

(deftest serialize-3
  (let [mymap {:a 42 :b (atom 43)}]
    (is (not (nil? (serialize mymap))))))

(deftest serialize-4
  (let [ref3 (atom "avere")
        ref2 (atom {:italian "fatto"})
        ref1 (atom {:infl :infinitive
                   :italian ref3})
        vp {:a ref1
            :b {:italian ref2
                :root {:infl :infinitive
                       :pass-prossimo ref2
                       :pass-prossimo-aux ref1}}
            :italian {:a ref3
                      :b ref2}
            :infl :infinitive}
        serialized (serialize vp)
        ]
    (not (nil? vp))
    (not (nil? serialized))
    (= (count serialized) 4)))

(deftest create-shared-values-1
  (let [ref2 (atom 42)
        ref1 (atom {:c ref2})
        mymap {:a ref1, :b ref1 :d ref2}
        my-ser (serialize mymap)
        create-shared-vals (create-shared-values my-ser)
        are-refs? (map (fn [val]
                         (ref? val))
                      create-shared-vals)
        derefs (map (fn [val]
                      @val)
                    create-shared-vals)]
    (is (= (first derefs)
           {:d :top
            :b :top
            :a :top}))
    (is (= (second derefs)
           {:c :top}))

    (is (= (nth derefs 2)
           42))
    
    (is (= are-refs? (list true true true)))))

(deftest create-path-in-1
  (let [path '(:a :b :c :d :e)
        val 43
        result (create-path-in path val)]
    (is (= (get-in result path) val))))

(deftest deser-with-ref
  (let [serialized [[nil {:a "PH"}] [[["a"]] 42]]
        deserialized (deserialize serialized)]
    (is (not (nil? deserialized)))
    (is (= (ref? (:a deserialized))))
    (is (= @(:a deserialized) 42))))

;; deserialize a map's serialized form
(deftest deser-1
  (let [ref2 (atom 42)
        ref1 (atom {:c ref2})
        mymap {:a ref1, :b ref1 :d ref2}
        my-ser (serialize mymap)
        my-deser (deserialize my-ser)]
    (is (not (= my-ser nil)))
    (is (ref? (:a my-deser)))

    ;; a)
    (is (= (ref? (get @(get my-deser :a) :c))))
    (is (= (ref? (get @(get my-deser :b) :c))))
    (is (= (ref? (:d my-deser))))
    (is (= (:a my-deser) (:b my-deser)))
    (is (= (get @(get my-deser :a) :c)
           (get my-deser :d)))
    (is (= (get @(get my-deser :b) :c)
           (get my-deser :d)))
    (is (= @(get @(get my-deser :a) :c)
           42))

    ;; similar tests as a) above, but using fs/get-in
    (is (number? (get-in my-deser [:a :c])))
    (is (number? (get-in my-deser [:b :c])))
    (is (number? (get-in my-deser [:d])))
    (is (= (get-in my-deser [:a :c])
           (get-in my-deser [:d])))
    (is (= (get-in my-deser [:b :c])
           (get-in my-deser [:d])))
    (is (= (get-in my-deser [:a :c])
           42))))

(deftest deser-2
  (let [ref3 (atom "avere")
        ref2 (atom {:italian "fatto"})
        ref1 (atom {:infl :infinitive
                   :italian ref3})
        vp {:a ref1
            :b {:italian ref2
                :root {:infl :infinitive
                       :pass-prossimo ref2
                       :pass-prossimo-aux ref1}}
            :italian {:a ref3
                      :b ref2}
            :infl :infinitive}
        myser (serialize vp)
        ]
    (not (nil? vp))
    (not (nil? myser))))

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

(deftest copy-with-not
  (let [fs1 {:a (atom {:not 42})}
        fs1-copy (copy fs1)]
    (is (not (fail? fs1-copy)))))

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

(deftest isomorphic-true1
  (is (= true (isomorphic? {:a 42 :b 43 :c 44} {:a 42 :b 43 :c 44}))))

(deftest isomorphic-false1
  (is (= false (isomorphic? {:a 42 :b 43 :c 45} {:a 42 :b 43 :c 44}))))

(deftest isomorphic-false2
  (is (= false (isomorphic? {:a 42 :b 43} {:a 42 :b 43 :c 44}))))

(deftest isomorphic-false3
  (is (= false (isomorphic? {:a 42 :b 43 :c 44} {:a 42 :b 43}))))

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
                (unify! common
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
                       ))])]
    (is (= (count be) 2))
    (is (not (fail? (nth be 0))))
    (is (not (fail? (nth be 1))))))

(deftest recursive-dissoc-test
  (let [fs {:a 42 :b {:c 43}}
        result (recursive-dissoc fs
                                 (fn [k] (= k :c)))]
    (is (= (get-in result [:b]) {}))))

(deftest remove-matching-keys-test
  (let [fs
        (let [myref (atom 42)]
          {:a myref :b {:c myref} :d {:e 43}})
        removed
        (remove-matching-keys fs (fn [k]
                                     (= k :c)))]
    (is (map? removed))
    (is (= 42 (get-in removed [:a])))
    (is (= :none (get-in removed [:b :c] :none)))))

(deftest prevent-cyclic-graph-1
  (let [foo
        (let [shared (atom :top)]
          {:a shared
           :b shared})
        bar
        (let [shared (atom :top)]
          {:a shared
           :b {:c shared}})]
    (is (fail? (unify! foo bar)))))

(deftest prevent-cyclic-graph-2
  (is (fail? (unify
              (let [shared (atom :top)]
                {:synsem {:subcat {:2 {:sem {:obj shared}}}
                          :sem {:obj shared}}})
              (let [shared (atom :top)]
                {:synsem {:subcat {:2 {:sem shared}}
                          :sem {:obj shared}}})))))

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

(deftest print-out-test
    ;; fs2:
    ;;
    ;; :apricot [1]    :banana  42    
    ;;                 :coconut [2] 43
    ;; :date    :fig   [2]            
    ;;          :grape :mango   [1]   
    ;;
    (let [fs (let [two (atom 43)
                   one (atom {:banana 42
                              :coconut two})]
               {:apricot one
                :date {:fig two
                       :grape {:mango one}}})
          line-oriented (by-rows fs)]

      (is (= (nth line-oriented 0)
             ":apricot [1]    :banana  42"))

      (is (= (nth line-oriented 1)
             "                :coconut [2] 43"))

      (is (= (nth line-oriented 2)
             ":date    :fig   [2]"))

      (is (= (nth line-oriented 3)
             "         :grape :mango   [1]"))))
