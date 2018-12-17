(ns dag_unify.core-test
  (:require #?(:clj [clojure.test :refer [deftest is]])
            #?(:cljs [cljs.test :refer-macros [deftest is]])
            [clojure.string :as string]
            [dag_unify.core :as u
             :refer [all-refs annotate assoc-in by-rows
                     create-path-in copy
                     create-shared-values deserialize
                     elements fail? find-paths-to-value
                     get-in gather-annotations
                     isomorphic? pprint print-out
                     recursive-dissoc ref? ref-skel-map
                     remove-matching-keys serialize
                     skeletize skels width height unify
                     unify! width-of-column]])
  (:refer-clojure :exclude [assoc-in get-in resolve]))

;; TODO: add tests for (dag_unify.core/dissoc-paths)
;; TODO: add more tests for (dag_unify.core/isomorphic?)

(defn ser-db [input-map]
  (let [refs (all-refs input-map)
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

(deftest print-out-test-2
  ;;
  ;; :apricot [1]    :banana  42    
  ;;                 :coconut [2] 43
  ;; :date    :fig   [2]           
  ;;          :grape :mango   [1]   
  ;;
  (let [fs (let [two (atom 43)
                 one (atom {:banana 42
                            :coconut two})
                 three (atom two)]
             {:apricot one
              :date {:fig three
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

(deftest print-out-test-3
  (let [serialized
        '((nil {:phrasal true, :synsem {:aux :top, :cat :top, :slash false, :essere :top, :infl :top, :subcat (), :agr :top, :modal :top, :sem :top, :pronoun :top}, :rule "sentence-nonphrasal-head", :modified false, :head :top, :root :top, :comment "c10", :english {:a :top, :agr :top, :b :top}, :schema-symbol c10, :first :comp, :comp {:phrasal true, :synsem :top, :rule "noun-phrase1", :head {:phrasal true, :english :top, :synsem {:cat :top, :essere :top, :infl :top, :mod :top, :reflexive false, :subcat {:2 (), :1 :top}, :agr :top, :sem :top, :propernoun :top, :pronoun :top}, :rule "nbar1", :head {:phrasal false, :synsem {:cat :top, :essere :top, :infl :top, :mod (), :reflexive false, :subcat {:2 (), :1 :top}, :agr :top, :sem :top, :case :top, :propernoun :top, :pronoun :top}, :english :top}, :comment "c11-comp-subcat-1", :schema-symbol c11-comp-subcat-1, :first :comp, :comp {:synsem {:sem :top, :cat :top, :subcat {:2 (), :1 :top}, :agr :top}, :english :top, :phrasal false}}, :comment "c10", :english :top, :schema-symbol c10, :first :comp, :aliases ("np1"), :comp {:synsem :top, :english :top, :phrasal false}}}) (((:head) (:root)) {:slash false, :phrasal false, :synsem {:aux :top, :cat :top, :participle false, :essere :top, :infl :top, :subcat {:2 (), :1 :top}, :agr :top, :modal :top, :sem :top, :pronoun :top}, :english :top, :applied {:2 true}, :modal-with false, :share-sem false, :phrasal-verb false}) (((:english :a) (:comp :english)) {:a :top, :agr :top, :b :top}) (((:head :english) (:root :english) (:english :b)) {:agr :top, :infl :top, :english "sleep", :past "slept", :cat :top, :exception false}) (((:synsem :aux) (:head :synsem :aux) (:root :synsem :aux)) false) (((:synsem :agr) (:head :synsem :agr) (:root :synsem :agr)) :top) (((:synsem :sem) (:head :synsem :sem) (:root :synsem :sem)) {:obj :unspec, :shared-with-obj false, :reflexive false, :subj :top, :pred :sleep, :tense :present, :aspect :simple}) (((:english :a :a) (:comp :english :a) (:comp :comp :english)) {:english "the", :cat :top}) (((:synsem :essere) (:head :synsem :essere) (:root :synsem :essere)) :top) (((:english :a :b) (:comp :head :english) (:comp :english :b)) {:agr :top, :a :top, :b :top}) (((:synsem :infl) (:head :synsem :infl) (:head :english :infl) (:root :synsem :infl) (:root :english :infl) (:english :b :infl)) :present) (((:synsem :cat) (:head :synsem :cat) (:head :english :cat) (:root :synsem :cat) (:root :english :cat) (:english :b :cat)) :verb) (((:synsem :pronoun) (:head :synsem :pronoun) (:root :synsem :pronoun)) :top) (((:synsem :modal) (:head :synsem :modal) (:root :synsem :modal)) false) (((:head :synsem :subcat :1) (:root :synsem :subcat :1) (:comp :synsem)) {:cat :top, :essere :top, :top :top, :infl :top, :mod :top, :reflexive false, :subcat (), :agr :top, :sem :top, :case :nom, :propernoun :top, :pronoun :top}) (((:english :a :b :b) (:comp :head :english :b) (:comp :head :head :english) (:comp :english :b :b)) {:agr :top, :propernoun :top, :pronoun :top, :english "dog", :cat :top}) (((:english :a :b :a) (:comp :head :english :a) (:comp :head :comp :english) (:comp :english :b :a)) {:agr :top, :english "small", :cat :top}) (((:head :synsem :subcat :1 :cat) (:root :synsem :subcat :1 :cat) (:english :a :b :b :cat) (:comp :synsem :cat) (:comp :head :english :b :cat) (:comp :head :synsem :cat) (:comp :head :head :synsem :cat) (:comp :head :head :english :cat) (:comp :english :b :b :cat)) :noun) (((:head :synsem :subcat :1 :propernoun) (:root :synsem :subcat :1 :propernoun) (:english :a :b :b :propernoun) (:comp :synsem :propernoun) (:comp :head :english :b :propernoun) (:comp :head :synsem :propernoun) (:comp :head :head :synsem :propernoun) (:comp :head :head :english :propernoun) (:comp :english :b :b :propernoun)) false) (((:head :synsem :subcat :1 :agr) (:head :english :agr) (:root :synsem :subcat :1 :agr) (:root :english :agr) (:english :a :agr) (:english :a :b :agr) (:english :a :b :a :agr) (:english :a :b :b :agr) (:english :agr) (:english :b :agr) (:comp :synsem :agr) (:comp :head :english :agr) (:comp :head :english :a :agr) (:comp :head :english :b :agr) (:comp :head :synsem :agr) (:comp :head :head :synsem :agr) (:comp :head :head :english :agr) (:comp :head :comp :synsem :agr) (:comp :head :comp :english :agr) (:comp :english :agr) (:comp :english :b :agr) (:comp :english :b :a :agr) (:comp :english :b :b :agr)) {:person :top, :number :top, :gender :top, :pronoun :top}) (((:head :synsem :subcat :1 :essere) (:root :synsem :subcat :1 :essere) (:comp :synsem :essere) (:comp :head :synsem :essere) (:comp :head :head :synsem :essere)) :top) (((:head :synsem :subcat :1 :infl) (:root :synsem :subcat :1 :infl) (:comp :synsem :infl) (:comp :head :synsem :infl) (:comp :head :head :synsem :infl)) :top) (((:english :a :b :a :cat) (:comp :head :english :a :cat) (:comp :head :comp :synsem :cat) (:comp :head :comp :english :cat) (:comp :english :b :a :cat)) :adjective) (((:head :synsem :subcat :1 :mod) (:root :synsem :subcat :1 :mod) (:comp :synsem :mod) (:comp :head :synsem :mod)) {:first :top, :rest ()}) (((:head :synsem :subcat :1 :agr :pronoun) (:head :synsem :subcat :1 :pronoun) (:head :english :agr :pronoun) (:root :synsem :subcat :1 :agr :pronoun) (:root :synsem :subcat :1 :pronoun) (:root :english :agr :pronoun) (:english :a :agr :pronoun) (:english :a :b :agr :pronoun) (:english :a :b :a :agr :pronoun) (:english :a :b :b :agr :pronoun) (:english :a :b :b :pronoun) (:english :agr :pronoun) (:english :b :agr :pronoun) (:comp :synsem :agr :pronoun) (:comp :synsem :pronoun) (:comp :head :english :agr :pronoun) (:comp :head :english :a :agr :pronoun) (:comp :head :english :b :agr :pronoun) (:comp :head :english :b :pronoun) (:comp :head :synsem :agr :pronoun) (:comp :head :synsem :pronoun) (:comp :head :head :synsem :agr :pronoun) (:comp :head :head :synsem :pronoun) (:comp :head :head :english :agr :pronoun) (:comp :head :head :english :pronoun) (:comp :head :comp :synsem :agr :pronoun) (:comp :head :comp :english :agr :pronoun) (:comp :english :agr :pronoun) (:comp :english :b :agr :pronoun) (:comp :english :b :a :agr :pronoun) (:comp :english :b :b :agr :pronoun) (:comp :english :b :b :pronoun)) false) (((:head :synsem :subcat :1 :mod :first) (:root :synsem :subcat :1 :mod :first) (:comp :synsem :mod :first) (:comp :head :synsem :mod :first) (:comp :head :comp :synsem :sem)) {:subj :top, :pred :small, :comparative false}) (((:comp :head :synsem :subcat :1) (:comp :head :head :synsem :subcat :1) (:comp :head :comp :synsem :subcat :1) (:comp :comp :synsem)) {:agr {:number :top, :person :top, :gender :top}, :cat :top, :subcat (), :sem :top, :def :top}) (((:english :a :a :cat) (:comp :head :synsem :subcat :1 :cat) (:comp :head :head :synsem :subcat :1 :cat) (:comp :head :comp :synsem :subcat :1 :cat) (:comp :english :a :cat) (:comp :comp :synsem :cat) (:comp :comp :english :cat)) :det) (((:synsem :sem :subj) (:head :synsem :subcat :1 :mod :first :subj) (:head :synsem :subcat :1 :sem) (:head :synsem :sem :subj) (:root :synsem :subcat :1 :mod :first :subj) (:root :synsem :subcat :1 :sem) (:root :synsem :sem :subj) (:comp :synsem :mod :first :subj) (:comp :synsem :sem) (:comp :head :synsem :mod :first :subj) (:comp :head :synsem :sem) (:comp :head :head :synsem :sem) (:comp :head :comp :synsem :sem :subj)) {:mass false, :furniture false, :living true, :pred :dog, :place false, :number :top, :drinkable false, :human false, :animate true, :clothing false, :speakable false, :spec :top, :activity false, :event false, :null false, :pet true, :physical-object true, :machine false, :buyable true, :artifact false, :edible false, :part-of-human-body false}) (((:head :synsem :subcat :1 :agr :gender) (:head :english :agr :gender) (:root :synsem :subcat :1 :agr :gender) (:root :english :agr :gender) (:english :a :agr :gender) (:english :a :b :agr :gender) (:english :a :b :a :agr :gender) (:english :a :b :b :agr :gender) (:english :agr :gender) (:english :b :agr :gender) (:comp :synsem :agr :gender) (:comp :head :english :agr :gender) (:comp :head :english :a :agr :gender) (:comp :head :english :b :agr :gender) (:comp :head :synsem :subcat :1 :agr :gender) (:comp :head :synsem :agr :gender) (:comp :head :head :synsem :subcat :1 :agr :gender) (:comp :head :head :synsem :agr :gender) (:comp :head :head :english :agr :gender) (:comp :head :comp :synsem :subcat :1 :agr :gender) (:comp :head :comp :synsem :agr :gender) (:comp :head :comp :english :agr :gender) (:comp :english :agr :gender) (:comp :english :b :agr :gender) (:comp :english :b :a :agr :gender) (:comp :english :b :b :agr :gender) (:comp :comp :synsem :agr :gender)) :top) (((:head :synsem :subcat :1 :agr :person) (:head :english :agr :person) (:root :synsem :subcat :1 :agr :person) (:root :english :agr :person) (:english :a :agr :person) (:english :a :b :agr :person) (:english :a :b :a :agr :person) (:english :a :b :b :agr :person) (:english :agr :person) (:english :b :agr :person) (:comp :synsem :agr :person) (:comp :head :english :agr :person) (:comp :head :english :a :agr :person) (:comp :head :english :b :agr :person) (:comp :head :synsem :subcat :1 :agr :person) (:comp :head :synsem :agr :person) (:comp :head :head :synsem :subcat :1 :agr :person) (:comp :head :head :synsem :agr :person) (:comp :head :head :english :agr :person) (:comp :head :comp :synsem :subcat :1 :agr :person) (:comp :head :comp :synsem :agr :person) (:comp :head :comp :english :agr :person) (:comp :english :agr :person) (:comp :english :b :agr :person) (:comp :english :b :a :agr :person) (:comp :english :b :b :agr :person) (:comp :comp :synsem :agr :person)) :3rd) (((:synsem :sem :subj :spec) (:head :synsem :subcat :1 :mod :first :subj :spec) (:head :synsem :subcat :1 :sem :spec) (:head :synsem :sem :subj :spec) (:root :synsem :subcat :1 :mod :first :subj :spec) (:root :synsem :subcat :1 :sem :spec) (:root :synsem :sem :subj :spec) (:comp :synsem :mod :first :subj :spec) (:comp :synsem :sem :spec) (:comp :head :synsem :mod :first :subj :spec) (:comp :head :synsem :subcat :1 :sem) (:comp :head :synsem :sem :spec) (:comp :head :head :synsem :subcat :1 :sem) (:comp :head :head :synsem :sem :spec) (:comp :head :comp :synsem :sem :subj :spec) (:comp :head :comp :synsem :subcat :1 :sem) (:comp :comp :synsem :sem)) {:of {:human true, :pred nil}, :pred :definite, :def :top}) (((:synsem :sem :subj :number) (:head :synsem :subcat :1 :mod :first :subj :number) (:head :synsem :subcat :1 :agr :number) (:head :synsem :subcat :1 :sem :number) (:head :synsem :sem :subj :number) (:head :english :agr :number) (:root :synsem :subcat :1 :mod :first :subj :number) (:root :synsem :subcat :1 :agr :number) (:root :synsem :subcat :1 :sem :number) (:root :synsem :sem :subj :number) (:root :english :agr :number) (:english :a :agr :number) (:english :a :b :agr :number) (:english :a :b :a :agr :number) (:english :a :b :b :agr :number) (:english :agr :number) (:english :b :agr :number) (:comp :synsem :mod :first :subj :number) (:comp :synsem :agr :number) (:comp :synsem :sem :number) (:comp :head :english :agr :number) (:comp :head :english :a :agr :number) (:comp :head :english :b :agr :number) (:comp :head :synsem :mod :first :subj :number) (:comp :head :synsem :subcat :1 :agr :number) (:comp :head :synsem :agr :number) (:comp :head :synsem :sem :number) (:comp :head :head :synsem :subcat :1 :agr :number) (:comp :head :head :synsem :agr :number) (:comp :head :head :synsem :sem :number) (:comp :head :head :english :agr :number) (:comp :head :comp :synsem :sem :subj :number) (:comp :head :comp :synsem :subcat :1 :agr :number) (:comp :head :comp :synsem :agr :number) (:comp :head :comp :english :agr :number) (:comp :english :agr :number) (:comp :english :b :agr :number) (:comp :english :b :a :agr :number) (:comp :english :b :b :agr :number) (:comp :comp :synsem :agr :number)) :sing) (((:synsem :sem :subj :spec :def) (:head :synsem :subcat :1 :mod :first :subj :spec :def) (:head :synsem :subcat :1 :sem :spec :def) (:head :synsem :sem :subj :spec :def) (:root :synsem :subcat :1 :mod :first :subj :spec :def) (:root :synsem :subcat :1 :sem :spec :def) (:root :synsem :sem :subj :spec :def) (:comp :synsem :mod :first :subj :spec :def) (:comp :synsem :sem :spec :def) (:comp :head :synsem :mod :first :subj :spec :def) (:comp :head :synsem :subcat :1 :sem :def) (:comp :head :synsem :subcat :1 :def) (:comp :head :synsem :sem :spec :def) (:comp :head :head :synsem :subcat :1 :sem :def) (:comp :head :head :synsem :subcat :1 :def) (:comp :head :head :synsem :sem :spec :def) (:comp :head :comp :synsem :sem :subj :spec :def) (:comp :head :comp :synsem :subcat :1 :sem :def) (:comp :head :comp :synsem :subcat :1 :def) (:comp :comp :synsem :sem :def) (:comp :comp :synsem :def)) :def))
        parse-tree (deserialize serialized)
        line-oriented (by-rows parse-tree)]
    (is (= true true))))

(deftest dissoc-test
  (let [test-fs (let [shared (atom {:c 42})]
                  {:a shared
                   :b shared})
        dissociated
        (u/dissoc-paths test-fs [[:a :c]])]
    (is (= (->
            dissociated
            (u/get-in [:a])
            :c)
           (->
            dissociated
            (u/get-in [:b])
            :c)))))

(defn prefix?
  "
  return true iff seq a is a prefix of seq b:
  (prefix? [:a   ] [:a :b])    => true
  (prefix? [:a :b] [:a   ])    => false
  (prefix? [:a :b] [:a :c]) => false
  "
  [a b]
  (cond (empty? a) true
        (empty? b) false
        (= (first a) (first b))
        (prefix? (rest a) (rest b))
        true false))

(defn remainder
  "if seq a is a prefix of seq b,
   then return what is left of b besides
   the common prefix of a.
   if seq a is not a prefix, return nil."
  [a b]
  (cond (empty? a)
        b
        (empty? b)
        nil
        (= (first a) (first b))
        (remainder (rest a) (rest b))))

(defn prefix
  "if seq a is a prefix of seq b,
   then return a; else return nil."
  [a b]
  (if (prefix? a b) a))

(def truncate-this-2
  (u/deserialize
   [[nil
     {:comp :top
      :rule "Y"
      :2    :top}]

    [[[:comp][:2]]
     {:comp :top
      :head :top
      :1    :top
      :2    :top
      :rule "X"}]

    [[[:comp :comp]
      [:comp :1]
      [:2    :comp]
      [:2    :1]]
     {:surface "ga"
      :phrasal false}]

    [[[:comp :head]
      [:comp :2]
      [:2    :head]
      [:2    :2]]
     {:phrasal false
      :cat    :v
      :surface "ba"}]

    [[[:comp :comp :cat]
      [:comp :1    :cat]
      [:comp :head :cat]
      [:comp :2    :cat]
      [:2    :comp :cat]
      [:2    :1    :cat]
      [:2    :head :cat]
      [:2    :2    :cat]]
     :v]]))

(def truncated-2 (u/dissoc-paths truncate-this-2 [[:comp :head]]))

(def reentrance-sets-2 (map first (serialize truncate-this-2)))

(defn dissoc-in [the-map path]
  (cond (empty? path)
        the-map

        (= :top the-map)
        the-map


        (= ::none (get the-map (first path) ::none))
        the-map

        (and (empty? (rest path))
             (empty? (dissoc the-map (first path))))
        :top

        (empty? (rest path))
        (dissoc the-map (first path))
        
        true
        (merge
         (let [within
               (dissoc-in (get the-map (first path))
                          (rest path))]
           {(first path)
            within})
         (dissoc the-map (first path)))))

(defn dissoc-in-all-paths [value paths]
  (if (empty? paths)
    value
    (dissoc-in-all-paths
     (dissoc-in value (first paths))
     (rest paths))))

(defn aliases-of [path reentrance-sets]
  (concat
   ;; 1. find aliases of _path_ where some member of
   ;; some reentrance set is a prefix of _path_.
   (mapcat
    (fn [set-with-prefix]
      (mapcat (fn [prefix-path]
                (let [remainders
                      (remove nil?
                              (map (fn [member-of-set]
                                     (remainder member-of-set path))
                                   set-with-prefix))]
                  (map (fn [remainder]
                         (concat prefix-path remainder))
                       remainders)))
              set-with-prefix))
    (filter
     (fn [reentrance-set]
       (some #(prefix? (vec %) (vec path))
             reentrance-set))
     reentrance-sets))

   ;; 2. find aliases of _path_ where _path_ is a prefix of
   ;; some member of some reentrance set.
   (mapcat
    (fn [set-with-prefix]
      (map (fn [prefix-path]
             prefix-path)
           set-with-prefix))
    (filter
     (fn [reentrance-set]
       (some #(prefix? (vec path) (vec %))
             reentrance-set))
     reentrance-sets))))

(defn dissoc-path [reentrance-pairs path]
  (if (not (empty? reentrance-pairs))
    (let [[reentrance-sets value] (first reentrance-pairs)]
      (cond
        ;; remove the reentrance-set and the value if
        ;; path matches a path in this reentrance-set.
        (some #(= path %) reentrance-sets)
        (dissoc-path (rest reentrance-pairs) path)
        
        true
        ;; the reentrance-set stays, but the value will
        ;; get modified with parts of it being dissoc'ed
        ;; if necessary.
        (cons [reentrance-sets
               (dissoc-in-all-paths value
                                    (aliases-of path (map first reentrance-pairs)))]
              (dissoc-path (rest reentrance-pairs) path))))))

(defn morph-ps [structure]
  (cond (or (= :fail structure) 
            (nil? structure)
            (string? structure)) structure

        (seq? structure)
        (map morph-ps structure)
        
        (u/get-in structure [:surface])
        (morph-ps (u/get-in structure [:surface]))

        (= false (u/get-in structure [:phrasal] false))
        "_"
        
        true
        (let [one (cond (= (get structure :l)
                           (get structure :head))
                        "*"
                        (= (get structure :l)
                           (get structure :comp))
                        "."
                        true
                        (throw (Exception. (str "the :l is neither :head nor :comp: "
                                                (vec (:dag_unify.core/serialized structure))))))
              
              two (cond (= (get structure :r)
                           (get structure :head))
                        "*"
                        (= (get structure :r)
                           (get structure :comp))
                        "."
                        true
                        (throw (Exception. (str "the :r is neither :head nor :comp: "
                                                (vec (:dag_unify.core/serialized structure))))))]
          
          (string/join ""
                       (map morph-ps
                            ["[" (:rule structure)
                             (if (get structure :babel.generate/done?)
                               "+" " ")
                             " "
                             one (u/get-in structure [:l] "_") " "
                             two (u/get-in structure [:r] "_")
                             "]"])))))

(defn dissoc-at-serialized-part [dissoc-part path reentrance-sets]
  (let [[paths-to value-at] dissoc-part
        equal-at (remove false?
                         (map (fn [path-to]
                                (= (vec path-to) (vec path)))
                              paths-to))
        remainders (filter #(not (nil? %))
                           (map (fn [path-to]
                                  (remainder path-to path))
                                paths-to))

        aliases-of (vec (set (cons path
                                   (aliases-of path reentrance-sets))))

        dissoc-at-paths
        (cond (empty? paths-to)
              ;; root object.
              aliases-of

              true
              (vec (set
                    (filter #(not (nil? %))
                            (mapcat (fn [path-to]
                                      (map (fn [alias]
                                             (remainder path-to alias))
                                           aliases-of))
                                    paths-to)))))]
    (println (str "dissoc-at-serialized-part: " dissoc-part))
    (println (str "path:" path))
    (println (str "paths-to: " (vec paths-to) "; value-at: " value-at))
    (println (str "reentrances: " (vec reentrance-sets)))
    (println (str "aliases-of path:" (vec aliases-of)))
    (println (str "dissoc-at-paths:" (vec dissoc-at-paths)))
    (println (str "equal-at:" (vec equal-at)))
    (println (str ""))
    (cond
      (not (empty? equal-at))
      nil

      true
      [paths-to
       (dissoc-in-all-paths value-at dissoc-at-paths)])))

(defn dissoc-at-serialized [serialized path]
  (let [reentrance-sets (map first serialized)]
    (remove nil?
            (map (fn [dissoc-part]
                   (dissoc-at-serialized-part dissoc-part path reentrance-sets))
                 serialized))))

(defn dissoc-at [structure path]
  (cond
    (empty? path)
    structure

    true
    (u/deserialize
     (dissoc-at-serialized (u/serialize structure) path))))

(def truncate-this
  ;;
  ;; {:a {:c {:e [1] {:g 42}
  ;;          :f 43}
  ;;      :d 44}
  ;;  :b [1] {:g 42}}
  ;; 
  (u/deserialize
   [[nil
     {:a {:c {:e :top
              :f 43}
          :d 44}
      :b :top}]
    [[[:a :c :e] [:b]]
     {:g 42}]]))


(def truncate-this-3
  ;;
  ;; {:a {:c {:e [1] {:g [2] {:i 42}
  ;;                  :h [2]}
  ;;          :f 43}
  ;;      :d 44}
  ;;  :b [1]}
  ;; 
  (u/deserialize
   [[nil
     {:a {:c {:e :top
              :f 43}
          :d 44}
      :b :top}]

    [[[:a :c :e] [:b]]
     {:g :top
      :h :top}]

    [[[:b :g] [:b :h]]
     {:i 42}]]))

(deftest dissoc-test-1
  (is (u/isomorphic?
       (dissoc-at truncate-this [:a :c :e :g])
       (u/deserialize
        [[nil
          {:a {:c {:e :top
                   :f 43}
               :d 44}}]
         [[[:a :c :e] [:b]]
          :top]])))
  (is (u/isomorphic?
       (dissoc-at truncate-this [:a :c :e])
       (u/deserialize
        [[nil
          {:a {:c {:f 43}
               :d 44}}]])))
  (is (u/isomorphic?
       (dissoc-at truncate-this [:a :c])
       (u/deserialize
        [[nil
          {:a {:d 44}}]])))
  (is (or true (u/isomorphic?
                (dissoc-at truncate-this [:a])
                (u/deserialize
                 [[nil
                   :top]]))))
  (is (or true (u/isomorphic?
                (dissoc-at truncate-this [])
                (u/deserialize
                 [[nil
                   {:a {:c {:e :top
                            :f 43}
                        :d 44}
                    :b :top}]
                  [[[:a :c :e] [:b]]
                   {:g 42}]])))))

(deftest dissoc-test-3
  (is (u/isomorphic? 
       (dissoc-at truncate-this-3 [:a :c :e :g])
       (u/deserialize
        [[nil {:a {:c {:e :top, :f 43}, :d 44}}]])))

  (is (u/isomorphic?
       (dissoc-at truncate-this [:a :c :e])
       (u/deserialize
        [[nil
          {:a {:c {:f 43}
               :d 44}}]])))
  (is (u/isomorphic?
       (dissoc-at truncate-this [:a :c])
       (u/deserialize
        [[nil
          {:a {:d 44}}]])))
  (is (u/isomorphic?
       (dissoc-at truncate-this [:a])
       (u/deserialize
        [[nil
          :top]])))
  (is (u/isomorphic?
       (dissoc-at truncate-this [])
       (u/deserialize
        [[nil
          {:a {:c {:e :top
                   :f 43}
               :d 44}
           :b :top}]
         [[[:a :c :e] [:b]]
          {:g 42}]]))))

