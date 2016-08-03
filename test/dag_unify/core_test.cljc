(ns dag_unify.core-test
  (:require #?(:clj [clojure.test :refer [deftest is]])
            #?(:cljs [cljs.test :refer-macros [deftest is]])
            [dag_unify.core :refer [all-refs create-path-in copy create-shared-values
                                    deserialize deserialize-with-remove
                                    expand-disj fail? get-in get-refs
                                    isomorphic? merge paths-to-value ref?
                                    recursive-dissoc
                                    refset2map ref-skel-map serialize
                                    remove-matching-keys
                                    skeletize skels step2 unify unifyc]])
  (:refer-clojure :exclude [get-in merge resolve]))

;; TODO: add more tests for (isomorphic?)

(defn ser-db [input-map]
  (let [refs (get-refs input-map)
        skels (skels input-map refs)]
    (ref-skel-map input-map)))

(deftest simple-merge-test
  (let [result (merge {:foo 99} {:bar 42})]
    (is (= (:foo result) 99))
    (is (= (:bar result) 42))))

(deftest simple-unify-test
  (let [result (unify {:foo 99} {:bar 42})]
    (is (= (:foo result) 99))
    (is (= (:bar result) 42))))

(deftest recursive-merge-of-3-maps
  (let [result
        (let [map1 {:foo {:bar 99}}
              map2 {:foo {:baz 42}}
              map3 {:biff 12}]
          (merge map1 map2 map3))]
    ;; test that result looks like:
    ;; {:foo {:bar 99
    ;;        :baz 42}
    ;;  :biff 12}}
    (is (= (:bar (:foo result)) 99))
    (is (= (:baz (:foo result)) 42))
    (is (= (:biff result) 12))
    (is (= (get-in result '(:foo :bar)) 99))
    (is (= (get-in result '(:foo :baz)) 42))
    (is (= (get-in result '(:biff)) 12))))

(deftest unify-unequal-atomic-values
  "Testing that unify(v1,v2)=fail if v1 != v2."
  (let [result (unify {:foo 42} {:foo 43})]
    (is (fail? result))))

(deftest merge-unequal-atomic-values
  "Testing that merge(v1,v2)=v2 (overriding)."
  (let [result (merge {:foo 42} {:foo 43})]
    (is (= (:foo result) 43))))

(deftest ignore-nils-in-values
  "Ignore nils in values (true,nil)."
  (let [result (merge {:foo true} {:foo nil})]
    (is (= (:foo result) true))))

(deftest merge-emptymap
  "emptymap (unlike with nil) overrides true in merge."
  (let [result (merge {:foo true} {:foo {}})]
    (is (= (:foo result) {}))))

(deftest merge-nil
  (let [result (merge {:foo nil} {:foo nil})]
    (is (= result {:foo nil}))))

(deftest unify-atomic-values-with-references
  (let [myref (atom :top)
        val 42
        result (unify myref val)]
    (is (ref? result))
    (is (= @result 42))))
      
;; {:a [1] :top
;;  :b [1]     } ,
;; {:a 42}
;;        =>
;; {:a [1] 42
;;  :b [1] }
(deftest merging-references-with-top-and-references
  (let [myref (atom :top)
        fs1 {:a myref :b myref}
        fs2 {:a 42}
        result (merge fs1 fs2)]
    (is (ref? (:a result)))
    (is (= @(:a result) 42))
    (is (= @(:b result) 42))
    (is (= (:a result) (:b result)))))

(deftest merging-with-references-with-top-and-references-2
  (let [myref (atom :top)
        fs1 {:a myref}
        fs2 {:a :foo}
        result (merge fs1 fs2)]
    (is (ref? (:a result)))
    (is (= @(:a result) :foo))))

(deftest merging-with-inner-reference-keyset
  (let [fs1 {:b (atom :top)}
        fs2 {:b 42}
        maps (list fs1 fs2)
        result (seq (set (mapcat #'keys maps)))] ;; mapcat->set->seq removes duplicates.
    (is (= result '(:b)))))

      ;; [b [1] :top], [b 42] => [b [1] 42]
(deftest merging-with-reference
  "merging with reference"
  (let [fs1 {:b (atom :top)}
        fs2 {:b 42}
        result (merge fs1 fs2)]
    (is (ref? (:b result)))
    (is (= @(:b result)) 42)))

;; [a [b [1] :top]], [a [b 42]] => [a [b [1] 42]]
(deftest merging-with-inner-reference
  "merging with inner reference"
  (let [fs1 {:a {:b (atom :top)}}
        fs2 {:a {:b 42}}
        result (merge fs1 fs2)]
    (is (ref? (:b (:a result))))
    (is (= @(:b (:a result))) 42)))

;; [a [b [1] top]], [a [b 42]] => [a [b [1] 42]]
(deftest merging-with-inner-reference-second-position
  (let [fs1 {:a {:b 42}}
        fs2 {:a {:b (atom :top)}}
        result (merge fs1 fs2)]
    (is (ref? (:b (:a result))))
    (is (= @(:b (:a result))) 42)))

(deftest merging-with-reference-second-position
  "merging with reference, second position"
  (let [fs1 {:a 42}
        fs2 {:a (atom :top)}
        result (merge fs1 fs2)]
    (is (ref? (:a result)))
    (is (= @(:a result) 42))))

(deftest merging-fail-with-not-1
  "test atom merging with ':not' (special feature) (first in list; fail)"
  (let [result (merge {:not 42} 42)]
    (is (= result :fail))))

(deftest merging-succeed-with-not
  "test atom merging with ':not' (special feature) (first in list; fail)"
  (let [result (merge 42 {:not 43})]
    (is (= result 42))))

(deftest merging-fail-with-not-2
  "test atom merging with ':not' (special feature) (second in list; fail)"
  (let [result (merge 42 {:not 42})]
    (is (= result :fail))))

(deftest unify-with-not
  (let [result (unify {:foo 42} {:foo {:not 43}})]
    (is (= result {:foo 42}))))

(deftest unify-fail-with-not
  "test atom unifying with ':not' (special feature) (first in list; fail)"
  (let [result (unify {:not 42} 42)]
    (is (= result :fail))))

(deftest unify-succeed-with-not
  "test atom unifying with ':not' (special feature) (second in list; succeed)"
  (let [result (unify 42 {:not 43})]
    (is (= result 42))))

(deftest unify-fail-with-not-2
  "test atom unifying with ':not' (special feature) (second in list; fail)"
  (let [result (unify 42 {:not 42})]
    (is (= result :fail))))

(deftest unify-nested-not
  "test unifying with ':not' (special feature)"
  (let [result (unify {:foo 42} {:foo {:not 43}})]
    (is (= result {:foo 42}))))

(deftest unify-with-not-and-top1
  "unifying {:not X} with :top should return {:not X} if X != top."
  (let [result (unify {:not 42} :top)]
    (is (= result {:not 42}))))

(deftest unify-with-not-and-top2
  "(reversed argument order as preceding): unifying :top with {:not X} should return {:not X} if X != top."
  (let [result (unify :top {:not 42})]
    (is (= result {:not 42}))))
      
(deftest complicated-merge
  (let [mycon (list {:comp {:number :singular, :cat :det}} {:gender :masc} {:comp {:def {:not :indef}}, :mass true} {:comp {}, :sport true})
        result (apply merge mycon)]
    (is (= (get-in result '(:comp :number)) :singular))))

(deftest merge-atomic-vals
  (let [result (merge 5 5)]
    (is (= result 5))))

(deftest unify-atomic-vals
  (let [result (unify 5 5)]
    (is (= result 5))))

(deftest unify-atomic-vals-fail
  (let [result (unify 5 4)]
    (is (= result :fail))))

(deftest maps-merge
  (let [result (merge '{:a 42} '{:b 43})]
    (is (= (:a result) 42)
        (= (:b result) 43))))

(deftest maps-unify
  (let [result (unify '{:a 42} '{:b 43})]
    (is (= (:a result) 42)
        (= (:b result) 43))))

(deftest merge-override
  (let [result (merge '{:a 42} '{:a 43})]
    (is (= (:a result) 43))))

(deftest unify-override
  (let [result (unify '{:a 42} '{:a 43})]
    (is (fail? result))))

(deftest map-with-reference
  (let [fs1 {:a (atom 42)}
        fs2 {:b (get fs1 :a)}
        result (unify fs1 fs2)]
    (is (ref? (:a result)))
    (is (ref? (:b result)))
    (is (= (:a result) (:b result)))))

(deftest unify-two-maps-one-with-references
  "unifying two maps, one with references."
  (let [fs1 {:a (atom :top)}
        fs2 {:b (get fs1 :a)}
        fs3 (unify fs1 fs2)
        fs4 {:a 42}
        result (unify fs3 fs4)]
    (is (ref? (:a result)))
    (is (ref? (:b result)))
    (is (= (:a result) (:b result)))
    (is (= @(:a result) 42))))

(deftest unify-two-maps-with-references
  "unifying two maps, both with references, same features"
  (let [fs1 {:a (atom 42)}
        fs2 {:b (get fs1 :a)}
        fs3 (unify fs1 fs2)
        fs4 {:a (atom 42)}
        fs5 {:b (get fs4 :a)}
        fs6 (unify fs4 fs5)
        result (unify fs3 fs6)]
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

(deftest paths-to-values-1
  "test path-to-value, which returns a list of all ways of reaching
a given value in a given map."
  (let [ref1 (atom 42)
        mymap {:a ref1 :b ref1}
        ptf (paths-to-value mymap ref1 nil)]
    (is (= ptf '((:a)(:b))))))

(deftest paths-to-values-2
  "test path-to-value, which returns a list of all ways of reaching
a given value in a given map."
  (let [ref2 (atom 42)
        ref1 (atom {:c ref2})
        mymap {:a ref1
               :b ref1
               :d ref2}
        paths-to-ref1 (paths-to-value mymap ref1 nil)]
    (is (= paths-to-ref1 '((:a)(:b))))))

(deftest paths-to-values-3
  "test path-to-value, which returns a list of all ways of reaching
a given value in a given map."
  (let [ref2 (atom 42)
        ref1 (atom {:c ref2})
        mymap {:a ref1
               :b ref1
               :d ref2}
        paths-to-ref2 (paths-to-value mymap ref2 nil)]
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
    (is (number? (get-in my-deser '(:a :c))))
    (is (number? (get-in my-deser '(:b :c))))
    (is (number? (get-in my-deser '(:d))))
    (is (= (get-in my-deser '(:a :c))
           (get-in my-deser '(:d))))
    (is (= (get-in my-deser '(:b :c))
           (get-in my-deser '(:d))))
    (is (= (get-in my-deser '(:a :c))
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

(deftest overflow
  "merge has a problem: we hit StackOverflowError java.util.regex.Pattern$BmpCharProperty.match (Pattern.java:3366) when this test is run.
   Code works as expected if merge is replaced with unify. However, this test passes - the SOE seems to only happen
when run from a REPL."
  (unify
   (get-in (merge (let [head-cat (atom :top)
                                    head-is-pronoun (atom :top)
                                    head-sem (atom :top)
                                    head-infl (atom :top)]
                                {:synsem {:cat head-cat
                                          :pronoun head-is-pronoun
                                          :sem head-sem
                                          :infl head-infl}
                                 :head {:synsem {:cat head-cat
                                                 :pronoun head-is-pronoun
                                                 :infl head-infl
                                                 :sem head-sem}}})

                              (let [essere (atom :top)
                                    infl (atom :top)]
                                {:italian {:a {:infl infl}}
                                 :english {:a {:infl infl}}
                                 :synsem {:infl infl
                                          :essere essere}
                                 :head {:italian {:infl infl}
                                        :english {:infl infl}
                                        :synsem {:essere essere
                                                 :infl infl}}}))
                 '(:head))
   (unify
    {:italian {:foo 42}}
    (let [infl (atom :top)]
      {:italian {:infl infl}
       :english {:infl infl}
       :synsem {:infl infl}}))))

(deftest nil-and-top
  ;; ...should return emptylist.
  (is (= nil
         (unify nil :top))))

(deftest nil-and-anything-except-top
  ;; ...should return :fail.
  (is (fail?
       (unify nil {:foo 42}))))

(deftest emptylist-and-top
  ;; ...should return emptylist.
  (is (= '()
         (unify '() :top))))

(deftest emptylist-and-anything-except-top
  ;; ...should return :fail.
  (is (fail?
       (unify '() {:foo 42}))))

(deftest refset2map-test
  (let [myref (atom #{1 2})
        input {:a myref
               :b #{{:c myref} {:d 3}}}
        result (refset2map input)]
    (is (map? result))
    (is (set? (get-in result '(:a))))
    (is (set? (get-in result '(:b))))
    (is (= myref (:ref (first (get-in result '(:a))))))
    (is (= myref (:ref (second (get-in result '(:a))))))
    (is (or (= 1 (:val (first (get-in result '(:a)))))
            (= 2 (:val (first (get-in result '(:a)))))))
    (is (= 2 (count (get-in result '(:b)))))
))

(deftest step2-test
  (let [myref (atom #{1 2})
        input {:a myref
               :b #{{:c myref} {:d 3}}}
        result (refset2map input)
        step2-result (step2 result)]
    (is (set? step2-result))
    (is (= (count step2-result) 6))))

(deftest test-final
  (let [input
        (let [myref (atom #{1 2})]
          {:a myref
           :b #{{:c myref} {:d 3}}})
        final (expand-disj input)]
    (= (count final) 2)))

(def parent
  (let [catref (atom :top)]
    {:head {:cat catref}
     :cat catref}))

(def disj-cat #{{:cat :noun}
                {:cat :verb}})

(def parent-with-disj
  (let [catref (atom #{{:cat :noun}
                      {:cat :verb}})]
    {:head {:cat catref}
     :cat catref}))

(deftest category-disjunction
  (let [result (expand-disj parent-with-disj)]
    (is (= (count result) 2))))

(deftest expand-constraints
  (let [constraints {:constraints #{{:synsem {:infl :futuro
                                              :sem {:tense :futuro}}}
                                    {:synsem {:infl :present
                                              :sem {:tense :present}}}}}
        constraints-expanded (expand-disj constraints)]
    (is (= (count constraints-expanded) 2))))

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
        result (unify arg1 arg2)]
    (is (not (fail? result)))
    (is (= result
           {:italiano "gatto"}))))

(deftest unify-with-string4
  (let [arg1 {:italiano "gatto"}
        arg2 {:italiano "cane"}
        result (unify arg1 arg2)]
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
              (unify common
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
                (unify common
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
