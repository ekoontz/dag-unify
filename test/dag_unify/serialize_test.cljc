(ns dag_unify.serialize-test
  (:require #?(:clj [clojure.test :refer [deftest is]])
            #?(:cljs [cljs.test :refer-macros [deftest is]])
            [dag_unify.core :as u]
            [dag_unify.dissoc :as d]
            [dag_unify.serialization :as s]))

(def serialized
  '((nil {:phrasal true, :synsem {:aux :top, :cat :top, :slash false, :essere :top, :infl :top, :subcat (), :agr :top, :modal :top, :sem :top, :pronoun :top}, :rule "sentence-nonphrasal-head", :modified false, :head :top, :root :top, :comment "c10", :english {:a :top, :agr :top, :b :top}, :schema-symbol c10, :first :comp, :comp {:phrasal true, :synsem :top, :rule "noun-phrase1", :head {:phrasal true, :english :top, :synsem {:cat :top, :essere :top, :infl :top, :mod :top, :reflexive false, :subcat {:2 (), :1 :top}, :agr :top, :sem :top, :propernoun :top, :pronoun :top}, :rule "nbar1", :head {:phrasal false, :synsem {:cat :top, :essere :top, :infl :top, :mod (), :reflexive false, :subcat {:2 (), :1 :top}, :agr :top, :sem :top, :case :top, :propernoun :top, :pronoun :top}, :english :top}, :comment "c11-comp-subcat-1", :schema-symbol c11-comp-subcat-1, :first :comp, :comp {:synsem {:sem :top, :cat :top, :subcat {:2 (), :1 :top}, :agr :top}, :english :top, :phrasal false}}, :comment "c10", :english :top, :schema-symbol c10, :first :comp, :aliases ("np1"), :comp {:synsem :top, :english :top, :phrasal false}}}) (((:head) (:root)) {:slash false, :phrasal false, :synsem {:aux :top, :cat :top, :participle false, :essere :top, :infl :top, :subcat {:2 (), :1 :top}, :agr :top, :modal :top, :sem :top, :pronoun :top}, :english :top, :applied {:2 true}, :modal-with false, :share-sem false, :phrasal-verb false}) (((:english :a) (:comp :english)) {:a :top, :agr :top, :b :top}) (((:head :english) (:root :english) (:english :b)) {:agr :top, :infl :top, :english "sleep", :past "slept", :cat :top, :exception false}) (((:synsem :aux) (:head :synsem :aux) (:root :synsem :aux)) false) (((:synsem :agr) (:head :synsem :agr) (:root :synsem :agr)) :top) (((:synsem :sem) (:head :synsem :sem) (:root :synsem :sem)) {:obj :unspec, :shared-with-obj false, :reflexive false, :subj :top, :pred :sleep, :tense :present, :aspect :simple}) (((:english :a :a) (:comp :english :a) (:comp :comp :english)) {:english "the", :cat :top}) (((:synsem :essere) (:head :synsem :essere) (:root :synsem :essere)) :top) (((:english :a :b) (:comp :head :english) (:comp :english :b)) {:agr :top, :a :top, :b :top}) (((:synsem :infl) (:head :synsem :infl) (:head :english :infl) (:root :synsem :infl) (:root :english :infl) (:english :b :infl)) :present) (((:synsem :cat) (:head :synsem :cat) (:head :english :cat) (:root :synsem :cat) (:root :english :cat) (:english :b :cat)) :verb) (((:synsem :pronoun) (:head :synsem :pronoun) (:root :synsem :pronoun)) :top) (((:synsem :modal) (:head :synsem :modal) (:root :synsem :modal)) false) (((:head :synsem :subcat :1) (:root :synsem :subcat :1) (:comp :synsem)) {:cat :top, :essere :top, :top :top, :infl :top, :mod :top, :reflexive false, :subcat (), :agr :top, :sem :top, :case :nom, :propernoun :top, :pronoun :top}) (((:english :a :b :b) (:comp :head :english :b) (:comp :head :head :english) (:comp :english :b :b)) {:agr :top, :propernoun :top, :pronoun :top, :english "dog", :cat :top}) (((:english :a :b :a) (:comp :head :english :a) (:comp :head :comp :english) (:comp :english :b :a)) {:agr :top, :english "small", :cat :top}) (((:head :synsem :subcat :1 :cat) (:root :synsem :subcat :1 :cat) (:english :a :b :b :cat) (:comp :synsem :cat) (:comp :head :english :b :cat) (:comp :head :synsem :cat) (:comp :head :head :synsem :cat) (:comp :head :head :english :cat) (:comp :english :b :b :cat)) :noun) (((:head :synsem :subcat :1 :propernoun) (:root :synsem :subcat :1 :propernoun) (:english :a :b :b :propernoun) (:comp :synsem :propernoun) (:comp :head :english :b :propernoun) (:comp :head :synsem :propernoun) (:comp :head :head :synsem :propernoun) (:comp :head :head :english :propernoun) (:comp :english :b :b :propernoun)) false) (((:head :synsem :subcat :1 :agr) (:head :english :agr) (:root :synsem :subcat :1 :agr) (:root :english :agr) (:english :a :agr) (:english :a :b :agr) (:english :a :b :a :agr) (:english :a :b :b :agr) (:english :agr) (:english :b :agr) (:comp :synsem :agr) (:comp :head :english :agr) (:comp :head :english :a :agr) (:comp :head :english :b :agr) (:comp :head :synsem :agr) (:comp :head :head :synsem :agr) (:comp :head :head :english :agr) (:comp :head :comp :synsem :agr) (:comp :head :comp :english :agr) (:comp :english :agr) (:comp :english :b :agr) (:comp :english :b :a :agr) (:comp :english :b :b :agr)) {:person :top, :number :top, :gender :top, :pronoun :top}) (((:head :synsem :subcat :1 :essere) (:root :synsem :subcat :1 :essere) (:comp :synsem :essere) (:comp :head :synsem :essere) (:comp :head :head :synsem :essere)) :top) (((:head :synsem :subcat :1 :infl) (:root :synsem :subcat :1 :infl) (:comp :synsem :infl) (:comp :head :synsem :infl) (:comp :head :head :synsem :infl)) :top) (((:english :a :b :a :cat) (:comp :head :english :a :cat) (:comp :head :comp :synsem :cat) (:comp :head :comp :english :cat) (:comp :english :b :a :cat)) :adjective) (((:head :synsem :subcat :1 :mod) (:root :synsem :subcat :1 :mod) (:comp :synsem :mod) (:comp :head :synsem :mod)) {:first :top, :rest ()}) (((:head :synsem :subcat :1 :agr :pronoun) (:head :synsem :subcat :1 :pronoun) (:head :english :agr :pronoun) (:root :synsem :subcat :1 :agr :pronoun) (:root :synsem :subcat :1 :pronoun) (:root :english :agr :pronoun) (:english :a :agr :pronoun) (:english :a :b :agr :pronoun) (:english :a :b :a :agr :pronoun) (:english :a :b :b :agr :pronoun) (:english :a :b :b :pronoun) (:english :agr :pronoun) (:english :b :agr :pronoun) (:comp :synsem :agr :pronoun) (:comp :synsem :pronoun) (:comp :head :english :agr :pronoun) (:comp :head :english :a :agr :pronoun) (:comp :head :english :b :agr :pronoun) (:comp :head :english :b :pronoun) (:comp :head :synsem :agr :pronoun) (:comp :head :synsem :pronoun) (:comp :head :head :synsem :agr :pronoun) (:comp :head :head :synsem :pronoun) (:comp :head :head :english :agr :pronoun) (:comp :head :head :english :pronoun) (:comp :head :comp :synsem :agr :pronoun) (:comp :head :comp :english :agr :pronoun) (:comp :english :agr :pronoun) (:comp :english :b :agr :pronoun) (:comp :english :b :a :agr :pronoun) (:comp :english :b :b :agr :pronoun) (:comp :english :b :b :pronoun)) false) (((:head :synsem :subcat :1 :mod :first) (:root :synsem :subcat :1 :mod :first) (:comp :synsem :mod :first) (:comp :head :synsem :mod :first) (:comp :head :comp :synsem :sem)) {:subj :top, :pred :small, :comparative false}) (((:comp :head :synsem :subcat :1) (:comp :head :head :synsem :subcat :1) (:comp :head :comp :synsem :subcat :1) (:comp :comp :synsem)) {:agr {:number :top, :person :top, :gender :top}, :cat :top, :subcat (), :sem :top, :def :top}) (((:english :a :a :cat) (:comp :head :synsem :subcat :1 :cat) (:comp :head :head :synsem :subcat :1 :cat) (:comp :head :comp :synsem :subcat :1 :cat) (:comp :english :a :cat) (:comp :comp :synsem :cat) (:comp :comp :english :cat)) :det) (((:synsem :sem :subj) (:head :synsem :subcat :1 :mod :first :subj) (:head :synsem :subcat :1 :sem) (:head :synsem :sem :subj) (:root :synsem :subcat :1 :mod :first :subj) (:root :synsem :subcat :1 :sem) (:root :synsem :sem :subj) (:comp :synsem :mod :first :subj) (:comp :synsem :sem) (:comp :head :synsem :mod :first :subj) (:comp :head :synsem :sem) (:comp :head :head :synsem :sem) (:comp :head :comp :synsem :sem :subj)) {:mass false, :furniture false, :living true, :pred :dog, :place false, :number :top, :drinkable false, :human false, :animate true, :clothing false, :speakable false, :spec :top, :activity false, :event false, :null false, :pet true, :physical-object true, :machine false, :buyable true, :artifact false, :edible false, :part-of-human-body false}) (((:head :synsem :subcat :1 :agr :gender) (:head :english :agr :gender) (:root :synsem :subcat :1 :agr :gender) (:root :english :agr :gender) (:english :a :agr :gender) (:english :a :b :agr :gender) (:english :a :b :a :agr :gender) (:english :a :b :b :agr :gender) (:english :agr :gender) (:english :b :agr :gender) (:comp :synsem :agr :gender) (:comp :head :english :agr :gender) (:comp :head :english :a :agr :gender) (:comp :head :english :b :agr :gender) (:comp :head :synsem :subcat :1 :agr :gender) (:comp :head :synsem :agr :gender) (:comp :head :head :synsem :subcat :1 :agr :gender) (:comp :head :head :synsem :agr :gender) (:comp :head :head :english :agr :gender) (:comp :head :comp :synsem :subcat :1 :agr :gender) (:comp :head :comp :synsem :agr :gender) (:comp :head :comp :english :agr :gender) (:comp :english :agr :gender) (:comp :english :b :agr :gender) (:comp :english :b :a :agr :gender) (:comp :english :b :b :agr :gender) (:comp :comp :synsem :agr :gender)) :top) (((:head :synsem :subcat :1 :agr :person) (:head :english :agr :person) (:root :synsem :subcat :1 :agr :person) (:root :english :agr :person) (:english :a :agr :person) (:english :a :b :agr :person) (:english :a :b :a :agr :person) (:english :a :b :b :agr :person) (:english :agr :person) (:english :b :agr :person) (:comp :synsem :agr :person) (:comp :head :english :agr :person) (:comp :head :english :a :agr :person) (:comp :head :english :b :agr :person) (:comp :head :synsem :subcat :1 :agr :person) (:comp :head :synsem :agr :person) (:comp :head :head :synsem :subcat :1 :agr :person) (:comp :head :head :synsem :agr :person) (:comp :head :head :english :agr :person) (:comp :head :comp :synsem :subcat :1 :agr :person) (:comp :head :comp :synsem :agr :person) (:comp :head :comp :english :agr :person) (:comp :english :agr :person) (:comp :english :b :agr :person) (:comp :english :b :a :agr :person) (:comp :english :b :b :agr :person) (:comp :comp :synsem :agr :person)) :3rd) (((:synsem :sem :subj :spec) (:head :synsem :subcat :1 :mod :first :subj :spec) (:head :synsem :subcat :1 :sem :spec) (:head :synsem :sem :subj :spec) (:root :synsem :subcat :1 :mod :first :subj :spec) (:root :synsem :subcat :1 :sem :spec) (:root :synsem :sem :subj :spec) (:comp :synsem :mod :first :subj :spec) (:comp :synsem :sem :spec) (:comp :head :synsem :mod :first :subj :spec) (:comp :head :synsem :subcat :1 :sem) (:comp :head :synsem :sem :spec) (:comp :head :head :synsem :subcat :1 :sem) (:comp :head :head :synsem :sem :spec) (:comp :head :comp :synsem :sem :subj :spec) (:comp :head :comp :synsem :subcat :1 :sem) (:comp :comp :synsem :sem)) {:of {:human true, :pred nil}, :pred :definite, :def :top}) (((:synsem :sem :subj :number) (:head :synsem :subcat :1 :mod :first :subj :number) (:head :synsem :subcat :1 :agr :number) (:head :synsem :subcat :1 :sem :number) (:head :synsem :sem :subj :number) (:head :english :agr :number) (:root :synsem :subcat :1 :mod :first :subj :number) (:root :synsem :subcat :1 :agr :number) (:root :synsem :subcat :1 :sem :number) (:root :synsem :sem :subj :number) (:root :english :agr :number) (:english :a :agr :number) (:english :a :b :agr :number) (:english :a :b :a :agr :number) (:english :a :b :b :agr :number) (:english :agr :number) (:english :b :agr :number) (:comp :synsem :mod :first :subj :number) (:comp :synsem :agr :number) (:comp :synsem :sem :number) (:comp :head :english :agr :number) (:comp :head :english :a :agr :number) (:comp :head :english :b :agr :number) (:comp :head :synsem :mod :first :subj :number) (:comp :head :synsem :subcat :1 :agr :number) (:comp :head :synsem :agr :number) (:comp :head :synsem :sem :number) (:comp :head :head :synsem :subcat :1 :agr :number) (:comp :head :head :synsem :agr :number) (:comp :head :head :synsem :sem :number) (:comp :head :head :english :agr :number) (:comp :head :comp :synsem :sem :subj :number) (:comp :head :comp :synsem :subcat :1 :agr :number) (:comp :head :comp :synsem :agr :number) (:comp :head :comp :english :agr :number) (:comp :english :agr :number) (:comp :english :b :agr :number) (:comp :english :b :a :agr :number) (:comp :english :b :b :agr :number) (:comp :comp :synsem :agr :number)) :sing) (((:synsem :sem :subj :spec :def) (:head :synsem :subcat :1 :mod :first :subj :spec :def) (:head :synsem :subcat :1 :sem :spec :def) (:head :synsem :sem :subj :spec :def) (:root :synsem :subcat :1 :mod :first :subj :spec :def) (:root :synsem :subcat :1 :sem :spec :def) (:root :synsem :sem :subj :spec :def) (:comp :synsem :mod :first :subj :spec :def) (:comp :synsem :sem :spec :def) (:comp :head :synsem :mod :first :subj :spec :def) (:comp :head :synsem :subcat :1 :sem :def) (:comp :head :synsem :subcat :1 :def) (:comp :head :synsem :sem :spec :def) (:comp :head :head :synsem :subcat :1 :sem :def) (:comp :head :head :synsem :subcat :1 :def) (:comp :head :head :synsem :sem :spec :def) (:comp :head :comp :synsem :sem :subj :spec :def) (:comp :head :comp :synsem :subcat :1 :sem :def) (:comp :head :comp :synsem :subcat :1 :def) (:comp :comp :synsem :sem :def) (:comp :comp :synsem :def)) :def)))

(defn benchmark []
  (count
    (take 10
          (repeatedly
           #(time (type (apply
                         u/unify
                         (take 50 (repeatedly (fn [] (s/deserialize serialized)))))))))))

(defn ser-db [input-map]
  (let [refs (s/all-refs input-map)
        skels (s/skels input-map refs)]
    (s/ref-skel-map input-map)))

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
          {;; TODO: could also be '((:b)(:a)).
           :ref ref1
           :skel {:c :top}} '((:a)(:b))
          {;; TODO: could also be '((:b :c)(:a :c)(:d))
           ;; (or other possible orderings).
           :ref ref2
           :skel 42} '((:a :c)(:b :c)(:d))}))))
          

(deftest serialize-1
  (let [ref1 (atom 42)
        mymap {:a ref1, :b ref1}
        ser (s/serialize mymap)]
    (is (= ser
           [[[] {:a :top, :b :top}] [[[:a] [:b]] 42]]))))

(deftest serialize-2
  (let [ref2 (atom 42)
        ref1 (atom {:c ref2})
        mymap {:a ref1, :b ref1 :d ref2}
        ser (s/serialize mymap)]
    (is
     (or
      (= ser
         [[[] {:a :top, :b :top, :d :top}]
          [[[:a :c] [:b :c] [:d]] 42]
          [[[:a] [:b]] {:c :top}]])
      (= ser
         [[[] {:a :top, :b :top, :d :top}]
          [[[:a] [:b]] {:c :top}]
          [[[:a :c] [:b :c] [:d]] 42]])))))

(deftest serialize-3
  (let [mymap {:a 42 :b (atom 43)}]
    (is (not (nil? (s/serialize mymap))))))

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
        serialized (s/serialize vp)]
        
    (not (nil? vp))
    (not (nil? serialized))
    (= (count serialized) 4)))

(deftest create-path-in-1
  (let [path '(:a :b :c :d :e)
        val 43
        result (s/create-path-in path val)]
    (is (= (u/get-in result path) val))))

(deftest deser-with-ref
  (let [serialized [[nil {:a "PH"}] [[["a"]] 42]]
        deserialized (s/deserialize serialized true)]
    (is (not (nil? deserialized)))
    (is (= (s/ref? (:a deserialized))))
    (is (= @(:a deserialized) 42))))

;; deserialize a map's serialized form
(deftest deser-1
  (let [ref2 (atom 42)
        ref1 (atom {:c ref2})
        mymap {:a ref1, :b ref1 :d ref2}
        my-ser (s/serialize mymap)
        my-deser (s/deserialize my-ser)]
    (is (not (= my-ser nil)))
    (is (s/ref? (:a my-deser)))

    ;; a)
    (is (= (s/ref? (get @(get my-deser :a) :c))))
    (is (= (s/ref? (get @(get my-deser :b) :c))))
    (is (= (s/ref? (:d my-deser))))
    (is (= (:a my-deser) (:b my-deser)))
    (is (= (get @(get my-deser :a) :c)
           (get my-deser :d)))
    (is (= (get @(get my-deser :b) :c)
           (get my-deser :d)))
    (is (= @(get @(get my-deser :a) :c)
           42))

    ;; similar tests as a) above, but using fs/get-in
    (is (number? (u/get-in my-deser [:a :c])))
    (is (number? (u/get-in my-deser [:b :c])))
    (is (number? (u/get-in my-deser [:d])))
    (is (= (u/get-in my-deser [:a :c])
           (u/get-in my-deser [:d])))
    (is (= (u/get-in my-deser [:b :c])
           (u/get-in my-deser [:d])))
    (is (= (u/get-in my-deser [:a :c])
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
        myser (s/serialize vp)]
        
    (not (nil? vp))
    (not (nil? myser))))


(def original-tree
  (let [ref8 (atom {:number :sing})
        ref9 (atom ref8)
        ref16 (atom :top)
        ref17 (atom false)
        ref18 (atom :top)
        ref19 (atom ref8)
        ref20 (atom {:agr ref19})
        ref22 (atom {:agr ref8
                     :exceptions ref16
                     :subcat {:1 {:agr ref19}}})
        ]
    {:agr ref8
     :comp ref20
     :head ref22
     :syntax-tree {:2 {:agr ref9
                       :exceptions ref16}}
     :1 ref20
     :2 ref22}))

(deftest serialize-5
  (is (= (count (s/serialize original-tree))
         5))
  (is (= (count (s/serialize2 original-tree))
         5)))

