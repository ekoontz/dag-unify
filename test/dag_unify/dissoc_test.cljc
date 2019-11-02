(ns dag_unify.dissoc-test
  (:require #?(:clj [clojure.test :refer [deftest is]])
            #?(:cljs [cljs.test :refer-macros [deftest is]])
            [dag_unify.core :as u]
            [dag_unify.dissoc :as d]
            [dag_unify.serialization :as s]))

(defn isomorphic? [a b]
  (cond (and (map? a)
             (map? b)
             (empty? a)
             (empty? b))
        true  ;; two empty maps are equal
        (and (map? a)
             (map? b)
             (or (empty? a)
                 (empty? b)))
        false ;; two maps whose key cardinality (different number of keys) is different are not equal.
        (and (map? a)
             (map? b))
        (let [a (dissoc a :dag_unify.serialization/serialized)
              b (dissoc b :dag_unify.serialization/serialized)]
          (and (isomorphic? (get a (first (keys a))) ;; two maps are isomorphic if their keys' values are isomorphic.
                            (get b (first (keys a))))
               (isomorphic? (dissoc a (first (keys a)))
                            (dissoc b (first (keys a))))))
        (and (u/ref? a)
             (u/ref? b))
        (isomorphic? @a @b)
        true
        (= a b)))

(def truncate-this
  ;;
  ;; {:a {:c {:e [1] {:g 42}
  ;;          :f 43}
  ;;      :d 44}
  ;;  :b [1] {:g 42}}
  ;; 
  (s/deserialize
   [[nil
     {:a {:c {:e :top
              :f 43}
          :d 44}
      :b :top}]
    [[[:a :c :e] [:b]]
     {:g 42}]]))

(deftest dissoc-test
  (let [test-fs (let [shared (atom {:c 42})]
                  {:a shared
                   :b shared})
        dissociated
        (d/dissoc-in test-fs [:a :c])]
    (is (isomorphic? dissociated
                     (let [shared (atom :top)]
                       {:a shared
                        :b shared})))))

(deftest dissoc-test-1
  (is (isomorphic?
       (d/dissoc-in truncate-this [:a :c :e :g])
       (s/deserialize
        [[nil
          {:a {:c {:e :top
                   :f 43}
               :d 44}}]
         [[[:a :c :e] [:b]]
          :top]])))
  (is (isomorphic?
       (d/dissoc-in truncate-this [:a :c :e])
       (s/deserialize
        [[nil
          {:a {:c {:f 43}
               :d 44}}]])))
  (is (isomorphic?
       (d/dissoc-in truncate-this [:a :c])
       (s/deserialize
        [[nil
          {:a {:d 44}}]])))
  (is (isomorphic?
       (d/dissoc-in truncate-this [:a])
       (s/deserialize
        [[nil
          :top]])))
  (is (isomorphic?
       (d/dissoc-in truncate-this [])
       (s/deserialize
        [[nil
          {:a {:c {:e :top
                   :f 43}
               :d 44}
           :b :top}]
         [[[:a :c :e] [:b]]
          {:g 42}]]))))

(def truncate-this-2
  ;;
  ;; {:a {:c {:e [1] {:g [2] {:i 42}
  ;;                  :h [2]}
  ;;          :f 43}
  ;;      :d 44}
  ;;  :b [1]}
  ;; 
  (s/deserialize
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

(deftest dissoc-test-2
  (is (isomorphic? 
       (d/dissoc-in truncate-this-2 [:a :c :e :g])
       (s/deserialize
        [[nil
          {:a {:c {:e :top
                   :f 43}
               :d 44}
           :b :top}]

         [[[:a :c :e] [:b]]
          :top]])))

  (is (isomorphic?
       (d/dissoc-in truncate-this [:a :c :e])
       (s/deserialize
        [[nil
          {:a {:c {:f 43}
               :d 44}}]])))
  (is (isomorphic?
       (d/dissoc-in truncate-this [:a :c])
       (s/deserialize
        [[nil
          {:a {:d 44}}]])))
  (is (isomorphic?
       (d/dissoc-in truncate-this [:a])
       (s/deserialize
        [[nil
          :top]])))
  (is (isomorphic?
       (d/dissoc-in truncate-this [])
       (s/deserialize
        [[nil
          {:a {:c {:e :top
                   :f 43}
               :d 44}
           :b :top}]
         [[[:a :c :e] [:b]]
          {:g 42}]]))))

(def truncate-this-3
  (s/deserialize
   '((nil
      {:comp :top,
       :head :top,
       :cat :top,
       :rule "Y",
       :phrasal true,
       :1 :top,
       :2 :top})
     (((:comp) (:2))
      {:comp :top,
       :head :top,
       :cat :top,
       :1 :top,
       :2 :top,
       :rule "X",
       :phrasal true})
     (((:head) (:1))
      {:cat :top,
       :phrasal false,
       :surface "ba"})
     (((:comp :comp) (:comp :1) (:2 :comp) (:2 :1))
      {:surface "ga",
       :cat :top,
       :phrasal false})
     (((:comp :head) (:comp :2) (:2 :head) (:2 :2))
      {:phrasal false,
       :cat :top,
       :surface "ba"})
     (((:head :cat) (:cat) (:1 :cat)) :v)
     (((:comp :comp :cat) (:comp :1 :cat) (:2 :comp :cat) (:2 :1 :cat)) :p)
     (((:comp :head :cat)
       (:comp :cat)
       (:comp :2 :cat)
       (:2 :head :cat)
       (:2 :cat)
       (:2 :2 :cat))
      :n))))

(deftest dissoc-test-3
  (is (isomorphic?
       (binding [d/remove-path? (fn [path]
                                  (or
                                   (d/prefix? [:head] path)
                                   (d/prefix? [:1] path)))]
         (d/dissoc-in truncate-this-3 [:head]))
       (s/deserialize
        '((nil {:rule "Y", :phrasal true, :comp :top, :2 :top, :cat :top})
          (((:comp) (:2))
           {:rule "X",
            :phrasal true,
            :comp :top,
            :1 :top,
            :head :top,
            :2 :top,
            :cat :top})
          (((:cat)) :v)
          (((:comp :comp) (:comp :1) (:2 :comp) (:2 :1))
           {:surface "ga", :phrasal false, :cat :top})
          (((:comp :head) (:comp :2) (:2 :head) (:2 :2))
           {:phrasal false, :surface "ba", :cat :top})
          (((:comp :comp :cat) (:comp :1 :cat) (:2 :comp :cat) (:2 :1 :cat)) :p)
          (((:comp :head :cat)
            (:comp :2 :cat)
            (:comp :cat)
            (:2 :head :cat)
            (:2 :2 :cat)
            (:2 :cat))
           :n))))))

