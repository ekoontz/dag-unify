(ns dag_unify.dissoc-test
  (:require #?(:clj [clojure.test :refer [deftest is]])
            #?(:cljs [cljs.test :refer-macros [deftest is]])
            [dag_unify.core :as u]
            [dag_unify.dissoc :as d]))

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

(deftest dissoc-test
  (let [test-fs (let [shared (atom {:c 42})]
                  {:a shared
                   :b shared})
        dissociated
        (d/dissoc-in test-fs [:a :c])]
    (is (u/isomorphic? dissociated
                     (let [shared (atom :top)]
                       {:a shared
                        :b shared})))))

(deftest dissoc-test-1
  (is (u/isomorphic?
       (d/dissoc-in truncate-this [:a :c :e :g])
       (u/deserialize
        [[nil
          {:a {:c {:e :top
                   :f 43}
               :d 44}}]
         [[[:a :c :e] [:b]]
          :top]])))
  (is (u/isomorphic?
       (d/dissoc-in truncate-this [:a :c :e])
       (u/deserialize
        [[nil
          {:a {:c {:f 43}
               :d 44}}]])))
  (is (u/isomorphic?
       (d/dissoc-in truncate-this [:a :c])
       (u/deserialize
        [[nil
          {:a {:d 44}}]])))
  (is (u/isomorphic?
       (d/dissoc-in truncate-this [:a])
       (u/deserialize
        [[nil
          :top]])))
  (is (u/isomorphic?
       (d/dissoc-in truncate-this [])
       (u/deserialize
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

(deftest dissoc-test-2
  (is (u/isomorphic? 
       (d/dissoc-in truncate-this-2 [:a :c :e :g])
       (u/deserialize
        [[nil
          {:a {:c {:e :top
                   :f 43}
               :d 44}
           :b :top}]

         [[[:a :c :e] [:b]]
          :top]])))

  (is (u/isomorphic?
       (d/dissoc-in truncate-this [:a :c :e])
       (u/deserialize
        [[nil
          {:a {:c {:f 43}
               :d 44}}]])))
  (is (u/isomorphic?
       (d/dissoc-in truncate-this [:a :c])
       (u/deserialize
        [[nil
          {:a {:d 44}}]])))
  (is (u/isomorphic?
       (d/dissoc-in truncate-this [:a])
       (u/deserialize
        [[nil
          :top]])))
  (is (u/isomorphic?
       (d/dissoc-in truncate-this [])
       (u/deserialize
        [[nil
          {:a {:c {:e :top
                   :f 43}
               :d 44}
           :b :top}]
         [[[:a :c :e] [:b]]
          {:g 42}]]))))

(def truncate-this-3
  (u/deserialize
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
  (is (u/isomorphic?
       (binding [d/remove-path? (fn [path]
                                  (or
                                   (d/prefix? [:head] path)
                                   (d/prefix? [:1] path)))]
         (d/dissoc-in truncate-this-3 [:head]))
       (u/deserialize
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

