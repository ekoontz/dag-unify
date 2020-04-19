(ns dag_unify.diagnostics
  (:require
   [clojure.pprint :as core-pprint]
   [clojure.string :refer [join]]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [cljslog.core :as log])
   [dag_unify.core :as u :refer [fail? ref? unify]]))

(defn strip-refs [map-with-refs]
  "return a map like map-with-refs, but without refs - (e.g. {:foo (atom 42)} => {:foo 42}) - used for printing maps in plain (i.e. non html) format"
  (cond
    (or (vector? map-with-refs)
        (seq? map-with-refs))
    (map strip-refs map-with-refs)
    (= map-with-refs {})
    {}
    (map? map-with-refs)
    (let [map-keys (remove #(= % :dag_unify.serialization/serialized)
                           (keys map-with-refs))]
      (zipmap map-keys
              (map #(strip-refs (get map-with-refs %))
                   map-keys)))
    (ref? map-with-refs)
    (strip-refs (deref map-with-refs))
    :else
    map-with-refs))

(defn- pathify
  "Transform a map into a map of paths/value pairs,
  where paths are lists of keywords, and values are atomic values.
  e.g.:
  {:foo {:bar 42, :baz 99}} =>  { { (:foo :bar) 42}, {(:foo :baz) 99} }
  The idea is to map the key :foo to the (recursive) result of pathify on :foo's value."
  [fs & [prefix]]
  (mapcat (fn [kv]
            (let [key (first kv)
                  val (second kv)]
              (if true
                (if (map? val)
                  (pathify val (concat prefix (list key)))
                  (if (and (ref? val)
                           (let [val @val]
                             (map? val)))
                    (pathify @val (concat prefix (list key)))
                    [{(concat prefix (list key))
                      (if (ref? val) @val ;; simply resolve references rather than trying to search for graph isomorphism.
                          val)}])))))
          fs))

(defn fail-path [fs1 fs2]
  (->> (concat (->> (pathify fs1) (map first) (map first))
               (->> (pathify fs2) (map first) (map first)))
       (map (fn [p] {:p p
                     :result (unify (u/get-in fs1 p :top)
                                    (u/get-in fs2 p :top))}))
       (filter #(= :fail (:result %)))
       (map (fn [pair]
              {:fail-path (:p pair)
               :val1 (u/pprint (u/get-in fs1 (:p pair) :top))
               :val2 (u/pprint (u/get-in fs2 (:p pair) :top))}))

       (take 1)
       first))

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
        (and (isomorphic? (get a (first (keys a))) ;; two maps are isomorphic if their keys' values are isomorphic.
                          (get b (first (keys a))))
             (isomorphic? (dissoc a (first (keys a)))
                          (dissoc b (first (keys a)))))
        (and (ref? a)
             (ref? b))
        (isomorphic? @a @b)
        true
        (= a b)))
