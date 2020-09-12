(ns dag_unify.diagnostics
  (:require
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [cljslog.core :as log])
   [dag_unify.core :as u :refer [ref? unify]]))

(defn strip-refs
  "return a map like map-with-refs, but without refs - (e.g. {:foo (atom 42)} => {:foo 42}) - used for printing maps in plain (i.e. non html) format"
  [map-with-refs]
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
        :else
        (= a b)))

(defn fail-path [arg1 arg2]
  (binding [u/diagnostics? true]
    (let [result (unify arg1 arg2)]
      (cond (= :fail (:fail result))
            result

            :else
            (do
              (log/warn (str "fail-path: unification succeeded with arguments."))
              nil)))))

