(ns dag_unify.core
  (:refer-clojure :exclude [assoc-in get-in])
  (:require
   [clojure.pprint :as core-pprint]
   [clojure.repl :refer [doc]]
   [clojure.string :refer [join]]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [cljslog.core :as log])
   [dag_unify.serialization :refer [create-path-in deserialize exception
                                    final-reference-of serialize]]))

;; TODO: consider making :fail and :top to be package-local keywords.
;; TODO: use commute to allow faster concurrent access: Rathore, p. 133.

(declare copy)
(declare ref?)
(declare resolve-ref)
(declare unify!)

(defn unify
  "like unify!, but non-destructively copy each argument before unifying."
  [val1 val2]
  (unify! (copy val1) (copy val2)))

(def ^:dynamic exception-if-cycle?
  "If true, and if unifying two DAGs would cause a cycle, thrown an exception. If false,
   return :fail rather than throwing an exception."
  false)

(defn unify-dags [dag1 dag2 containing-refs]
  ;; This is the canonical unification case: unifying two DAGs
  ;; (dags with references possibly within them).
  (let [keys (vec (set (concat (keys dag1) (keys dag2))))
        values
        (map (fn [key]
               (let [value
                     (cond
                       (empty? dag1) (key dag2 :top)
                       (empty? dag2) (key dag1 :top)
                       true
                       (unify! (key dag1 :top)
                               (key dag2 :top)
                               containing-refs))]
                 (if (and (ref? value) (some #(= (final-reference-of value) %) containing-refs))
                   (if exception-if-cycle?
                     (let [cycle-detection-message
                           (str "containment failure: "
                                "val: " (final-reference-of value) " is referenced by one of the containing-refs: " containing-refs)]
                       (exception cycle-detection-message))
                     :fail)
                   value)))
             keys)]
    (if (some #(= (resolve-ref %) :fail)
              values)
      :fail
      (zipmap
       keys
       values))))

(defn unify!
  "destructively merge arguments, where arguments are maps possibly containing references, 
   so that sharing relationship in the arguments is preserved in the result"
  [val1 val2 & [containing-refs]]
  (log/debug (str "val1: " (type val1) "; val2: " (if (keyword? val2) val2 (type val2))))
  (cond
    (and (map? val1)
         (map? val2))
    (unify-dags val1 val2 containing-refs)

    (and (= val1 :top)
         (map? val2))
    (unify-dags val2 nil containing-refs)

    (and (= val2 :top)
         (map? val1))
    (unify-dags val1 nil containing-refs)

    (= val1 :top)
    val2
    
    (= val2 :top)
    val1
    
    ;; expensive if val1 and val2 are not atomic values: the above
    ;; checks should ensure that by now, val1 and val2 are atomic:
    (= val1 val2) val1

    (and
     (ref? val1)
     (= :fail @val1))
    :fail

    (and
     (ref? val2)
     (= :fail @val2))
    :fail
    
    ;; val1 is a ref, val2 is not a ref:
    (and
     (ref? val1)
     (not (ref? val2)))
    (do (swap! val1
               (fn [x]
                 (unify! @val1 val2 (cons val1 containing-refs))))
        val1)
    
    ;; val2 is a ref, val1 is not a ref.
    (and
     (ref? val2)
     (not (ref? val1)))
    (do (swap! val2
               (fn [x]
                 (unify! val1 @val2 (cons val2 containing-refs))))
        val2)

    ;; both val1 and val2 are refs, and point (either directly or indirectly) to the same value:
    (and
     (ref? val1)
     (ref? val2)
     (= (final-reference-of val1)
        (final-reference-of val2)))
    val1

    (and
     (ref? val1)
     (ref? val2))
    (do
      (swap! val1
             (fn [x] (unify! @val1 @val2 (cons val1 (cons val2 containing-refs)))))
      (swap! val2
             (fn [x] val1)) ;; note that now val2 is a ref to a ref.
      val1)

    :else
    (do
      (log/debug (str "unify! else case."))
      :fail)))

(defn vec-contains?
  "return true if e is in v, otherwise return false."
  [v e]
  (not (empty? (filter #(= e %) v))))

(defn ref? [val]
  #?(:clj
     (= (type val) clojure.lang.Atom))
  #?(:cljs
     (= (type val) cljs.core/Atom)))

(defn resolve-ref
  "if arg is not a ref, return arg. if is a ref, return (resolve @arg)"
  [arg]
  (if (ref? arg)
    (resolve-ref @arg)
    arg))

;; TODO: need tests: many tests use (get-in), but need more dedicated tests for it alone.
(defn get-in
  "same as clojure.core (get-in), but references are resolved and followed."
  [in-map path & [not-found]]
  (let [result
        (if (first path)
          (let [result (get in-map (first path) not-found)]
            (if (= result not-found) not-found
                (get-in (resolve-ref result) (rest path) not-found)))
          in-map)]
    (resolve-ref result)))

(defn assoc-in
  "Similar to clojure.core/assoc-in, but uses unification so that existing values are unified rather than overwritten."
  [m path v]
  (unify m
         (create-path-in path v)))

(defn assoc-in!
  "Similar to assoc-in, but m's references (if any) are modified."
  [m path v]
  (unify! m
          (create-path-in path v)))

(def ^:dynamic old2new)
(declare copy-with-binding)
(defn copy [input]
  (binding [old2new (atom {})]
    (copy-with-binding input)))

(defn copy-with-binding [input]
  (cond
    (ref? input)
    (let [entry-if-any
          (get @old2new (final-reference-of input))]
      ;; input is a ref that we have seen _input_ before, so return the new
      ;; already-created copy of _input_:
      (or entry-if-any
          ;; input is a ref that we have *not* seen before, so:
          ;; 1. create a new ref:
          (let [input (final-reference-of input)
                new-input (atom nil)]
            ;; 2. update the old-to-new map with this new ref:
            (swap! old2new
                   (fn [x] (assoc x input new-input)))
            ;; set the value of the new ref, based on the existing _input_:
            (swap! new-input
                   (fn [x] (copy-with-binding @input)))
            ;; and return the new ref:
            new-input)))

    (map? input)
    (into {}
          (map (fn [[k v]]
                 [k (copy-with-binding v)])
              input))

    ;; simply an atomic value; nothing needed but to return the original atomic value:
    true input))

(defn fail? [arg]
  (= :fail arg))

(defn pprint [input]
  (cond
    (or (true? input)
        (false? input)
        (string? input)
        (keyword? input)
        (number? input)
        (empty? input))
    input
    (map? input)
    (core-pprint/pprint (dissoc input :dag_unify.serialization/serialized))
    (ref? input)
    (pprint @input)
    true
    (core-pprint/pprint input)))

