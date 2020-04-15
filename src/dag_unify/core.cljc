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

(defn unify-dags
  "Unify two maps by taking the union of their keys and for each
   _k_ in this union, call unify! on the values of _k_ for each map."
  [dag1 dag2 containing-refs]
  ;; This is the canonical unification case: unifying two DAGs
  ;; (dags with references possibly within them).
  (let [keys (vec (set (concat (keys dag1) (keys dag2))))
        values
        (map (fn [key]
               (let [value
                     (unify! (key dag1 :top)
                             (key dag2 :top)
                             containing-refs)
                     final-ref (if (ref? value) (final-reference-of value))]
                 (cond (and final-ref (some #(= final-ref %) containing-refs))
                       (if exception-if-cycle?
                         (let [cycle-detection-message
                               (str "containment failure: "
                                    "val: " final-ref " is referenced by one of the containing-refs: " containing-refs)]
                           (exception cycle-detection-message))
                         :fail)
                       (and final-ref (= @final-ref :fail))
                       :fail
                       true
                       value)))
             keys)]
    (if (some #(= % :fail) values)
      :fail
      (zipmap
       keys
       values))))

(defn unify!
  "merge input arguments val1 and val2. if val1 and val2 are maps, merge recursively (via unify-dags).
   if val1 and/or val2 are references (Atoms), the references(s) will be updated to point to a new, unified value
   by resolving the references.
   if val1 and/or val2 are atomic values (e.g. strings, numbers, etc), the unification is
   their equal value if they are equal, or :fail if they are not equal."
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

(def ref-counter (atom 0))
(def ^:dynamic ref2counter-value)
(declare pprint-with-binding)
(defn pprint [input]
  (binding [ref2counter-value (atom {})]
    (swap! ref-counter (fn [x] 0))
    ;; doing a (deserialize (serialize)) on the input to remove
    ;; singleton references (references which are only used once in the input),
    ;; which clutters up the displayed output unnecessarily.
    (pprint-with-binding (deserialize (serialize input)))))

(defn pprint-with-binding [input]
  (cond
    (ref? input)
    (let [entry-if-any
          (get @ref2counter-value (final-reference-of input))]
      ;; input is a ref that we have seen _input_ before, so return the new
      ;; already-created pprint of _input_:
      (if entry-if-any
        [entry-if-any]

        ;; else:
        ;; input is a ref that we have *not* seen before, so:
        ;; 1. create a new ref:
        (let [input (final-reference-of input)
              increment (swap! ref-counter (fn [x] (+ 1 x)))]
          (swap! ref2counter-value
                 (fn [x] (assoc x input @ref-counter)))
          [[@ref-counter] (pprint-with-binding @input)])))

    (map? input)
    (into {}
          (map (fn [[k v]]
                 [k (pprint-with-binding v)])
              input))

    ;; simply an atomic value; nothing needed but to return the original atomic value:
    true input))
