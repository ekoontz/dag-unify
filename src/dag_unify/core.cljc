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
(declare unify!)

(defn unify
  "like unify!, but copy each argument before unifying so that the original inputs' references are not modified."
  [val1 val2]
  (unify! (copy val1) (copy val2)))

(defn unify-dags [dag1 dag2 containing-refs]
  ;; This is the canonical unification case: unifying two DAGs
  ;; (dags with references possibly within them).
  (log/debug (str "unify-dags dag1: " dag1))
  (log/debug (str "           dag2: " dag2))
  (if (not (nil? containing-refs))
    (log/debug (str "          containing-refs: " containing-refs)))
  (->>
   (clojure.set/union (keys dag1)
                      (keys dag2))
   (map (fn [key]
          (let [debug (log/debug (str "unify-dags: working on key: " key))
                value (unify! (key dag1 :top)
                              (key dag2 :top)
                              containing-refs)]
            (log/debug (str "unified value for " key " : " value " with type: " (type value) (if (ref? value) (str " -> " @value))))
            (if (and (ref? value) (some #(= (final-reference-of value) %) containing-refs))
              (let [cycle-detection-message
                    (str "containment failure: "
                         "val: " (final-reference-of value) " is referenced by one of the containing-refs: " containing-refs)]
                (do
                  (log/debug cycle-detection-message)
                  (exception cycle-detection-message))))
            (cond
              (or (= :fail value)
                  (and (ref? value) (= :fail @value)))
              :fail
              true
              {key value}))))

   (reduce (fn [a b]
             (cond (or (= a :fail) (= b :fail))
                   :fail
                   true (merge a b))))))

(defn unify!
  "destructively merge arguments, where arguments are maps possibly containing references, 
   so that sharing relationship in the arguments is preserved in the result"
  [val1 val2 & [containing-refs]]
  (log/debug (str "unify! val1: " val1))
  (log/debug (str "       val2: " val2))
  (if (not (nil? containing-refs))
    (log/debug (str "       containing-refs: " containing-refs)))
  (cond
    (and (map? val1)
         (map? val2))
    (let [result (unify-dags val1 val2 containing-refs)]
      (log/debug (str "unify! result of unifying the 2 dags: " result))
      result)

    (or (= val1 :fail)
        (= val2 :fail))
    :fail
    
    (and (= val1 :top)
         (map? val2))
    (do
      (log/debug (str "unify!: val1 is :top; val2 is a map."))
      (unify-dags val2 {} containing-refs))

    (and (= val2 :top)
         (map? val1))
    (do
      (log/debug (str "unify!: val1 is a map; val2 is  :top"))
      (unify-dags val1 {} containing-refs))
    
    (= val1 :top)
    val2
    
    (= val2 :top)
    val1
    
    ;; expensive if val1 and val2 are not atomic values: the above
    ;; checks should ensure that by now, val1 and val2 are atomic.
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
    
    ;; val2 is a ref, val1 is not a ref:
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

    ;; both val1 and val2 are refs:
    (and
     (ref? val1)
     (ref? val2))
    (do
      ;; set val1 to point to a unification of the values of val1 and val2:
      (swap! val1 (fn [x] (unify! @val1 @val2 (cons val1 (cons val2 containing-refs)))))
      
      ;; set val2 to point to val1, (which is itself a ref):
      (swap! val2 (fn [x] val1))
      val1)
  
    :else
    :fail))

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
          (->> input
               (map (fn [[k v]]
                      (if (not (= k ::refs))
                        [k (copy-with-binding v)])))))
               
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
