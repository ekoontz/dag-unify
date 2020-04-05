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

(declare all-refs)
(declare copy)
(declare ref?)
(declare unify!)

(defn unify
  "like unify!, but non-destructively copy each argument before unifying."
  [val1 val2]
  (unify! (copy val1) (copy val2)))

(defn unify-dags [dag1 dag2]
  ;; This is the canonical unification case: unifying two DAGs
  ;; (dags with references possibly within them).
  (loop [dag1 dag1
         dag2 dag2
         keys-of-dag1 (keys dag1)]
    ;; if keys-of-dag1 is empty, then dag2 is a dag containing
    ;; only keys that were *not* in dag1:
    (if (empty? keys-of-dag1)
      dag2
      
      (let [key (first keys-of-dag1)
            value (unify! (key dag1 :top)
                          (key dag2 :top))]
        (if (= :fail value)
          :fail
          (recur (dissoc dag1 key)
                 (merge
                  (dissoc dag2 key)
                  {key value})
                 (rest keys-of-dag1)))))))

(defn unify!
  "destructively merge arguments, where arguments are maps possibly containing references, 
   so that sharing relationship in the arguments is preserved in the result"
  [val1 val2]
  (log/debug (str "val1: " (type val1) "; val2: " (if (keyword? val2) val2 (type val2))))
  (cond
    (and (map? val1)
         (map? val2))
    (unify-dags val1 val2)

    (or (= val1 :fail)
        (= val2 :fail))
    :fail
    
    (= val1 :top)
    val2
    
    (= val2 :top)
    val1
    
    ;; expensive if val1 and val2 are not atomic values: the above
    ;; checks should ensure that by now, val1 and val2 are atomic.
    (= val1 val2) val1

    ;; val1 is a ref, val2 is not a ref:
    (and
     (ref? val1)
     (not (ref? val2)))
    (cond
      (some #(= val1 %) (all-refs val2))
      (exception (str "containment failure (OLD): "
                      " val2: " val2 "'s references contain val1: " val1))
      true
      (do (swap! val1
                 (fn [x]
                   (unify! @val1 val2)))
          val1))
    
    ;; val2 is a ref, val1 is not a ref:
    (and
     (ref? val2)
     (not (ref? val1)))
    (cond
      (some #(= val2 %) (all-refs val1))
      (exception (str "containment failure: "
                      " val1: " val1 "'s references contain val2: " val2))
      true
      (do (swap! val2
                 (fn [x]
                   (unify! val1 @val2)))
          val2))
    
    ;; both val1 and val2 are refs:
    (and
     (ref? val1)
     (ref? val2))
    (cond
      (= (final-reference-of val1)
         (final-reference-of val2))
      val1
      
      (or (some #(= val2 %) (all-refs @val1))
          (some #(= val1 %) (all-refs @val2)))
      (exception (str "containment failure: "
                      " val1: " val1 "'s references contain val2: " val2))
      :else
      (do
        
        ;; set val1 to point to a unification of the values of val1 and val2:
        (swap! val1
               (fn [x]
                 (unify! @val1 @val2)))
        
        ;; set val2 to point to val1, (which is itself a ref):
        (swap! val2
               (fn [x] val1))
        
        val1))
  
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

(def ^:dynamic found-refs nil)
(declare all-refs-with-binding)

(defn all-refs [input]
  (cond
    (and (map? input) (::refs input))
    (do
      (log/info (str "returning saved refs: " (::refs input)))
      (::refs input))
    true
    (binding [found-refs (atom (set nil))]
      (all-refs-with-binding input))))

(defn- all-refs-with-binding [input]
  (cond
    (and (ref? input)
         (contains? @found-refs input))
    []

    (ref? input)
    (let [input (final-reference-of input)]
      (swap! found-refs
             (fn [x]
               (conj @found-refs input)))
      (cons input (all-refs-with-binding @input)))

    (and (map? input) (empty? input))
    []

    (map? input)
    ;; get refs for the first key's value:
    (concat (all-refs-with-binding (second (first input)))
            ;; ..and refs for the remaining keys' values:
            (all-refs-with-binding (dissoc input (first (first input)))))
    true
    []))
