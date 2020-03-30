(ns dag_unify.core
  ;; TODO: define a dissoc that works with special values 
  ;; which are not maps but keywords, like :fail; 
  ;; e.g.:
  ;; (dissoc :fail :anykey) => :fail
  ;; (dissoc :top :anykey) => :top
  ;; Another approach would be to not modify dissoc to handle non-maps, and instead
  ;; use special values that *are* maps.
  ;; e.g. {:fail :fail} rather than simply :fail,
  ;; and {:top :top} rather than simply :top.

  (:refer-clojure :exclude [assoc-in get-in])
  (:require
   [clojure.pprint :as core-pprint]
   [clojure.repl :refer [doc]]
   [clojure.string :refer [join]]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [cljslog.core :as log])
   [dag_unify.serialization :refer [all-refs create-path-in
                                    deserialize exception final-reference-of serialize]]))

;; TODO: consider making :fail and :top to be package-local keywords.
;; TODO: use commute to allow faster concurrent access: Rathore, p. 133.

(declare copy)
(declare ref?)
(declare unify!)
(declare vec-contains?)

(defn unify
  "like unify!, but non-destructively copy each argument before unifying."
  [val1 val2]
  (unify! (copy val1) (copy val2)))

(defn unify!
  "destructively merge arguments, where arguments are maps possibly containing references, 
   so that sharing relationship in the arguments is preserved in the result"
  [val1 val2]
  (log/debug (str "val1: " (type val1) "; val2: " (if (keyword? val2) val2 (type val2))))
  (cond
    (and (map? val1)
         (map? val2))
    ;; This is the canonical unification case: unifying two DAGs
    ;; (maps with possible references within them).
    (let [arg1 val1
          arg2 val2
          result
          (loop [arg1 arg1 arg2 arg2 keys-of-arg1 (keys arg1)]
            ;; if keys-of-arg1 is empty, then arg2 contains
            ;; only keys that were *not* in arg1:
            (if (empty? keys-of-arg1)
              arg2
              
              (let [key1 (first keys-of-arg1)
                    result (unify! (key1 arg1 :top)
                                   (key1 arg2 :top))]
                (cond
                  (= :fail result) :fail
                  
                  (and (ref? result)
                       (= :fail @result)) :fail
                  
                  true (recur arg1
                              (merge
                               arg2
                               {key1 result})
                              (rest keys-of-arg1))))))]
      result)
    (or (= val1 :fail)
        (= val2 :fail))
    :fail
    
    (= val1 :top)
    val2
    
    (= val2 :top)
    val1
    
    ;; expensive if val1 and val2 are not atomic values: the above
    ;; checks should ensure that by now val1 and val2 are atomic.
    (= val1 val2) val1
    
    ;; val1 is a ref, val2 is not a ref.
    (and
     (ref? val1)
     (not (ref? val2)))
    (do
      (log/debug (str "case 1."))
      (cond
        (vec-contains? (vec (all-refs val2)) val1)
        (exception (str "containment failure: "
                        " val2: " val2 "'s references contain val1: " val1))
        
        true
        (do (swap! val1
                   (fn [x] (unify! @val1 val2)))
            val1)))
    
    ;; val2 is a ref, val1 is not a ref.
    (and
     (ref? val2)
     (not (ref? val1)))
    (do
      (log/debug (str "case 2: val1 is not a ref; val2 *is* a ref."))
      (cond
        (vec-contains? (vec (all-refs val1)) val2)
        (exception (str "containment failure: "
                        " val1: " val1 "'s references contain val2: " val2))
        true
        (do (swap! val2
                   (fn [x] (unify! val1 @val2)))
            val2)))
    (and
     (ref? val1)
     (ref? val2))
    (do
      (log/debug (str "case 3: both val1 and val2 are refs."))
      (cond
        (= (final-reference-of val1)
           (final-reference-of val2))
        val1
        
        (or (vec-contains? (vec (all-refs @val1)) val2)
            (vec-contains? (vec (all-refs @val2)) val1))
        (exception (str "containment failure: "
                        " val1: " val1 "'s references contain val2: " val2))
        
        (= @val1 val2) ;; val1 -> val2
        val2
        
        :else
        (do
          (swap! val1
                 (fn [x] (unify! @val1 @val2)))
          (swap! val2
                 (fn [x] val1)) ;; note that now val2 is a ref to a ref.
          val1)))
    
    ;; convoluted way of expressing: "if val1 has the form: {:not X}, then .."
    (not (= :notfound (:not val1 :notfound)))
    (if (= val2 :top)
      val1
      ;; else
      (let [result (unify! (:not val1) val2)]
        (if (= result :fail)
          val2
          :fail)))
    
    ;; convoluted way of expressing: "if val2 has the form: {:not X}, then .."
    (not (= :notfound (:not val2 :notfound)))
    (if (= val1 :top)
      val2
      (let [result (unify! val1 (:not val2))]
        (if (= result :fail)
          val1
          :fail)))
    
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
