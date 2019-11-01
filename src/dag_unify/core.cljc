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

  (:refer-clojure :exclude [assoc-in exists? get-in merge resolve]) ;; TODO: don't override (merge)
  (:require
   [clojure.pprint :as core-pprint]
   [clojure.repl :refer [doc]]
   [clojure.string :refer [join]]
   [clojure.tools.logging :as log]
   [dag_unify.serialization :refer [all-refs create-path-in deserialize exception
                                    serialize]]))

;; use map or pmap.
#?(:clj (def ^:const mapfn map))
#?(:cljs (def ^:const mapfn map))

;; TODO: consider making :fail and :top to be package-local keywords.

;; TODO: many code paths below only look at val1 and val2, and ignore rest of args beyond that.
;; either consider all args, or change signature of (unify) to take only val1 val2.
;; see also lexiconfn/unify (probably will change signature, but make lexiconfn/unify handle
;; have signature [& args] and pass to unify/unify with appropriate translation.
;;
;; TODO: support lazy sequences and vectors
;;
;; TODO: use commute to allow faster concurrent access: Rathore, p. 133.

(declare copy)
(declare merge)
(declare merge-with-keys)
(declare ref?)
(declare simple-unify)
(declare simplify-ref)
(declare unify!)
(declare vec-contains?)

(defn unify
  "like unify!, but non-destructively copy each argument before unifying."
  [& args]
  (let [result
        (apply unify!
               (map (fn [arg]
                      (copy arg))
                    args))]
    (cond (map? result)
          ;; save the serialization so that future copies of this map
          ;; will be faster
          (assoc result :dag_unify.serialization/serialized (serialize result))
          true result)))

(defn unify!
  "destructively merge arguments, where arguments are maps possibly containing references, so that 
   sharing relationship in the arguments is preserved in the result"
  ([val1]
   val1)

  ([val1 val2 & rest-args]
   (log/debug (str "val1: " (type val1) "; val2: " (if (keyword? val2) val2 (type val2))))
   (cond
     ;; This is the canonical unification case: unifying two DAGs
     ;; (maps with possible references within them).
     ;;
     (and (map? val1)
          (map? val2))
     (let [val1 (reduce dissoc val1 dag_unify.serialization/*exclude-keys*)
           result (merge-with-keys
                   val1
                   (reduce dissoc val2 dag_unify.serialization/*exclude-keys*)
                   (keys val1))]
       (log/debug (str "got here: map/map; result: " result))
       (if (empty? rest-args)
         result
         (unify! result
                 (apply unify! rest-args))))
     
     (or (= val1 :fail)
         (= val2 :fail))
     :fail
     
     (and (= val1 :top)
          (empty? rest-args))
     val2
     
     (= val1 :top)
     (apply unify! (cons val2 rest-args))
     
     (and (= val2 :top)
          (not (empty? rest-args)))
     (apply unify! (cons val1 rest-args))
     
     (= val2 :top) val1
     
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
         (throw (Exception. (str "containment failure: "
                                 " val2: " val2 "'s references contain val1: " val1)))
         
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
         (throw (Exception. (str "containment failure: "
                                 " val1: " val1 "'s references contain val2: " val2)))
         true
         (do (swap! val2
                    (fn [x] (unify! val1 @val2)))
             val2)))
     
     (= val1 '())
     :fail
     
     (= val1 nil)
     :fail
     
     (and
      (ref? val1)
      (ref? val2))
     (do
       (log/debug (str "case 3: both val1 and val2 are refs."))
       (cond
         (= (simplify-ref val1)
            (simplify-ref val2))
         val1
         
         (or (vec-contains? (vec (all-refs @val1)) val2)
             (vec-contains? (vec (all-refs @val2)) val1))
         (throw (Exception. (str "containment failure: "
                                 " val1: " val1 "'s references contain val2: " val2)))
         
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
       :fail))))

(defn vec-contains?
  "return true if e is in v, otherwise return false."
  [v e]
  (not (empty? (filter #(= e %) v))))

(defn ref? [val]
  #?(:clj
     (= (type val) clojure.lang.Atom))
  #?(:cljs
     (= (type val) cljs.core.Atom)))

(defn resolve
  "if arg is not a ref, return arg. if is a ref, return (resolve @arg)"
  [arg]
  (if (ref? arg)
    (resolve @arg)
    arg))

(defn simplify-ref
  "if arg is a ref and @arg is not a ref, return arg. if @arg is also a ref, return (simplify-ref @arg). else, return arg."
  [arg]
  (if (ref? arg)
    (if (not (ref? @arg))
      arg
      (simplify-ref @arg))
    (exception (str "simplify-ref was passed a non-ref: " arg " of type: " (type arg)))))

;; TODO: need tests: many tests use (get-in), but need more dedicated tests for it alone.
(defn get-in
  "same as clojure.core (get-in), but references are resolved and followed."
  [in-map path & [not-found]]
  (let [result
        (if (first path)
          (let [result (get in-map (first path) not-found)]
            (if (= result not-found) not-found
                (get-in (resolve result) (rest path) not-found)))
          in-map)]
    (if (ref? val)
      @result
      result)))

(declare fail?)

(defn failr? [fs keys]
  (and (not (empty? keys))
       (or (fail? (get fs (first keys)))
           (failr? fs (rest keys)))))

;; TODO: use multi-methods.
;; TODO: keep list of already-seen references to avoid
;; cost of traversing substructures more than once.
(defn fail?
  "(fail? fs) <=> true if at least one of fs's path's value is :fail."
  [fs]
  (cond (= :fail fs) true
        (seq? fs) false ;; a sequence is never fail.
        (= fs :fail) true ;; :fail is always fail.

        (and (map? fs) (fail? (get-in fs [:fail] :top))) true
        (and (map? fs) (= true (get-in fs [:fail] :top))) true ;; note: :top != true, and (fail? {:fail :top}) => false.

        (fn? fs) false ;; a function is never fail.

        (ref? fs) (fail? @fs)

        (not (map? fs)) false

        :else
        ;; otherwise, check recursively.
        ;; TODO: rewrite using (recur)
        (do
          (cond
            (= fs :fail) true
            (map? fs)
            (failr? fs (keys fs))
            :else false))))

(defn nonfail [maps]
  (filter (fn [each-map]
            (not (fail? each-map)))
          maps))

(defn fail-path-r
  "find the first failing path in a fs."
  [fs & [ fs-keys ] ]
  (if (map? fs)
    (let [fs-keys (if fs-keys fs-keys (keys fs))]
      (if (not (empty? fs-keys))
        (if (fail? (get-in fs (list (first fs-keys))))
          (cons (first fs-keys) (fail-path-r (get-in fs (list (first fs-keys)))))
          (fail-path-r fs (rest fs-keys)))))))

(defn merge-with-keys [arg1 arg2 keys-of-arg1]
  (loop [arg1 arg1 arg2 arg2 keys-of-arg1 keys-of-arg1]
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
                      (clojure.core/merge
                       {key1 result}
                       (dissoc arg2 key1))
                      (rest keys-of-arg1)))))))

(defn deref-map [input]
  input)

(defn pathify
  "Transform a map into a map of paths/value pairs,
  where paths are lists of keywords, and values are atomic values.
  e.g.:
  {:foo {:bar 42, :baz 99}} =>  { { (:foo :bar) 42}, {(:foo :baz) 99} }
  The idea is to map the key :foo to the (recursive) result of pathify on :foo's value."
  [fs & [prefix]]
  (mapcat (fn [kv]
            (let [key (first kv)
                  val (second kv)]
              (if (not (contains? dag_unify.serialization/*exclude-keys* key))
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

(defn create-shared-values [serialized]
  (mapfn (fn [paths-vals]
           (let [val (second paths-vals)]
             ;; TODO: why/why not do copy val rather than just val(?)
             (atom val)))
         serialized))

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

(defn copy [input]
  (let [serialized (serialize input)
        deserialized (deserialize serialized)]
    (if (or (not (map? deserialized))
            (not (= :none (:dag_unify.serialization/serialized deserialized :none))))
      deserialized
      ;; save the serialization so that future copies of this map
      ;; will be faster:
      (assoc deserialized :dag_unify.serialization/serialized serialized))))

(defn label-of [parent]
  (if (:rule parent) (:rule parent) (:comment parent)))

(defn has-path [path paths]
  (if (first paths)
    (if (= (first paths) path)
      true
      (has-path path (rest paths)))))

(defn path-to-ref-index [serialized path n]
  "given serialized form of a map, find the index for _path_. Start with 0."
  ;; TODO: n should be optional and default to 0
  (if (first serialized)
    (let [paths (butlast (first serialized))
          has-path (has-path path (first paths))]
      (if (not (nil? has-path))
        n
        (path-to-ref-index (rest serialized) path (+ n 1))))))

(defn strip-refs [map-with-refs]
  "return a map like map-with-refs, but without refs - (e.g. {:foo (atom 42)} => {:foo 42}) - used for printing maps in plain (i.e. non html) format"
  (cond
    (or (vector? map-with-refs)
        (seq? map-with-refs))
    (map strip-refs map-with-refs)
    (= map-with-refs {})
    {}
    (map? map-with-refs)
    (let [map-keys (sort (keys map-with-refs))]
      (let [first-key (first (keys map-with-refs))
            val (get map-with-refs first-key)]
        (dissoc
         (conj
          {first-key (strip-refs val)}
          (strip-refs (dissoc map-with-refs first-key)))
         :dag_unify.serialization/serialized)))
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
        (let [a (dissoc a :dag_unify.serialization/serialized)
              b (dissoc b :dag_unify.serialization/serialized)]
          (and (isomorphic? (get a (first (keys a))) ;; two maps are isomorphic if their keys' values are isomorphic.
                            (get b (first (keys a))))
               (isomorphic? (dissoc a (first (keys a)))
                            (dissoc b (first (keys a))))))
        (and (ref? a)
             (ref? b))
        (isomorphic? @a @b)
        true
        (= a b)))

;; TODO: use a reduce or recur here rather
;; than simply recursion
(defn find-fail-in [fs1 fs2 paths]
  (if (not (empty? paths))
    (let [path (first paths)
          val1 (get-in fs1 path :top)
          val2 (get-in fs2 path :top)]
      (if (fail? (unify val1 val2))
        {:fail-path (str "/" (join "/" path))
         :val1 (strip-refs val1)
         :val2 (strip-refs val2)}
        (find-fail-in fs1 fs2 (rest paths))))))

(defn fail-path-between
  "If unifying fs1 and fs2 leads to a fail somewhere, show the path to the fail. Otherwise return nil. Not efficient: use only for diagnostics."
  [fs1 fs2]
  (let [paths-in-fs1 (map #(first (first %)) (pathify fs1))
        paths-in-fs2 (map #(first (first %)) (pathify fs2))]
    (find-fail-in fs1 fs2 (concat paths-in-fs1 paths-in-fs2))))

;; shorter alternative to the above.
(defn fail-path [fs1 fs2]
  "if unifying fs1 and fs2 leads to a fail somewhere, show the path to the fail. Otherwise return nil."
  (fail-path-between fs1 fs2))

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

