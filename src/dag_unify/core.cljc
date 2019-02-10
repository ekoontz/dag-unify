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
   [clojure.repl :refer [doc]]
   [clojure.pprint :as core-pprint]
   [clojure.string :refer [join]]))

;; use map or pmap.
#?(:clj (def ^:const mapfn map))
#?(:cljs (def ^:const mapfn map))

;; TODO: consider making :fail and :top to be package-local keywords.

(defn exception [error-string]
  #?(:clj
     (throw (Exception. error-string)))
  #?(:cljs
     (throw (js/Error. error-string))))

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

(defn exists? [the-map path]
  (not (= :does-not-exist
          (get-in the-map path :does-not-exist))))

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
          (defn failr? [fs keys]
            (and (not (empty? keys))
                 (or (fail? (get fs (first keys)))
                     (failr? fs (rest keys)))))
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

(declare all-refs)  ;; needed by unify for cycle-checking.
(declare copy)

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
(declare serialize)
(declare simple-unify)
(declare unify!)

(defn unify
  "like unify!, but non-destructively copy each argument before unifying."
  [& args]
  (let [result
        (apply unify!
               (mapfn (fn [arg]
                        (copy arg))
                      args))]
    (cond (map? result)
          ;; save the serialization so that future copies of this map
          ;; will be faster
          (assoc result ::serialized (serialize result))
          true result)))

(defn unifyc
  "alias for (defn unify)"
  [& args]
  (apply unify args))

(def ^:dynamic *exclude-keys* (set #{::serialized}))

(defn unify!
  "destructively merge arguments, where arguments are maps possibly containing references, so that 
   sharing relationship in the arguments is preserved in the result"
  ([val1]
   val1)
  
  ([val1 val2 & rest-args]
   (cond
     ;; This is the canonical unification case: unifying two DAGs
     ;; (maps with possible references within them).
     ;;
     (and (map? val1)
          (map? val2))
     (let [result (merge-with-keys
                   (reduce dissoc val1 *exclude-keys*)
                   (reduce dissoc val2 *exclude-keys*)
                   (filter #(not (contains? *exclude-keys* %)) ;; TODO: rather than filter, simply get keys from dissoc'ed val1 (above)
                           (keys val1)))]
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
       (cond
         (contains? (set (all-refs val2)) val1)
         :fail ;; cannot unify these because it would create a cycle.
         
         true
         (do (swap! val1
                    (fn [x] (unify! @val1 val2)))
             val1)))
     
     ;; val2 is a ref, val1 is not a ref.
     (and
      (ref? val2)
      (not (ref? val1)))
     (do
       (cond
         (contains? (set (all-refs val1)) val2)
         :fail
         true
         (do
           (swap! val2
                  (fn [x] (unify! val1 @val2)))
           val2)))
     
     (= val1 '())
     :fail
     
     (= val1 nil)
     :fail
     
     (and
      (ref? val1)
      (ref? val2))
     (cond
       (= (simplify-ref val1)
          (simplify-ref val2))
       val1
       
       (or (contains? (set (all-refs @val1)) val2)
           (contains? (set (all-refs @val2)) val1))
       :fail
       
       (= @val1 val2) ;; val1 -> val2
       val2
       
       :else
       (do
         (swap! val1
                (fn [x] (unify! @val1 @val2)))
         (swap! val2
                (fn [x] val1)) ;; note that now val2 is a ref to a ref.
         val1))
     
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
     :fail)))

(defn merge-with-keys [arg1 arg2 keys-of-arg1]
  (loop [arg1 arg1 arg2 arg2 keys-of-arg1 keys-of-arg1]
    (let [key1 (first keys-of-arg1)
          result (if (not (empty? keys-of-arg1))
                   (unify! (key1 arg1 :top)
                           (key1 arg2 :top)))]
      (cond

        ;; if keys-of-arg1 is empty, then arg2 contains only keys that
        ;; were *not* in arg1.
        (empty? keys-of-arg1) arg2

        ;; TODO: consider using: (= :fail result) rather than (expensive) (fail?).
        (fail? result) :fail
        true (recur arg1
                    (clojure.core/merge
                     {key1 result}
                     (dissoc arg2 key1))
                    (rest keys-of-arg1))))))

;; TODO: get rid of (dag_unify.core/merge).
(defn- merge
  "warning: {} is the identity value, not nil; that is: (merge X {}) => X, but (merge X nil) => nil, (not X)."
  [& args]
  (if (empty? (rest args)) (first args))
  (let [val1 (first args)
        val2 (second args)]
    (cond
     (= (count args) 1)
     (first args)

     (and (map? val1)
          (map? val2))
     (reduce #(merge-with merge %1 %2) args)

     (and
      (ref? val1)
      (not (ref? val2)))
     (do (swap! val1
                (fn [x] (merge @val1 val2)))
         val1)

     (and
      (ref? val2)
      (not (ref? val1)))
     (do (swap! val2
                (fn [x] (merge val1 @val2)))
         val2)

     (and
      (ref? val1)
      (ref? val2))
     (do (swap! val1
                (fn [x] (merge @val1 @val2)))
         val1)

     (and (= val2 :top)
          (not (= :notfound (:not val1 :notfound))))
     val1

     (not (= :notfound (:not val1 :notfound)))
     (let [result (unify! (:not val1) val2)]
       (if (= result :fail)
         val2
         :fail))

     (and (= val1 :top)
          (not (= :notfound (:not val2 :notfound))))
     val2

     (not (= :notfound (:not val2 :notfound)))
     (let [result (unify! val1 (:not val2))]
        (if (= result :fail)
          val1
          :fail))

     (or (= val1 :fail)
         (= val2 :fail))
     :fail

     (= val1 :top) val2
     (= val2 :top) val1
     (= val1 nil) val2

     ;; note difference in behavior between nil and :nil!:
     ;; (nil is ignored, while :nil! is not).
     ;; (merge 42 nil) => 42
     ;; (merge 42 :nil!) => :nil!
     (= val2 nil) val1
     (= val2 :nil!) val2
     (= val2 "nil!") val2 ;; TODO: remove this if not needed.

     (= val1 val2) val1

     :else ;override with remainder of arguments, like core/merge.
     (apply merge (rest args)))))

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
              (if (not (contains? *exclude-keys* key))
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

(defn find-paths-to-value
  "find all paths in _map_ which are equal to _value_, where _value_ is (ref?)=true."
  [input value path]
  (cond
    (ref? input)
    (cond (= input value) [path] ;; found the value that we were looking for.
          true
          ;; did not find the value, so keep looking within this value.
          (find-paths-to-value @input value path))
    (map? input)
    (apply concat
           (map (fn [key]
                  (find-paths-to-value
                   (get input key)
                   value
                   (concat path [key])))
                (keys input)))))

(defn all-refs [input]
  (cond
    (ref? input)
    (cons
     (if (ref? @input)
       ;; dereference double-references (references to another reference) :
       @input
       ;; a simple reference: reference to a non-reference (e.g. a map, boolean, etc):
       input)
     (all-refs @input))
    (map? input)
    ;; TODO: fix bug here: vals resolves @'s
    (mapcat all-refs
            (mapfn (fn [val]
                     ;; dereference double-references (references to another reference) :
                     ;; note the implicit assumption that the level of indirection is not greater than this.
                     (if (and (ref? val)
                              (ref? @val))
                       @val
                       ;; a simple reference: reference to a non-reference (e.g. a map, boolean, etc):
                       val))
                   (vals input)))))

(defn skeletize [input-val]
  (if (map? input-val)
    (let [sans-serialized (apply dissoc input-val *exclude-keys*)]
      (zipmap (keys sans-serialized)
              (mapfn (fn [val]
                       (if (ref? val)
                         :top
                         (if (map? val)
                           (skeletize val)
                           val)))
                     (vals sans-serialized))))
    input-val))

;; TODO s/map/input-map/
(defn skels
  "create map from reference to their skeletons."
  [input-map refs]
  (let [refs (all-refs input-map)]
    (zipmap
     refs
     (mapfn (fn [ref]
            (skeletize @ref))
          refs))))

(defn ref-skel-map
  "associate each reference in _input-map_ with:
   1. its skeleton
   2. all paths to point to it."
  [^clojure.lang.PersistentHashMap input-map]
  (let [refs (all-refs input-map)
        ;; skels returns a map from a reference to its skeleton.
        skels (skels input-map refs)]
    (zipmap
     ;; associate each ref with its skeleton.
     (mapfn (fn [ref]
             {:ref ref
              :skel (get skels ref)})
           refs)

     ;; list of all paths that point to each ref in _input-map_.
     (mapfn (fn [eachref]
             (find-paths-to-value input-map eachref nil))
           refs))))

;; (((:a :c) (:b :c) (:d))
;;  ((:a) (:b))
;;  nil)
;;     =>
;; {((:a :c) (:b :c) (:d)) => 2
;;  ((:a)    (:b))         => 1
;;  nil                    => 0
;; }
(defn max-lengths [serialization]
  ;; check type (TODO: use multimethods instead)
  (if (= (first (first serialization)) ())
    (exception "Serializing a map failed because one of the map's keys had a sequence as its value. For now, only maps and atoms are supported as values of keys."))
  (let [keys (keys serialization)]
    (zipmap
     keys
     (mapfn (fn [paths]
             (cond (nil? paths) 0
                   (empty? paths) 0
                   true
                   (apply max (mapfn (fn [path] (if (nil? path) 0 (count path)))
                                    paths))))
           keys))))

(defn sort-by-max-lengths [serialization]
  (let [max-lengths (max-lengths serialization)]
    (sort (fn [x y] (< (second x) (second y)))
          max-lengths)))

(defn sort-shortest-path-ascending-r [serialization path-length-pairs]
  (mapfn (fn [path-length-pair]
          (let [paths (first path-length-pair)]
            (list paths
                  (get serialization paths))))
        path-length-pairs))

(defn ser-intermed [input-map]
  (let [top-level (skeletize input-map)
        rsk (ref-skel-map input-map)
        sk (map :skel (keys rsk))]
    (clojure.core/merge
     {nil top-level}
     (zipmap
      (vals rsk)
      sk))))

(defn create-shared-values [serialized]
  (mapfn (fn [paths-vals]
          (let [val (second paths-vals)]
            ;; TODO: why/why not do copy val rather than just val(?)
            (atom val)))
        serialized))
  
(defn create-path-in
  "create a path starting at map through all keys in map:
   (create-path-in '(a b c d e) value) => {:a {:b {:c {:d {:e value}}}}})"  
  [path value]
  (if (first path)
    (if (rest path)
      (let [assigned (create-path-in (rest path) value)]
        {(keyword (first path)) assigned})
      {(first path) value})
    value))

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

;; Serialization format is a sequence:
;; (
;;  paths1 => map1 <= 'base'
;;  paths2 => map2
;;  ..
;; )
;; 'base' is the outermost map 'skeleton' (
;; a 'skeleton' is a map with the dummy placeholder
;; value :top). paths1 is always nil.
;;
;; Note that (deserialize) should be able to cope with
;; both lists and arrays (i.e. just assume a sequence).
(defn deserialize [serialized]
  (let [base (second (first serialized))]
    (apply merge
           (let [all
                 (cons base
                       (flatten
                        (mapfn (fn [paths-val]
                                 (let [paths (first paths-val)
                                       val (atom (second paths-val))]
                                   (mapfn (fn [path]
                                            (create-path-in path val))
                                          paths)))
                               (rest serialized))))]
             all))))

(defn serialize [input-map]
  (let [memoized (get input-map ::serialized :none)]
    (if (not (= memoized :none))
      memoized
      (let [ser (ser-intermed input-map)]
        ;; ser is a intermediate (but fully-serialized) representation
        ;; of the input map, as a map from pathsets to reference-free maps
        ;; (maps which have no references within them).
        
        ;; In place of the references in the original map, the reference-free
        ;; maps have simply a dummy value (the value :top) stored where the
        ;; the reference is in the input-map.
        ;;
        ;; ser:
        ;;
        ;;   pathset    |  value
        ;; -------------+---------
        ;;   pathset1   => value1
        ;;   pathset2   => value2
        ;;      ..         ..
        ;;   nil        => outermost_map
        ;;
        ;; Each pathset is a set of paths to a shared value, the value
        ;; shared by all paths in that pathset.
        ;;
        ;; The last row shown is for the outermost_map that represents
        ;; the entire input, which is why its pathset is nil.
        ;;
        ;; However, ser is not sorted by path length: it needs to be
        ;; sorted so that, when deserialization is done, assignment
        ;; will occur in the correct order: shortest path first.
        
        ;; Thefore, we now sort _ser_ in a shortest-path-first order, so that
        ;; during de-serialization, all assignments will happen in this
        ;; same correct order.

        (sort-shortest-path-ascending-r ser (sort-by-max-lengths ser))))))

(defn copy [input]
  (let [serialized (serialize input)
        deserialized (deserialize serialized)]
    (if (or (not (map? deserialized))
            (not (= :none (::serialized deserialized :none))))
      deserialized
      ;; save the serialization so that future copies of this map
      ;; will be faster:
      (assoc deserialized ::serialized serialized))))

(defn label-of [parent]
  (if (:rule parent) (:rule parent) (:comment parent)))

(defn ref= [map path1 path2]
  "return true iff path1 and path2 point to the same object."
  ;; TODO: add error checking.
  (let [butlast-val1 (get-in map (butlast path1) :none)
        butlast-val2 (get-in map (butlast path2) :none)]
    (and
     (not (= butlast-val1 :none))
     (not (= butlast-val2 :none))
     (= (get butlast-val1 (last path1) :none1)
        (get butlast-val2 (last path2) :none2)))))

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
         ::serialized)))
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
        (let [a (dissoc a ::serialized)
              b (dissoc b ::serialized)]
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
    (core-pprint/pprint (dissoc input ::serialized))
    (ref? input)
    (pprint @input)
    true
    (core-pprint/pprint input)))

