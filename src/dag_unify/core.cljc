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
  (:refer-clojure :exclude [exists? get-in merge resolve])
  (:require
   [clojure.set :refer [intersection subset? union]]
   [clojure.string :as string]))

;; use map or pmap.
#?(:clj (def ^:const mapfn pmap))
#?(:cljs (def ^:const mapfn map))

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

(defn resolve [arg]
  "if arg is not a ref, return arg. if is a ref, return (resolve @arg)"
  (if (ref? arg)
    (resolve @arg)
    arg))

;; TODO: need tests: many tests use (get-in), but need more dedicated tests for it alone.
(defn get-in [in-map path & [not-found]]
  "same as clojure.core (get-in), but it resolves references if need be."
  (cond (seq? in-map)
        (map (fn [each]
               (get-in each path not-found))
             in-map)
        true
        (let [result
              (if (first path)
                (let [result (get in-map (first path) not-found)]
                  (if (= result not-found) not-found
                      (get-in (resolve result) (rest path) not-found)))
                in-map)]
          (if (ref? val)
            @result
            result))))

(defn exists? [the-map path]
  (not (= :does-not-exist
          (get-in the-map path :does-not-exist))))

;; TODO: use multi-methods.
;; TODO: keep list of already-seen references to avoid
;; cost of traversing substructures more than once.
(defn fail? [fs]
  "(fail? fs) <=> true if at least one of fs's path's value is :fail."
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

(defn fail-path-r [fs & [ fs-keys ] ]
  "find the first failing path in a fs."
  (if (map? fs)
    (let [fs-keys (if fs-keys fs-keys (keys fs))]
      (if (not (empty? fs-keys))
        (if (fail? (get-in fs (list (first fs-keys))))
          (cons (first fs-keys) (fail-path-r (get-in fs (list (first fs-keys)))))
          (fail-path-r fs (rest fs-keys)))))))

(defn any? [fn members]
  (if (not (empty? members))
    (or (fn (first members))
        (any? fn (rest members)))

    ;; members is empty.
    false))

(declare expand-disj) ;; needed by unify.
(declare copy)
(declare unifyc)

;; TODO: many code paths below only look at val1 and val2, and ignore rest of args beyond that.
;; either consider all args, or change signature of (unify) to take only val1 val2.
;; see also lexiconfn/unify (probably will change signature, but make lexiconfn/unify handle
;; have signature [& args] and pass to unify/unify with appropriate translation.
;;
;; TODO: support lazy sequences and vectors
;;
;; TODO: use commute to allow faster concurrent access: Rathore, p. 133.

(declare merge)
(declare merge-with-keys)
(declare simple-unify)

(declare copy)
(declare unify)

(defn unifyc [& args]
  "like fs/unify, but fs/copy each argument before unifying."
  (apply unify
         (mapfn (fn [arg]
                 (copy arg))
               args)))

(defn unify [& args]
  (let [val1 (first args)
        val2 (second args)]
    (cond
      ;; This is the canonical unification case: unifying two DAGs
      ;; (maps with possible references within them).
      ;;
      (and (map? val1)
           (map? val2))
      (let [result (merge-with-keys val1 val2 (keys val1))]
        (if (empty? (rest (rest args)))
          result
          (unify result
                 (apply unify (rest (rest args))))))
                       
      (= (count args) 1)
      (first args)
           
      (or (= val1 :fail)
          (= val2 :fail))
      :fail

      (= val1 :top) val2
      (= val2 :top) val1

      (= val1 val2) val1

      (= val1 '())
      :fail

      (= val1 nil)
      :fail

      (nil? args) nil
           
      ;; val1 is a ref, val2 is not a ref.
      (and
       (ref? val1)
       (not (ref? val2)))
      (do
        (swap! val1
               (fn [x] (unify @val1 val2)))
        val1)
           
      ;; val2 is a ref, val1 is not a ref.
      (and
       (ref? val2)
       (not (ref? val1)))
      (do
        (swap! val2
               (fn [x] (unify val1 @val2)))
        val2)

      (and
       (ref? val1)
       (ref? val2))
      (cond
        (or (= val1 val2) ;; same reference.
            (= val1 @val2)) ;; val1 <- val2
        val1

        (= @val1 val2) ;; val1 -> val2
        val2
              
        :else
        (do
          (swap! val1
                 (fn [x] (unify @val1 @val2)))
          (swap! val2
                 (fn [x] val1)) ;; note that now val2 is a ref to a ref.
          val1))

      ;; convoluted way of expressing: "if val1 has the form: {:not X}, then .."
      (not (= :notfound (:not val1 :notfound)))
      (if (= val2 :top)
        val1
        ;; else
        (let [result (unify (:not val1) val2)]
          (if (= result :fail)
            val2
            :fail)))
           
      ;; convoluted way of expressing: "if val2 has the form: {:not X}, then .."
      (not (= :notfound (:not val2 :notfound)))
      (if (= val1 :top)
        val2
        (let [result (unify val1 (:not val2))]
          (if (= result :fail)
            val1
            :fail)))
      
      :else
      :fail)))

(defn merge-with-keys [arg1 arg2 keys1]
  (if (not (empty? keys1))
    (let [key1 (first keys1)
          result (unify (key1 arg1 :top)
                        (key1 arg2 :top))]
      (if (fail? result)
        :fail
        (let [rest-result
              (merge-with-keys arg1
                               (dissoc arg2 key1)
                               (rest keys1))]
          (if (fail? rest-result)
            :fail
            (clojure.core/merge {key1 result}
                                rest-result)))))
    arg2))

(defn merge [& args]
  "warning: {} is the identity value, not nil; that is: (merge X {}) => X, but (merge X nil) => nil, (not X)."
  (if (empty? (rest args)) (first args))
  (let [val1 (first args)
        val2 (second args)]
    (cond

     (set? val1)
     (set (filter (fn [each]
                    (not (fail? each)))
                  (reduce union
                          (map (fn [each]
                                 (let [result (merge (copy each) (copy val2))]
                                   (cond (set? result)
                                         result
                                         (seq? result)
                                         (set result)
                                         true
                                         (set (list result)))))
                               val1))))

     (set? val2)
     (set (filter (fn [each]
                    (not (fail? each)))
                  (reduce union
                          (map (fn [each]
                                 (let [result (merge (copy each) (copy val1))]
                                   (cond (set? result)
                                         result
                                         (seq? result)
                                         (set result)
                                         true
                                         (set (list result)))))
                               val2))))

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
     (let [result (unify (:not val1) val2)]
       (if (= result :fail)
         val2
         :fail))

     (and (= val1 :top)
          (not (= :notfound (:not val2 :notfound))))
     val2

     (not (= :notfound (:not val2 :notfound)))
     (let [result (unify val1 (:not val2))]
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

;; TODO: remove *exclude-keys*,(pathify-r) and (pathify) in favor of fs's versions.
(def ^:dynamic *exclude-keys* (set #{:_id :ref :refmap}))

(defn pathify-r [fs & [prefix]]
"Transform a map into a map of paths/value pairs,
 where paths are lists of keywords, and values are atomic values.
 e.g.:
 {:foo {:bar 42, :baz 99}} =>  { { (:foo :bar) 42}, {(:foo :baz) 99} }
The idea is to map the key :foo to the (recursive) result of pathify on :foo's value."
  (mapcat (fn [kv]
            (let [key (first kv)
                  val (second kv)]
;              (println (str "K:" key))
              (if (not (contains? *exclude-keys* key))
                (if (map? val)
                  (do
;                    (println (str "PAM"))
                    (pathify-r val (concat prefix (list key))))
                  (if (and (ref? val)
                           (let [val @val]
                             (map? val)))
                    (pathify-r @val (concat prefix (list key)))
                  (do
;                    (println (str "not PAM" (type val)))
                    (list {(concat prefix (list key))
                           (if (ref? val) @val ;; simply resolve references rather than trying to search for graph isomorphism.
                               val)})))))))
          fs))

(defn pathify [fs]
  (pathify-r fs))

(def uniq-using-recur
  "remove duplicates by checking first and second: if equal, remove the first and keep the second. otherwise, keep both."
  (fn [sorted-vals]
    (loop [sv sorted-vals result nil]
      (let [first-val (first sv)]
        (if (nil? (first sv))
          result
          (let [second-val (second sv)]
            (if (= first-val second-val)
              (recur (rest sv)
                     result)
              (recur (rest sv)
                     (cons first-val result)))))))))

(defn uniq [sorted-vals]
  (reverse (uniq-using-recur sorted-vals)))

(defn paths-to-value [map value path]
  (cond
    (= map value) (list path)
    (ref? map) (paths-to-value @map value path)
    (map? map) (mapcat (fn [key]
                         (paths-to-value (get map key) value (concat path (list key))))
                       (keys map))))
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
    (concat
     (mapcat (fn [key]
               (let [val (get input key)]
                 (if (ref? input)
                   (if (ref? @val)
                     (list @val)
                     (list val)))))
             input)
     (all-refs
      (mapfn (fn [val]
              ;; dereference double-references (references to another reference) :
              (if (and (ref? val)
                       (ref? @val))
                @val
                ;; a simple reference: reference to a non-reference (e.g. a map, boolean, etc):
               val))
           (vals input))))
    (seq? input)
    (mapcat (fn [each-input]
              (all-refs each-input))
            input)))

(defn skeletize [input-val]
  (if (map? input-val)
    (zipmap (keys (dissoc input-val :serialized))
            (mapfn (fn [val]
                    (if (ref? val)
                      :top
                      (if (map? val)
                        (skeletize val)
                        val)))
                  (vals (dissoc input-val :serialized))))
    input-val))

;; TODO s/map/input-map/
;; TODO: merge or distinguish from all-refs (above)
(defn get-refs [input-map]
  (all-refs input-map))

;; TODO s/map/input-map/
(defn skels [input-map refs]
  "create map from reference to their skeletons."
  (let [refs (get-refs input-map)]
    (zipmap
     refs
     (mapfn (fn [ref]
            (skeletize @ref))
          refs))))

(defn ref-skel-map [input-map]
  "associate each reference in _input-map_ with:
   1. its skeleton
   2. all paths to point to it."
  (let [refs (get-refs input-map)
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
             (paths-to-value input-map eachref nil))
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
  (cond (set? input-map)
        (set (map (fn [each]
                    (ser-intermed each))
                  input-map))
        true
        (let [top-level (skeletize input-map)
              rsk (ref-skel-map input-map)
              sk (mapfn (fn [ref-skel]
                         (:skel ref-skel))
                       (keys rsk))]
          (clojure.core/merge
           {nil top-level}
           (zipmap
            (vals rsk)
            sk)))))

(defn create-shared-values [serialized]
  (mapfn (fn [paths-vals]
          (let [val (second paths-vals)]
            ;; TODO: why/why not do copy val rather than just val(?)
            (atom val)))
        serialized))

(defn create-path-in [path value]
  "create a path starting at map through all keys in map:
   (create-path-in '(a b c d e) value) => {:a {:b {:c {:d {:e value}}}}})"
  (if (first path)
    (if (rest path)
      (let [assigned (create-path-in (rest path) value)]
        {(keyword (first path)) assigned})
      {(first path) value})
    value))

;; Serialization format is a sequence:
;; (
;;  paths1 => map1 <= 'base'
;;  paths2 => map2
;;  ..
;; )
;; 'base' is the outermost map 'skeleton' (
;; a 'skeleton' is a map with the dummy placeholder
;; value :top).
;;
;; Note that (deserialize) should be able to cope with
;; both lists and arrays (i.e. just assume a sequence).
(defn deserialize [serialized]
  (cond (set? serialized)
        (set (map (fn [each]
                    (deserialize each))
                  serialized))
        true (let [base (second (first serialized))]
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
                        all)))))

(defn recursive-dissoc [a-map pred]
  "like dissoc, but works recursively. Only works on reference-free maps (contains no atoms)."
  (if (not (empty? a-map))
    (let [k (first (first a-map))
          v (second (first a-map))]
      (if (pred k)
        (recursive-dissoc (dissoc a-map k)
                          pred)
        (conj
         {k (cond (map? v)
                  (recursive-dissoc v pred)
                  true v)}
         (recursive-dissoc (dissoc a-map k)
                           pred))))
    {}))

(defn deserialize-with-remove [serialized pred]
  (cond (set? serialized)
        (set (map (fn [each]
                    (deserialize each))
                  serialized))
        true (let [base (recursive-dissoc (second (first serialized)) pred)]
               (apply merge
                      (let [all
                            (cons base
                                  (flatten
                                   (map (fn [paths-val]
                                          (let [paths (first paths-val)
                                                val (atom
                                                     (cond (map? atom)
                                                           (recursive-dissoc
                                                            (second paths-val)
                                                            pred)
                                                           true
                                                           (second paths-val)))]
                                            (map (fn [path]
                                                   (if (empty?
                                                        (remove false?
                                                                (map (fn [key-in-path]
                                                                       (pred key-in-path))
                                                                     path)))
                                                     (create-path-in path val)))
                                                 paths)))
                                        (rest serialized))))]
                        all)))))

(defn serialize [input-map]
  (cond
   (set? input-map)
   (set (map (fn [each]
               (serialize each))
             input-map))
   true
   (let [memoized (get input-map :serialized :none)]
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

         (sort-shortest-path-ascending-r ser (sort-by-max-lengths ser)))))))

(defn copy [input]
  (cond (seq? input)
        (map (fn [each]
               (copy each))
             input)
        true
        (deserialize (serialize input))))

(defn trunc [serialized]
  "create a new serialized map with all paths removed that are non-immediate."
  (map (fn [paths-skel-pair]
         (let [paths (first paths-skel-pair)
               skel (second paths-skel-pair)]
           (let [filtered-paths
                 (filter (fn [path]
                           (let [take-2 (take 2 path)]
                             (and (not (= take-2
                                          '(:head :head)))
                                  (not (= take-2
                                          '(:comp :comp)))
                                  (not (= take-2
                                          '(:head :comp)))
                                  (not (= take-2
                                          '(:comp :2)))
                                  (not (= take-2
                                          '(:head :1)))
                                  (not (= take-2
                                          '(:head :2)))
                                  (not (= take-2
                                          '(:1 :1)))
                                  (not (= take-2
                                          '(:1 :comp)))
                                  (not (= take-2
                                          '(:1 :head)))
                                  (not (= take-2
                                          '(:1 :2)))
                                  (not (= take-2
                                          '(:2 :2)))
                                  (not (= take-2
                                          '(:2 :1)))
                                  (not (= take-2
                                          '(:2 :head)))
                                  (not (= take-2
                                          '(:2 :comp))))))
                         paths)]
           (list filtered-paths skel))))
       serialized))

(defn copy-trunc [map]
  (deserialize (trunc (serialize map))))

(defn has-path [path paths]
  (if (first paths)
    (if (= (first paths) path)
      true
      (has-path path (rest paths)))))

(defn path-to-ref-index [serialized path n]
  "given serialized form of a map, find the index for _path_. Start with 0."
  (if (first serialized)
    (let [paths (butlast (first serialized))
          has-path (has-path path (first paths))]
      (if (not (nil? has-path))
        n
        (path-to-ref-index (rest serialized) path (+ n 1))))))

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
       (conj
        {first-key (strip-refs val)}
        (strip-refs (dissoc map-with-refs first-key)))))
   (ref? map-with-refs)
   (strip-refs (deref map-with-refs))
   :else
   map-with-refs))

(defn remove-top-values [fs]
  "Use case is logging where we don't care about uninformative key->value pairs where value is simply :top. Also strips refs for readability."
  (cond

   (= fs {})
   {}

   (map? fs)
   (let [map-keys (sort (keys fs))]
     (let [first-key (first (keys fs))
           val (get fs first-key)]
       (cond
        (and (not (= first-key :1)) 
             (not (= first-key :2)) 
             (not (= first-key :3))
             (= val :top)) ;; remove-top-values: core action of this function.
        (remove-top-values (dissoc fs first-key))

        (= first-key :comp-filter-fn) ;; TODO: deprecate and remove comp-filter-fn.
        (remove-top-values (dissoc fs first-key))

         ;; else, KV is not :top, so keep it.
        true
        (conj
         {first-key (remove-top-values val)}
         (remove-top-values (dissoc fs first-key))))))

   (ref? fs)
   ;; strip refs for readability.
   (remove-top-values (deref fs))

   :else
   fs))

(defn remove-matching-keys [fs pred]
  (let [serialized (serialize fs)]
    (deserialize-with-remove serialized pred)))

(defn remove-top-values-log [fs]
  (let [result (remove-top-values fs)]
    ;; TODO: should not need to re-call this: workaround for the fact that remove-top-values doesn't work correctly,
    ;; but does seem to work correctly if called again on its own output.
    (remove-top-values result)))

(defn refset2map [fs]
  "Turn every ref to a set into a map with two keys: :ref and :val."
  (cond

   (set? fs)
   (set (map (fn [each]
               (refset2map each))
             fs))

   (and (ref? fs)
        (set? @fs))
   (set (map (fn [each]
               {:val (refset2map each)
                :ref fs})
             @fs))

   (ref? fs)
   {:val (refset2map @fs)
    :ref fs}

   (and (map? fs)
        (not (empty? fs)))
   (let [key (first (first fs))
         val (key fs)]
     (conj
      {key (refset2map val)}
      (refset2map (dissoc fs key))))

   true
   fs))

(defn get-all-ref-tuples [fs & [path]]
  "returns list of ref:val:path tuples."
  (let [path (if path path nil)]
    (cond
     (and (map? fs)
          (not (empty? fs))
          (:ref fs))
     (list {:ref (:ref fs)
            :val (:val fs)
            :path path})
     (and (map? fs)
          (not (empty? fs)))
     (let [key (first (first fs))
           val (key fs)]
       (concat
        (get-all-ref-tuples val (concat path (list key)))
        (get-all-ref-tuples (dissoc fs key) path)))
     true nil)))

(defn get-all-refs-for [fs]
  "get the set of refs in a normalized fs"
  (apply union
         (map (fn [tuple]
                (set (list (:ref tuple))))
              (get-all-ref-tuples fs))))

(defn cartesian [set1 set2]
  "for x in set1, y in set2, conj each x and each y"
  (cond (empty? set1) set2
        (empty? set2) set1
        true
        (apply union
               (map (fn [each-map-in-set-1]
                      (set (map (fn [each-map-in-set-2]
                                  (conj each-map-in-set-1 each-map-in-set-2))
                                set2)))
                    set1))))
(defn get-unified-value-for [fs ref]
  "get all values to be unified for the given ref in the given normalized fs."
  (reduce unify
          (apply concat
                 (map (fn [tuple]
                        (let [tuple-ref (:ref tuple)]
                          (if (= tuple-ref ref)
                            (list (:val tuple)))))
                      (get-all-ref-tuples fs)))))

(defn copy-with-ref-substitute [fs old-ref new-ref]
  "create new fs, but with new-ref substituted for every occurance of {:ref ref,:val X}"
  (cond
   (and (map? fs)
        (not (empty? fs))
        (not (nil? (:ref fs)))
        (= (:ref fs) old-ref))
   new-ref

   (and (map? fs)
        (not (empty? fs)))
   (let [key (first (first fs))
         val (key fs)]
     (conj {key (copy-with-ref-substitute val old-ref new-ref)}
           (copy-with-ref-substitute (dissoc fs key) old-ref new-ref)))
   true
   fs))

(defn copy-with-assignments [fs assignments]
  (if (not (empty? assignments))
    (let [assignment (first assignments)
          old-ref (:ref assignment)
          new-ref (atom (:val assignment))]
      (copy-with-assignments
       (copy-with-ref-substitute fs old-ref new-ref)
       (rest assignments)))
    fs))

(defn dissoc-paths [fs & [paths]]
  "dissoc a path from a map; e.g.: (dissoc-paths {:a {:b 42 :c 43}} '(:a :b)) => {:a {:c 43}}."
  (do
    (cond (empty? paths)
          fs

          (seq? fs)
          (map #(dissoc-paths % paths) fs)

          (ref? fs)
          (dissoc-paths @fs paths)

          (keyword? fs)
          fs

          (empty? fs)
          :top

          (seq? fs)
          (cons (dissoc-paths (first fs))
                (dissoc-paths (rest fs)))

          true
          (let [path (first paths)]
            (dissoc-paths
             (cond (keyword fs)
                   fs
                   (and (map? fs)
                        (not (empty? fs))
                        (not (empty? path)))
                   (let [feature (first path)]
                     (cond (ref? fs)
                           (dissoc-paths @fs (list path))
                           (map? fs)
                           (cond
                            (and
                             (empty? (rest path))
                             (empty? (dissoc fs feature)))
                            :top

                            (empty? (rest path))
                            (dissoc fs feature)

                            (not (= :notfound (get-in fs (list feature) :notfound)))
                            (conj
                             {feature (dissoc-paths (get-in fs (list feature)) (list (rest path)))}
                             (dissoc fs feature))

                            true
                            (dissoc-paths fs (rest paths)))))

                   true
                   (exception
                    (str "dissoc-paths: don't know what to do with this input argument (fs): "
                         fs)))
             (rest paths))))))

(defn remove-false [spec]
  (cond (map? spec)
        (into {}
              (map (fn [key]
                     (let [val (get-in spec (list key))]
                       (if (not (= val false))
                         [key (remove-false val)])))
                   (keys spec)))
        
        (seq? spec)
        (map (fn [each]
               (remove-false each))
             spec)
        (ref? spec)
        (remove-false @spec)

        true
        spec))

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

(defn label-of [parent]
  (if (:rule parent) (:rule parent) (:comment parent)))

;; TODO: use a reduce or recur here rather
;; than simply recursion
(defn find-fail-in [fs1 fs2 paths]
  (if (not (empty? paths))
    (let [path (first paths)
          val1 (get-in fs1 path :top)
          val2 (get-in fs2 path :top)]
      (if (fail? (unify val1 val2))
        {:fail-path (str "/" (string/join "/" path))
         :val1 (strip-refs val1)
         :val2 (strip-refs val2)}
        (find-fail-in fs1 fs2 (rest paths))))))

(defn fail-path-between [fs1 fs2]
  "if unifying fs1 and fs2 leads to a fail somewhere, show the path to the fail. Otherwise return nil."
  (let [paths-in-fs1 (map #(first (first %)) (pathify-r fs1))
        paths-in-fs2 (map #(first (first %)) (pathify-r fs2))]
    (find-fail-in fs1 fs2 (concat paths-in-fs1 paths-in-fs2))))

;; shorter alternative to the above.
(defn fail-path [fs1 fs2]
  "if unifying fs1 and fs2 leads to a fail somewhere, show the path to the fail. Otherwise return nil."
  (fail-path-between fs1 fs2))

;; BELOW: DEPRECATED. Terrible performance. Either implement in
;; a feasible, efficient manner, or remove.
(defn compare-bytewise [a b index]
  "compare two byte arrays by casting each byte to short."
  (if (> (alength a) index)
    (if (> (alength b) index)
      (if (= (nth a index)
             (nth b index))
        (compare-bytewise a b (+ 1 index))
        (< (nth a index)
           (nth b index)))
      true)
    false))

(defn sorted-paths-1 [paths]
  (sort (fn [x y]
          (let [size-x (count x)
                size-y (count y)]
            (cond (< (count x) (count y)) true
                  (> (count x) (count y)) false
                  true (compare-bytewise (.getBytes (str x)) (.getBytes (str y)) 0))))
          paths))

(defn sorted-paths [serialized path n index]
  (let [lookup (nth serialized index)
        allpaths (seq (first (butlast lookup)))]
    (sorted-paths-1 allpaths)))

(defn is-first-path [serialized path n index]
  (if (nil? index)
    (exception (str "Index was null in serialized feature structure: " serialized)))
  (let [lookup (nth serialized index)
          firstpath (seq (first (sorted-paths serialized path n index)))]
      true))

(defn first-path [serialized path n index]
  (let [lookup (nth serialized index)
        firstpath (seq (first (sorted-paths serialized path n index)))]
    firstpath))
