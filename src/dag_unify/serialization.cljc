(ns dag_unify.serialization
  (:refer-clojure :exclude [merge]) ;; TODO: don't override (merge)
  (:require
   [clojure.set :as set :refer [union]]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [cljslog.core :as log])))

(def ^:dynamic *exclude-keys* (set #{::serialized}))

(declare merge)

(defn ref? [val]
  #?(:clj
     (= (type val) clojure.lang.Atom))
  #?(:cljs
     (= (type val) cljs.core.Atom)))

(defn exception [error-string]
  #?(:clj
     (throw (Exception. error-string)))
  #?(:cljs
     (throw (js/Error. error-string))))

(defn skeletize [input-val]
  (if (map? input-val)
    (let [sans-serialized (apply dissoc input-val *exclude-keys*)]
      (zipmap (keys sans-serialized)
              (map
               (fn [val]
                 (if (ref? val)
                     :top
                     (if (map? val)
                       (skeletize val)
                       val)))
               (vals sans-serialized))))
    input-val))

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
            (map (fn [val]
                     ;; dereference double-references (references to another reference) :
                     ;; note the implicit assumption that the level of indirection is not greater than this.
                     (if (and (ref? val)
                              (ref? @val))
                       @val
                       ;; a simple reference: reference to a non-reference (e.g. a map, boolean, etc):
                       val))
                 (vec (set (vals input)))))))

(defn find-paths-to-value
  "find all paths in _map_ which are equal to _value_, where _value_ is (ref?)=true."
  [input value path]
  (cond
    (ref? input)
    (cond

      ;; found the value that we were looking for.
      (= input value)
      [path]

      true
      ;; did not find the value, so keep looking within this value.
      (find-paths-to-value @input value path))

    (map? input)
    (reduce concat
            (map (fn [key]
                   (find-paths-to-value
                    (get input key)
                    value
                    (concat path [key])))
                 (keys input)))))

(defn find-paths-to-refs
  "find all paths in _map_ which point to any ref."
  [input path retval]
  (cond
    (ref? input)
    (find-paths-to-refs
     @input
     path
     (assoc retval input (union (set [path])
                                (get retval input #{}))))

    (map? input)
    (reduce (fn [a b] (merge-with union a b))
            (map (fn [key]
                   (find-paths-to-refs
                    (get input key)
                    (concat path [key])
                    retval))
                 (keys input)))

    true
    retval))

(defn serialize2 [input]
  (let [memoized (get input ::serialized ::none)]
    (cond
      (not (= memoized ::none))
      memoized

      (ref? input)
      (serialize2 @input)

      true
      (cons
       [[]
        (skeletize input)]
       (let [fptr (find-paths-to-refs input [] {})]
         (map (fn [ref]
                [(vec (map vec (get fptr ref)))
                 (skeletize @ref)])
              (keys fptr)))))))

(defn skels
  "create map from reference to their skeletons."
  [input-map refs]
  (let [refs (keys (find-paths-to-refs input-map [] {}))]
    (zipmap
     refs
     (map (fn [ref]
            (skeletize @ref))
          refs))))

(defn ref-skel-map
  "associate each reference in _input-map_ with:
   1. its skeleton
   2. all paths to point to it."
  [^clojure.lang.PersistentHashMap input-map]
  (let [refs
        ;; TODO: fix all-refs so that we don't need to dedup the output.
        (vec (set (all-refs input-map)))
        ;; skels returns a map from a reference to its skeleton.

        skels (skels input-map refs)]
    (zipmap
     ;; associate each ref with its skeleton.
     (map (fn [ref]
            {:ref ref
             :skel (get skels ref)})
          refs)

     ;; list of all paths that point to each ref in _input-map_.
     (map (fn [ref]
            (find-paths-to-value input-map ref nil))
          refs))))

(defn ser-intermed [input-map]
  (let [rsk (ref-skel-map input-map)]
    (clojure.core/merge
     {nil (skeletize input-map)}
     (zipmap
      (vals rsk)
      (map :skel (keys rsk))))))

(defn merge-skeleton
  "For all shared values with only a single path leading to it, the corresponding
   value is merged with the base 'skeleton', and that path-value pair is removed from the
   serialized representation."
  [si]
  (let [skel (-> si first rest first)]
    (clojure.core/merge
     si
     {nil
      (reduce merge
              (cons skel (->> (-> si rest)
                              (filter (fn [[paths val]] (= (count paths) 1)))
                              (map (fn [[paths val]] (assoc-in {} (first paths) val))))))})))

(defn serialize
  "Turns a DAG into a serialized representation, which can be again
  deserialized by (deserialize) (below).  Returns a list of pairs that
  look like: <pathset,value>. The first element of the list has an
  empty pathset, and the value is the 'skeleton' of the entire tree.
  For the rest of the elements in the list, each element contains
  a list of paths, and a common value, which all paths share."
  [input-map]
  (let [memoized (get input-map ::serialized :none)]
    (cond
      (not (= memoized :none))
      memoized

      (ref? input-map)
      (serialize @input-map)

      true
      ;; ser-intermed returns an intermediate (but fully-serialized) representation
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
      ;;   []         => skeleton
      ;;   pathset1   => value1
      ;;   pathset2   => value2
      ;;      ..         ..
      ;;
      ;; Each pathset is a set of paths to a shared value, the value
      ;; shared by all paths in that pathset.
      ;;
      ;; Convert all the paths to vectors; might be possible to remove this
      ;; conversion.
      (->
       (->> (-> (ser-intermed input-map)
                (dissoc [nil])
                merge-skeleton)
            (filter (fn [[paths val]]
                      (or (empty? paths) false
                          (> (count paths) 1))))
            (map (fn [[paths val]]
                   [(vec (map vec paths)) val])))
       vec))))

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
(defn deserialize
  "Turns a serialized representation, as returned by (serialize),
  above, back into a DAG. _serialized_ is a list of pairs that look
  like: <pathset,value>. The first element of the list has an empty
  pathset, and the value is the 'skeleton' of the entire tree.  For
  the rest of the elements in the list, an atom is created for the
  common value, and each path in the pathset points to that atom, and
  this result is then merged with the skeleton.  If
  always-create-atom? is true, (deserialize) will not create an atom
  for a value where there is only path pointing to it, as an
  optimization, since it saves time and space when copying or unifying."
  [serialized & [always-create-atom?]]
  (let [skeleton (second (first serialized))]
    (reduce merge
            (cons skeleton
                  (flatten
                   (map (fn [paths-val]
                          (let [paths (first paths-val)
                                val
                                (if (or (> (count paths) 1) always-create-atom?)
                                  (atom (second paths-val))
                                  (do
                                    (log/debug (str "no need to create an atom: only one path: " (first paths)))
                                    (second paths-val)))]
                            (map (fn [path]
                                   (create-path-in path val))
                                 paths)))
                        (rest serialized)))))))

;; TODO: get rid of this; too much redundancy with (dag_unify.core/unify!)
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
     (let [result (merge (:not val1) val2)]
       (if (= result :fail)
         val2
         :fail))

     (and (= val1 :top)
          (not (= :notfound (:not val2 :notfound))))
     val2

     (not (= :notfound (:not val2 :notfound)))
     (let [result (merge val1 (:not val2))]
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

(defn cache-serialization [structure serialized]
  (if (or (= serialized ::no-sharing) (empty? (rest serialized)))
    (assoc structure ::serialized ::no-sharing)
    (assoc structure ::serialized serialized)))
