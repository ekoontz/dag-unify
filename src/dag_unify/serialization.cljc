(ns dag_unify.serialization
  (:refer-clojure :exclude [merge])) ;; TODO: don't override (merge)

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
                 (vals input)))))

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

(defn skels
  "create map from reference to their skeletons."
  [input-map refs]
  (let [refs (all-refs input-map)]
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
  (let [refs (all-refs input-map)
        ;; skels returns a map from a reference to its skeleton.
        skels (skels input-map refs)]
    (zipmap
     ;; associate each ref with its skeleton.
     (map (fn [ref]
            {:skel (get skels ref)})
          refs)

     ;; list of all paths that point to each ref in _input-map_.
     (map (fn [eachref]
            (find-paths-to-value input-map eachref nil))
          refs))))

(defn ser-intermed [input-map]
  (let [top-level (skeletize input-map)
        rsk (ref-skel-map input-map)
        sk (map :skel (keys rsk))]
    (clojure.core/merge
     {nil top-level}
     (zipmap
      (vals rsk)
      sk))))

(defn sort-shortest-path-ascending-r [serialization path-length-pairs]
  (map (fn [path-length-pair]
         (let [paths (first path-length-pair)]
           (list paths
                 (get serialization paths))))
       path-length-pairs))

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
     (map (fn [paths]
            (cond (nil? paths) 0
                  (empty? paths) 0
                  true
                  (apply max (map (fn [path] (if (nil? path) 0 (count path)))
                                  paths))))
          keys))))

(defn sort-by-max-lengths [serialization]
  (let [max-lengths (max-lengths serialization)]
    (sort (fn [x y] (< (second x) (second y)))
          max-lengths)))

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
(defn deserialize [serialized]
  (let [base (second (first serialized))]
    (apply merge
           (let [all
                 (cons base
                       (flatten
                        (map (fn [paths-val]
                               (let [paths (first paths-val)
                                     val (atom (second paths-val))]
                                 (map (fn [path]
                                        (create-path-in path val))
                                      paths)))
                             (rest serialized))))]
             all))))

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

