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
  (cond
    (ref? input-val) (skeletize @input-val)
    (map? input-val)
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
    true
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

(defn final-reference-of [input]
  (cond (ref? @input)
        (final-reference-of @input)
        true input))

(defn find-paths-to-refs
  "find all paths in _map_ which point to any ref."
  [input path retval]
  (cond
    (ref? input)
    (find-paths-to-refs
     @(final-reference-of input)
     path
     (assoc retval (final-reference-of input)
            (cons path
                  (get retval input []))))

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

(defn merge-skeleton
  "For all shared values with only a single path leading to it, the corresponding
   value is merged with the base 'skeleton', and that path-value pair is removed from the
   serialized representation."
  [si]
  (let [skel (-> si first rest first)]
    (clojure.core/merge
     (rest si)
     [[]
      (reduce merge
              (cons skel (->> (-> si rest)
                              (filter (fn [[paths val]]
                                        (= (count paths) 1)))
                              (map (fn [[paths val]]
                                     (assoc-in {} (first paths) val))))))])))

(defn serialize [input]
  (let [memoized (get input ::serialized ::none)]
    (cond
      (not (= memoized ::none))
      memoized

      (ref? input)
      (serialize @input)

      true
      (->>

       (->

        (let [fptr (find-paths-to-refs input [] {})]
          (map (fn [ref]
                 [(vec (map vec (set (get fptr ref))))
                  (skeletize @ref)])
               (keys fptr)))

        ((fn [rest-serialized]
           (cons
            [nil
             (skeletize input)]
            rest-serialized)))

        merge-skeleton)

       (filter (fn [[paths val]]
                 (or (empty? paths)
                     (> (count paths) 1))))))))

(defn create-path-in
  "create a path starting at map through all keys in map:
   (create-path-in '(a b c d e) value) => {:a {:b {:c {:d {:e value}}}}});
   the same as clojure.core/assoc-in, except when input path is empty."
  [path value]
  (if (empty? path) value
      (clojure.core/assoc-in {} path value)))

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

(defn- merge
  [val1 val2]
  (cond
    (and (map? val1)
         (map? val2))
    (merge-with merge val1 val2)
    
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
    
    (= val1 :top) val2
    (= val2 :top) val1
    (= val1 val2) val1))

(defn cache-serialization [structure serialized]
  (if (or (= serialized ::no-sharing) (empty? (rest serialized)))
    (assoc structure ::serialized ::no-sharing)
    (assoc structure ::serialized serialized)))
