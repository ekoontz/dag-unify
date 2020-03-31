(ns dag_unify.serialization
  (:refer-clojure :exclude [merge]) ;; TODO: don't override (merge)
  (:require
   [clojure.set :as set :refer [union]]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [cljslog.core :as log])))

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
    (zipmap (keys input-val)
            (map
             (fn [val]
               (if (ref? val)
                 :top
                 (if (map? val)
                   (skeletize val)
                   val)))
             (vals input-val)))
    true
    input-val))

(declare final-reference-of)

(def ^:dynamic found-refs nil)

(declare all-refs-with-binding)

(defn all-refs [input]
  (binding [found-refs (atom (set nil))]
    (all-refs-with-binding input)))

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

(defn final-reference-of [input]
  (cond (ref? @input)
        (final-reference-of @input)
        true input))

(defn find-paths-to-refs
  "find all paths in _map_ which point to any ref."
  [input path retval]
  (cond
    (ref? input)
    (let [final (final-reference-of input)]
      (find-paths-to-refs
       @final
       path
       (assoc retval final
              (cons path
                    (get retval input nil)))))

    (map? input)
    (reduce (fn [a b] (merge-with concat a b))
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
      (vec
       (reduce merge
               (cons skel (->> (-> si rest)
                               (filter (fn [[paths val]]
                                         (= (count paths) 1)))
                               (map (fn [[paths val]]
                                      (assoc-in {} (first paths) val)))))))])))

(defn serialize [input]
  (cond
    (ref? input) (serialize @input)
    true
    (vec
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
                    (> (count paths) 1))))
      (map vec)))))

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
     (ref? val2))
    (do (swap! val1
               (fn [x] (merge @val1 @val2)))
        val1)

    (ref? val1)
    (do (swap! val1
               (fn [x] (merge @val1 val2)))
        val1)
    
    (ref? val2)
    (do (swap! val2
               (fn [x] (merge val1 @val2)))
        val2)

    (= val1 :top)
    val2

    (= val2 :top)
    val1

    (= val1 val2) val1
    
    :else (exception (str "merge: unhandled case: val1: " val1 "; val1: " val2))))
