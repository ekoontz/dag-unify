(ns dag_unify.dissoc
  (:require
   [dag_unify.serialization :as s]))

;; 'dissoc-in' function defined here along with
;; its supporting functions. 
;; the idea is to remove a value at a given
;; path from a dag, so that, as well as removing the given path,
;; all other paths in the dag that refer to that value are
;; also removed.

;; can be overridden to only dissoc
;; certain paths and not others:
;; see dissoc-test/dissoc-test-4.
(def ^:dynamic remove-path? (fn [_] true))

(declare dissoc-path)

(defn dissoc-in
  "dissoc a path in a dag, as well as any other path in the dag to the same value."
  [structure path]
  (cond
    (empty? path)
    structure

    :else
    (s/deserialize
     (let [serialized (s/serialize structure)]
       (dissoc-path serialized (map first serialized) path)))))

(declare aliases-of)
(declare dissoc-in-map)
(declare get-remainders-for)
(declare prefix?)

(defn dissoc-path
  "given a serialized dag and the remaining pathsets within it, remove _path_ from the graph and all
   paths that point to the value that _path_ points to."
  [serialized pathsets path]
  (when (seq serialized)
    (let [[reentrance-set value] (first serialized)
          filtered-reentrance-set (remove #(remove-path? %) reentrance-set)
          new-reentrance-set (if (some #(prefix? path %) reentrance-set)
                               filtered-reentrance-set
                               reentrance-set)
          remainders (get-remainders-for
                      (set (cons path
                                 (aliases-of path pathsets)))
                      reentrance-set)
          new-value
          (reduce (fn [value path]
                    (let [dim (dissoc-in-map value path)]
                      dim))
                  value
                  remainders)]
      (cons [new-reentrance-set new-value]
            (dissoc-path (rest serialized) pathsets path)))))

(defn dissoc-in-map
  "dissoc a nested path from the-map; e.g.:
  (dissoc-in {:a {:b 42, :c 43}} [:a :b]) => {:a {:c 43}}." 
  [the-map path]
  (cond (or (empty? path)
            (= :top the-map)
            (= ::none (get the-map (first path) ::none)))
        the-map

        (and (empty? (rest path))
             (empty? (dissoc the-map (first path))))
        :top

        (empty? (rest path))
        (dissoc the-map (first path))
        
        :else
        (merge
         {(first path)
          (dissoc-in-map (get the-map (first path))
                         (rest path))}
         (dissoc the-map (first path)))))

(defn prefix?
  "return true iff seq a is a prefix of seq b:
  (prefix? [:a   ] [:a :b])    => true
  (prefix? [:a :b] [:a   ])    => false
  (prefix? [:a :b] [:a :c]) => false"
  [a b]
  (cond (empty? a) true
        (empty? b) false
        (= (first a) (first b))
        (prefix? (rest a) (rest b))
        :else false))

(defn remainder
  "if seq a is a prefix of seq b,
   then return what is left of b besides
   the common prefix of a.
   if seq a is not a prefix, return nil."
  [a b]
  (cond (empty? a)
        b
        (empty? b)
        nil
        (= (first a) (first b))
        (remainder (rest a) (rest b))))

(defn aliases-of
  "given _path_ and a set of set of paths, for each subset s,
   if a member m1 of s is a prefix of _path_, concatenate
   each member other m2 of s to remainder(m2,path)."
  [path reentrance-sets]
  (concat
   ;; 1. find reentrance sets where some member of
   ;; some reentrance set is a prefix of _path_:
   (->>
    reentrance-sets
    (mapcat
     (fn [reentrance-set]
       (->>
        reentrance-set
        (mapcat (fn [reentrance-path]
                  (->>
                   reentrance-set
                   (remove #(= % reentrance-path))
                   (map #(remainder % path))
                   (remove nil?)
                   (map #(concat reentrance-path %)))))))))

   ;; 2. get all paths in reentrance sets where
   ;; _path_ is a prefix of a member of the reentrance set.
   ;; TODO: pull 2. out into its own function; it's not
   ;; returning aliases of path, but rather prefixes.
   (->>
    reentrance-sets
    (filter
     (fn [reentrance-set]
       (some #(prefix? path %)
             reentrance-set)))
    (reduce concat))))

(defn get-remainders-for [aliases-of-path reentrance-set]
  (set
   (cond (empty? reentrance-set)
         aliases-of-path
         :else
         (mapcat (fn [each-alias-of-path]
                   (remove nil?
                           (map (fn [each-reentrance-path]
                                  (remainder each-reentrance-path each-alias-of-path))
                                reentrance-set)))
                 aliases-of-path))))

