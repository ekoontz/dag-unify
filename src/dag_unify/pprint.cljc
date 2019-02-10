(ns dag_unify.pprint
  (:require
   [clojure.string :refer [join]]
   [dag_unify.core :as u :refer [ref? serialize unify]]))

(def ^:dynamic *local-keys* (set #{:dag_unify.core/serialized ::annotate}))

(defn key-rank [k keys]
  "used for drawing: determine y axis of k within keys."
  (if (= k (first keys))
    0
    (+ 1 (key-rank k (rest keys)))))

(defn rank-within [path fs]
  "used for drawing: determine y axis of path within fs."
  (let [ks (keys fs)]
    (+ (key-rank (first path) ks)
       (if (not (empty? (rest path)))
         (rank-within (rest path) (u/get-in fs [(first path)]))
         0))))

(defn count-refs-in-path [path fs]
  (if (empty? path) 0
      (let [val (get fs (first path))]
        (+ (count-refs-in-path (rest path) (u/get-in fs [(first path)]))
           (if (ref? val) 1 0)))))

(defn is-first-ref? [path sets]
  "return true if _path_ is the first member of a set of paths that all point to a single reference."
  (and (not (empty? sets))
       (or (= path (first sets))
           (is-first-ref? path (rest sets)))))

(defn width [fs & [firsts path]]
  "find the width (length of horizontal dimension) of a fs for 
   printing out to a fixed-width-font representation."
  (let [firsts (or firsts (map first (map first (rest (serialize fs)))))
        is-first-ref? (is-first-ref? path firsts)]
    (cond (map? fs)
          (+ 1 (apply max
                      (map (fn [k]
                             (let [val (get fs k)]
                               (width val firsts (concat path [k]))))
                           (keys fs))))
          (and (ref? fs) (= is-first-ref? true)) (+ 1 (width @fs))
          (ref? fs) 1
          true 1)))

(defn height [fs & [firsts path]]
  "find the height (length of vertical dimension) of a fs for 
   printing out to a fixed-width-font representation."
  (let [firsts (or firsts (map first (map first (rest (serialize fs)))))
        is-first-ref? (is-first-ref? path firsts)]
    (cond (map? fs)
          (apply +
                 (map (fn [k]
                        (let [val (get fs k)]
                          (height val firsts (concat path [k]))))
                      (keys fs)))
          (and (ref? fs) (= is-first-ref? true)) (height @fs)
          (ref? fs) 1
          true 1)))

(defn index-of-ref [path path-sets & [i]]
  (let [i (or i 1)]
    (if (not (empty? path-sets))
      (if (contains? (set (first path-sets)) path)
        i
        (index-of-ref path (rest path-sets) (+ 1 i)))
      (throw (Exception. (str "no path-set found that contains:" path))))))

(defn annotate [fs & [firsts path x y path-sets]]
  "mark up a _fs_ recursively with an :x,:y:,:width, and :height."
  (let [firsts (or firsts (map first (map first (rest (serialize fs)))))
        is-path-first-ref? (is-first-ref? path firsts)
        path-sets (or path-sets (map first (rest (serialize fs))))
        x (or x 1)
        y (or y 1)]
    (cond (and (map? fs)
               (not (empty? fs)))
          (let [k (first (keys fs))
                val (get fs k)
                height (height val firsts (concat path [k]))
                path-plus-k (concat path [k])]
            (unify fs
                   {::annotate
                    {k
                     {:x x
                      :type (cond (map? val)
                                  :map
                                  (and (ref? val)
                                       (is-first-ref? path-plus-k firsts))
                                  :first-ref
                                  (ref? val)
                                  :ref
                                  true :other)
                      :index (cond (ref? val)
                                   (index-of-ref path-plus-k
                                                 path-sets)
                                   true nil)
                      :y y}}}
                   {k (annotate val firsts path-plus-k
                                (+ x 1)
                                y path-sets)}
                   (annotate (dissoc fs k) firsts path
                             x (+ 0 y height) path-sets)))

          (and (ref? fs) (= is-path-first-ref? true))
          (annotate @fs
                    firsts path
                    (+ 1 x) y path-sets)

          (ref? fs) fs
          true fs)))

(defn gather-annotations [fs & [firsts path annotate]]
  "create a map from paths in _fs_ to the values at those paths."
  (let [path (or path [])
        firsts (or firsts
                   (map first (map
                               (fn [set] (reverse (sort-by (fn [a] (str a)) set)))
                               (map first (rest (serialize fs))))))
        annotate (or annotate (get fs ::annotate))
        is-path-first-ref? (is-first-ref? path firsts)]
    (cond (and (map? fs)
               (not (empty? (keys fs))))
          (let [k (first (keys fs))
                path-plus-k (concat path [k])]
            (if (contains? *local-keys* k)
              ;; ignore this _k_ and continue.
              (gather-annotations (dissoc fs k) firsts path annotate)
              (clojure.core/merge
               {path-plus-k (get annotate k)}
               (if (or (not (ref? (get fs k)))
                       (is-first-ref? path-plus-k firsts))
                 (gather-annotations (u/get-in fs [k])
                                     firsts
                                     path-plus-k
                                     (get (u/get-in fs [k]) ::annotate)))
               (gather-annotations (dissoc fs k) firsts path annotate)))))))

(defn add-indices [elements values]
  (if (not (empty? elements))
    (let [element (first elements)
          {x :x
           y :y
           type :type
           index :index} element
          {v :v} (first values)]
      (cond (= type :first-ref)
            (merge {{:x (+ 1 x) :y y :type :index}
                    {:v index}
                    {:x (+ 2 x) :y y :type :other} {:v v}}
                   (add-indices (rest elements) (rest values)))
            (= type :ref)
            (merge {{:x (+ 1 x) :y y :type :index}
                    {:v index}}
                   (add-indices (rest elements) (rest values)))
            (and (not (nil? v)) (not (= type :index)))
            (merge {{:x (+ 1 x) :y y :type :other} {:v v}}
                   (add-indices (rest elements) (rest values)))
            true
            (add-indices (rest elements) (rest values))))))

(defn elements [fs]
  (let [;; path => {:index :type :x :y}
        path-to-cell (gather-annotations (annotate fs))
        
        ;; invert path-to-cell to: {:index :type :x :y} => path
        cell-to-path (zipmap (map (fn [v]
                                    (cond (nil? (:index v))
                                          (reduce dissoc v
                                                  [:type :index])
                                          true v))
                                  (vals path-to-cell))
                             (map (fn [path]
                                    (merge 
                                     {:k (last path)}
                                     (if (not (map? (u/get-in fs path)))
                                       {:v (u/get-in fs path)})))
                                  (keys path-to-cell)))

        with-indices (merge cell-to-path
                            (add-indices (keys cell-to-path)
                                         (vals cell-to-path)))
        elements (map (fn [each]
                        (cond (= (:type each) :ref)
                              (dissoc each :v)
                              true
                              each))
                      (map (fn [k]
                             (merge k (get with-indices k)))
                           (keys with-indices)))
        ;; cleanup (1)
        elements
        (map #(if (or (= (:type %) :first-ref)
                      (= (:type %) :other)
                      (= (:type %) :ref))
                (dissoc % :type)
                %) elements)
        
        ;; cleanup (2)
        elements ;; if :k, remove :index and :v
        (map #(if (:k %)
                (dissoc (dissoc % :index) :v)
                %) elements)
        
        ;; cleanup (3)
        elements (remove #(= nil (:v % :none)) elements)
        
        ;; cleanup (4)
        elements (map #(if (= (:type %) :index)
                         (merge {:i (:v %)}
                                (dissoc (dissoc % :v) :type))
                         %)
                      elements)]
    elements))

(defn width-of-cell [cell]
  (count (cond
           (:k cell)
           (str (:k cell))

           (:i cell)
           (str "[" (:i cell) "]")

           (:v cell)
           (str (:v cell))

           true "")))

(defn width-of-column [elements column]
  (let [height (apply max (map :y elements))
        width (apply max (map :x elements))
        grouped-by-rows (map (fn [row] (sort-by (fn [elem] (:x elem))
                                                (filter #(= (:y %) row)
                                                        elements)))
                             (range 1 (+ 1 height)))
        grouped-by-cols (map (fn [col] (sort-by (fn [elem] (:y elem))
                                                (filter #(= (:x %) col)
                                                        elements)))
                             (range 1 (+ 1 width)))]
    (apply max (map width-of-cell
                    (nth grouped-by-cols column)))))

(defn elements-of-row [elements row]
  (sort-by (fn [elem]
             (:x elem))
           (map #(dissoc % :y)
                (filter #(= (:y %) row)
                        elements))))

(defn padding [value width]
  (join ""
        (map (fn [index]
               " ")
             (range 0 (- width (width-of-cell value))))))

(defn by-rows [fs]
  "return an array of strings which are a line-oriented, fixed-width
  character representation of the input _fs_"
  (let [elements (elements fs)
        height (apply max (map :y elements))
        width (apply max (map :x elements))
        grouped-by-rows (map (fn [row] (sort-by (fn [elem] (:x elem))
                                                (filter #(= (:y %) row)
                                                        elements)))
                             (range 1 (+ 1 height)))
        column-widths (map (fn [col-index]
                             (width-of-column elements col-index))
                           (range 0 width))]
    (doall
     (map (fn [row]
            (let [line-elements (elements-of-row elements row)
                  map-by-column (map (fn [column-index]
                                       (filter #(= (:x %) column-index)
                                               line-elements))
                                     (range 1 (+ 1 width)))]
              (str (clojure.string/trimr
                    (join " "
                          (map (fn [column]
                                 (let [v (first (nth map-by-column column))
                                       padding (padding v (nth column-widths column))]
                                   (cond (nil? v) padding
                                         (= (:type v) :first-ref)
                                         (str (:k v) padding)
                                         
                                         (= (:type v) :ref)
                                         (str (:k v) padding)
                                         
                                         (:i v)
                                         (str "[" (:i v) "]" padding)
                                         
                                         (:k v)
                                         (str (:k v) padding)
                                         
                                         (not (= ::none (:v v ::none)))
                                         (str (:v v) padding)
                                        
                                         true
                                         (str padding))))
                               (range 0 width)))))))
          (range 1 (+ 1 height))))))

(defn print-out [fs]
  (do (doall (map println (filter #(not (empty? %))
                                  (by-rows fs))))
      nil))

