(ns dag_unify.benchmark
  "Performance benchmarks for dag_unify.core's unification.

  Each scenario builds one or more large, complicated DAGs (deeply nested
  maps, densely shared references, exponential path counts, ...) and then
  measures how long a single (unify) / (unify!) of them takes.

  Run with:

      lein bench                 ; standard sizes
      lein bench --quick         ; small sizes, fast smoke run
      lein bench --scale 4       ; multiply the default sizes
      lein bench --runs 10       ; more repetitions per scenario
      lein bench --csv out.csv   ; also write results to a CSV file
      lein bench help

  Sizes are all derived from a single --scale factor, so the whole suite
  can be made heavier or lighter without touching this file.

  The non-destructive scenarios reuse the same input DAGs across runs, and
  the destructive ones rebuild their inputs inside the timed section, so
  every measurement reflects unification work and not input construction.

  The whole run happens on a thread with a large stack so that the
  intentionally deep DAGs do not overflow the default stack."
  (:require [clojure.string :as str]
            [dag_unify.core :as dag])
  (:import (java.util Locale)))

;; ---------------------------------------------------------------------------
;; DAG generators
;; ---------------------------------------------------------------------------

(defn nested-chain
  "A map nested `depth` levels deep, with `leaf` at the bottom."
  [depth leaf]
  (reduce (fn [acc _] {:next acc}) leaf (range (long depth))))

(defn wide-map
  "A flat map with `width` distinct keys and values."
  [width]
  (into {} (map (fn [i] [(keyword (str "k" i)) i]) (range (long width)))))

(defn shared-ref-dag
  "A DAG with `n` top-level keys, each pointing at several atoms that are
  shared across keys. `value-fn` is called with the ref index to produce
  the atom's initial value (default :top)."
  ([n] (shared-ref-dag n (constantly :top)))
  ([n value-fn]
   (let [n (long n)
         refs (vec (map value-fn (range n)))]
     (into {}
           (for [i (range n)]
             [(keyword (str "n" i))
              {:self (refs i)
               :next (refs (mod (inc i) n))
               :left {:a (refs (mod (* 2 i) n))}
               :right {:b (refs (mod (+ 3 i) n))
                       :c (refs (mod (* 3 i) n))}}])))))

(defn shared-fan-dag
  "A DAG of `depth` levels and `breadth` children per level. The same child
  map is referenced by every key of its parent, so the number of distinct
  paths through the DAG is breadth^depth even though the number of distinct
  maps is only depth*breadth."
  [depth breadth leaf]
  (let [keys (mapv #(keyword (str "c" %)) (range (long breadth)))]
    (reduce (fn [acc _]
              (into {} (map (fn [k] [k acc]) keys)))
            leaf
            (range (long depth)))))

(defn shared-subtree-dag
  "Returns [ref-dag value-dag]. ref-dag has `n` keys, each an atom whose
  value is :top; value-dag has the same `n` keys, each pointing at the very
  same `subtree`. Unifying them forces one cycle-detection scan of `subtree`
  per key."
  [n subtree]
  (let [keys (mapv #(keyword (str "s" %)) (range (long n)))
        refs (vec (repeatedly (long n) #(atom :top)))]
    [(into {} (map (fn [k r] [k r]) keys refs))
     (into {} (map (fn [k] [k subtree]) keys))]))

(defn feature-structure
  "A moderately sized linguistic feature structure, specialized with `i`."
  [i]
  (let [agr (atom :top)
        subj (atom :top)]
    {:cat :verb
     :agr agr
     :id i
     :phon (str "v" i)
     :synsem {:cat :verb
              :agr agr
              :subcat {:1 {:cat :noun :agr agr :sem subj}
                       :2 {:cat :noun :sem {:pred :name :obj (atom :top)}}}
              :sem {:pred :verb :subj subj :obj (atom :top)}}
     :morph {:tense {:present {:1sing (str "v" i "s")}}
             :form i}}))

(defn feature-template
  "A more general feature structure, so unifying it with any
  (feature-structure i) succeeds."
  []
  (let [agr (atom :top)
        subj (atom :top)]
    {:cat :verb
     :agr agr
     :synsem {:cat :verb
              :agr agr
              :subcat {:1 {:cat :noun :agr agr :sem subj}}
              :sem {:pred :verb :subj subj}}
     :morph {:tense (atom :top)}}))

;; ---------------------------------------------------------------------------
;; Scenario construction
;; ---------------------------------------------------------------------------

(defn- log2 [x]
  (/ (Math/log (max 1.0 (double x))) (Math/log 2.0)))

(defn scenarios
  "Build the list of benchmark scenarios for the given scale factor.
  Each scenario is {:name string :op (fn [] result)}."
  [scale]
  (let [n  (fn [base] (max 1 (long (Math/round (* (double base) scale)))))
        ;; the exponential scenario is scaled by depth, not by breadth,
        ;; so that its path count grows smoothly instead of exploding.
        fan-depth  (+ 8 (max 0 (int (Math/round (log2 scale)))))
        template   (feature-template)]
    [{:name "deep-nested-chain"
      :op   (let [a (nested-chain (n 20000) :leaf)
                 b (nested-chain (n 20000) :leaf)]
              (fn [] (dag/unify a b)))}

     {:name "wide-flat-map"
      :op   (let [a (wide-map (n 300000))
                 b (wide-map (n 300000))]
              (fn [] (dag/unify a b)))}

     {:name "shared-ref-dag"
      :op   (let [a (shared-ref-dag (n 8000))
                 b (shared-ref-dag (n 8000))]
              (fn [] (dag/unify a b)))}

     {:name "shared-ref-dag-filled"
      :op   (let [mk (fn [] (shared-ref-dag
                             (n 5000)
                             (fn [i] {:payload {:i i :nested {:deep (str i)}}})))
                  a  (mk)
                  b  (mk)]
              (fn [] (dag/unify a b)))}

     {:name "exponential-paths"
      :op   (let [a (shared-fan-dag fan-depth 4 :leaf)
                  b (shared-fan-dag fan-depth 4 :leaf)]
              (fn [] (dag/unify a b)))}

     {:name "incremental-unify!"
      :op   (let [steps (n 5000)]
              (fn []
                (reduce (fn [acc i] (dag/unify! acc {(keyword (str "k" i)) i}))
                        {}
                        (range steps))))}

     {:name "refs-vs-shared-subtree"
      :op   (let [nrefs  (n 600)
                 sdepth (n 6000)]
              (fn []
                (let [[refs values] (shared-subtree-dag nrefs (nested-chain sdepth :leaf))]
                  (dag/unify! refs values))))}

     {:name "grammar-rule-application"
      :op   (let [k (n 12000)]
              (fn []
                ;; each word is unified against the same shared template, so
                ;; the template is copied and unified k times.
                (count (into [] (map (fn [i] (dag/unify template (feature-structure i))))
                             (range k)))))}]))

;; ---------------------------------------------------------------------------
;; Timing harness
;; ---------------------------------------------------------------------------

(defn- median [xs]
  (let [s (sort xs)
        n (count s)]
    (if (odd? n)
      (double (nth s (quot n 2)))
      (/ (+ (nth s (dec (quot n 2))) (nth s (quot n 2))) 2.0))))

(defn- mean [xs]
  (/ (double (reduce + xs)) (count xs)))

(defn- millis [nanos]
  (/ (double nanos) 1.0e6))

(defn- fmt [x]
  (String/format Locale/ROOT "%.2f" (to-array [(double x)])))

(defn bench-scenario
  "Warm up and then time `op` `runs` times. Returns a map of statistics."
  [{:keys [name op]} {:keys [warmup runs]}]
  ;; Verify the scenario actually unifies (rather than immediately failing),
  ;; and count that pass as the first warmup.
  (let [verify-result (op)
        ok? (not (dag/fail? verify-result))]
    (dotimes [_ (max 0 (dec warmup))] (op))
    (let [samples (vec (repeatedly runs
                                   (fn []
                                     (let [t0 (System/nanoTime)
                                           _  (op)
                                           t1 (System/nanoTime)]
                                       (millis (- t1 t0))))))]
      {:name   name
       :ok?    ok?
       :runs   runs
       :min    (apply min samples)
       :median (median samples)
       :mean   (mean samples)
       :max    (apply max samples)})))

(defn- print-header []
  (println)
  (println (str (format "%-26s" "scenario")
                (format "%8s" "runs")
                (format "%11s" "min(ms)")
                (format "%11s" "median(ms)")
                (format "%11s" "mean(ms)")
                (format "%11s" "max(ms)")
                "  status"))
  (println (apply str (repeat 88 "-"))))

(defn- print-result [{:keys [name ok? runs min median mean max]}]
  (println (str (format "%-26s" name)
                (format "%8d" runs)
                (format "%11s" (fmt min))
                (format "%11s" (fmt median))
                (format "%11s" (fmt mean))
                (format "%11s" (fmt max))
                "  " (if ok? "ok" "FAIL?"))))

(defn- write-csv [path results]
  (let [header "scenario,runs,min_ms,median_ms,mean_ms,max_ms,status"
        rows (map (fn [{:keys [name ok? runs min median mean max]}]
                    (str/join "," [name runs (fmt min) (fmt median)
                                   (fmt mean) (fmt max) (if ok? "ok" "fail")]))
                  results)]
    (spit path (str (str/join "\n" (cons header rows)) "\n"))))

;; ---------------------------------------------------------------------------
;; Command line
;; ---------------------------------------------------------------------------

(defn- parse-args [args]
  (loop [opts {:scale 1.0 :runs 5 :warmup 2 :csv nil}
         args args]
    (if (seq args)
      (let [arg (first args)
            more (rest args)]
        (cond
          (#{"--quick" "-q"} arg) (recur (assoc opts :scale 0.25 :runs 3 :warmup 1) more)
          (#{"--help" "-h" "help" "--usage"} arg) (recur (assoc opts :help true) more)
          (#{"--scale" "-s"} arg) (recur (assoc opts :scale (Double/parseDouble (first more)))
                                          (rest more))
          (#{"--runs" "-r"} arg)  (recur (assoc opts :runs (Long/parseLong (first more)))
                                          (rest more))
          (#{"--warmup" "-w"} arg) (recur (assoc opts :warmup (Long/parseLong (first more)))
                                           (rest more))
          (#{"--csv"} arg)        (recur (assoc opts :csv (first more)) (rest more))
          :else (recur opts more)))
      opts)))

(defn- usage []
  (println "Usage: lein bench [options]")
  (println)
  (println "  --quick, -q        run with small DAGs and fewer repetitions")
  (println "  --scale, -s N      multiply all default DAG sizes by N (default 1)")
  (println "  --runs,  -r N      timed repetitions per scenario (default 5)")
  (println "  --warmup, -w N     warmup repetitions per scenario (default 2)")
  (println "  --csv PATH         also write the results to a CSV file")
  (println "  help               show this help")
  (println)
  (println "Note: (lein bench --help) is intercepted by Leiningen; use")
  (println "      (lein bench help) or (lein run -m dag_unify.benchmark --help)."))

(defn run-all [{:keys [scale runs warmup csv]}]
  (println "dag-unify benchmark")
  (println (str "Clojure " (clojure-version)
                ", Java " (System/getProperty "java.version")
                ", scale " scale
                ", warmup " warmup
                ", runs " runs))
  (let [scenarios (scenarios scale)]
    (print-header)
    (let [results (mapv (fn [scenario]
                          (System/gc)
                          (let [result (bench-scenario scenario {:warmup warmup :runs runs})]
                            (print-result result)
                            (flush)
                            result))
                        scenarios)]
      (println (apply str (repeat 88 "-")))
      (println (str "total median: "
                    (fmt (reduce + (map :median results)))
                    " ms across " (count results) " scenarios"))
      (when (some (complement :ok?) results)
        (println "\nWARNING: some scenarios returned :fail; their timings are not meaningful."))
      (when csv
        (write-csv csv results)
        (println (str "wrote " csv)))
      results)))

(def ^:const stack-size
  "Large thread stack so the intentionally deep DAGs do not overflow."
  (* 512 1024 1024))

(defn -main [& args]
  (let [opts (parse-args args)]
    (if (:help opts)
      (usage)
      (let [result (promise)
            thread (Thread. nil
                            (fn []
                              (deliver result
                                       (try {:ok (run-all opts)}
                                            (catch Throwable t {:error t}))))
                            "dag-unify-benchmark"
                            stack-size)]
        (.start thread)
        (.join thread)
        (let [{:keys [error]} @result]
          (when error
            (println "benchmark failed:" (.getMessage ^Throwable error))
            (System/exit 1)))))))
