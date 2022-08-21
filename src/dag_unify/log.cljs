(ns dag_unify.log
  (:refer-clojure :exclude [time])
  (:require [goog.log :as glog])
  (:import goog.debug.Console))

(defn fmt [msgs]
  (apply str (interpose " " (map pr-str msgs))))

(defn debug [& s]
  (let [msg (fmt s)]
    (js/console.debug msg)))        

(defn error [& s]
  (let [msg (fmt s)]
    (js/console.error msg)))    

(defn info [& s]
  (let [msg (fmt s)]
    (js/console.info msg)))

(defn warn [& s]
  (let [msg (fmt s)]
    (js/console.warn msg)))
