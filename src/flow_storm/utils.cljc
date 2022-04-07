(ns flow-storm.utils
  (:require [flow-storm.tracer])
  (:import [flow_storm.tracer FormInitTrace BindTrace FnCallTrace ExecTrace]))

(defn read-trace-tag [form]
  `(flow-storm.commands/trace ~form))

(defn read-rtrace-tag [form]
  `(flow-storm.api/runi ~form))

(defn colored-string [s c]
  (let [color {:red 31
               :yellow 33}]
    #?(:clj
       (format "\033[%d;1;1m%s\033[0m" (color c) s)
       :cljs "UNIMPLEMENTED")))


(defn fn-call-trace? [trace]
  (instance? FnCallTrace trace))

(defn exec-trace? [trace]
  (instance? ExecTrace trace))
