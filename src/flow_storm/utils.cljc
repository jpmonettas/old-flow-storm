(ns flow-storm.utils
  (:require [flow-storm.tracer])
  (:import [flow_storm.tracer InitTrace BindTrace FnCallTrace ExecTrace]))

(defn colored-string [s c]
  (let [color {:red 31
               :yellow 33}]
    (format "\033[%d;1;1m%s\033[0m" (color c) s)))


(defn fn-call-trace? [trace]
  (instance? FnCallTrace trace))

(defn exec-trace? [trace]
  (instance? ExecTrace trace))
