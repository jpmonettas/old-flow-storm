(ns flow-storm.utils
  (:require [flow-storm.tracer])
  (:import [flow_storm.tracer FnCallTrace ExecTrace]))

(defn read-trace-tag [form]
  `(flow-storm.commands/trace ~form))

(defn read-rtrace-tag [form]
  `(flow-storm.api/runi ~form))

#?(:clj
   (defn colored-string [s c]
     (let [color {:red 31
                  :yellow 33}]
       (format "\033[%d;1;1m%s\033[0m" (color c) s)))

   :cljs
   (defn colored-string [_ _]
     "UNIMPLEMENTED"))

(defn fn-call-trace? [trace]
  (instance? FnCallTrace trace))

(defn exec-trace? [trace]
  (instance? ExecTrace trace))
