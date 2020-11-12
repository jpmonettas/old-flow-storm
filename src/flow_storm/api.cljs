(ns flow-storm.api
  (:require [flow-storm.tracer :as t])
  (:require-macros [flow-storm.api]))

;; Flow-storm api is composed of :

;;    - connect (function)
;;    - trace   (macro)

(def traced-vars-orig-fns (atom {}))

;; here we just define connect and require the trace macro from
;; the clojure ns

(def connect
  "Connects to flow-storm debugger.
  Once connected, all generated traces are sent to the debugger thru
  a websocket connection.
  Optionally you can provide a map with :host and :port keys."
  t/connect)

(def trace-ref
  "Adds a watch to ref with ref-name that traces its value changes."
  t/trace-ref)

(def untrace-ref
  "Removes the watch added by trace-ref."
  t/untrace-ref)
