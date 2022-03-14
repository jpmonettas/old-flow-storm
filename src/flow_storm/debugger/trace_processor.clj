(ns flow-storm.debugger.trace-processor
  (:require [flow-storm.debugger.state :as dbg-state]
            [flow-storm.debugger.ui.main :as ui-main]
            flow-storm.tracer)
  (:import [flow_storm.tracer InitTrace ExecTrace FnCallTrace BindTrace]))

(defprotocol ProcessTrace
  (process [_]))


(defn increment-trace-counter []
  (swap! dbg-state/*state update :trace-counter inc)
  (ui-main/update-trace-counter (:trace-counter @dbg-state/*state)))

(extend-protocol ProcessTrace
  InitTrace
  (process [trace]
    (println "Processing INIT" trace))

  ExecTrace
  (process [trace]
    (println "Processing EXEC" trace))

  FnCallTrace
  (process [trace]
    (println "Processing FN_CALL" trace))

  BindTrace
  (process [trace]
    (println "Processing BIND" trace)))

(defn dispatch-trace [trace]
  (process trace)
  (increment-trace-counter))
