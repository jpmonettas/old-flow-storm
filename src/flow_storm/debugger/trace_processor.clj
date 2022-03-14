(ns flow-storm.debugger.trace-processor
  (:require [flow-storm.debugger.state :as dbg-state]
            [flow-storm.debugger.ui.main :as ui-main]))

(defn- process-init-trace [trace]
  (println "Processing INIT" trace))

(defn- process-exec-trace [trace]
  (println "Processing EXEC" trace))

(defn- process-fn-call-trace [trace]
  (println "Processing FN_CALL" trace))

(defn- process-bind-trace [trace]
  (println "Processing BIND" trace))

(defn increment-trace-counter []
  (swap! dbg-state/*state update :trace-counter inc)
  (ui-main/update-trace-counter (:trace-counter @dbg-state/*state)))

(defn dispatch-trace [[ttype :as trace]]
  (increment-trace-counter)
  (case ttype
    :init-trace    (process-init-trace trace)
    :exec-trace    (process-exec-trace trace)
    :fn-call-trace (process-fn-call-trace trace)
    :bind-trace    (process-bind-trace trace)))
