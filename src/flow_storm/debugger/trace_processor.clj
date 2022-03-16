(ns flow-storm.debugger.trace-processor
  (:require [flow-storm.debugger.state :as state]
            [flow-storm.debugger.ui.main :as ui-main]
            [flow-storm.debugger.ui.flows :as ui-flows]
            flow-storm.tracer)
  (:import [flow_storm.tracer InitTrace ExecTrace FnCallTrace BindTrace]))

(defprotocol ProcessTrace
  (process [_]))


(defn increment-trace-counter []
  (swap! state/*state update :trace-counter inc)
  (ui-main/update-trace-counter (:trace-counter @state/*state)))



(extend-protocol ProcessTrace
  InitTrace
  (process [{:keys [flow-id form-id thread-id form ns timestamp] :as t}]
    ;; if flow doesn't exist, create one
    (when-not (state/flow @state/*state flow-id)
      (swap! state/*state state/add-flow (state/empty-flow flow-id timestamp))
      (ui-flows/create-empty-flow flow-id))

    ;; if thread doesn't exist, create one
    (when-not (state/thread @state/*state flow-id thread-id)
      (swap! state/*state state/add-thread flow-id (state/empty-thread thread-id))
      (ui-flows/create-empty-thread flow-id thread-id))

    ;; add the form

    (let [new-form (state/create-form form-id ns form)]
      (swap! state/*state state/add-form flow-id thread-id new-form))
    #_(ui-flows/add-form flow-id thread-id new-form))

  ExecTrace
  (process [trace]
    )

  FnCallTrace
  (process [trace]
    )

  BindTrace
  (process [trace]
    ))

(defn dispatch-trace [trace]
  (process trace)
  (increment-trace-counter))
