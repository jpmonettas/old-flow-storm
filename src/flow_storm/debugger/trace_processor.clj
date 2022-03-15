(ns flow-storm.debugger.trace-processor
  (:require [flow-storm.debugger.state :as dbg-state]
            [flow-storm.debugger.ui.main :as ui-main]
            [flow-storm.debugger.ui.flows :as ui-flows]
            flow-storm.tracer)
  (:import [flow_storm.tracer InitTrace ExecTrace FnCallTrace BindTrace]))

(defprotocol ProcessTrace
  (process [_]))


(defn increment-trace-counter []
  (swap! dbg-state/*state update :trace-counter inc)
  (ui-main/update-trace-counter (:trace-counter @dbg-state/*state)))

(defn create-empty-flow [state flow-id timestamp]
  (assoc-in state [:flows flow-id] {:flow/id flow-id
                                    :flow/forms {}
                                    :flow/threads {}
                                    :timestamp timestamp
                                    }))

(defn form-pprint-tokens [form]
  ;; TODO: print tokens
  [])

(defn create-form [id ns form]
  {:form/id id
   :form/ns ns
   :form/form form
   :form/pprint-tokens (form-pprint-tokens form)})

(extend-protocol ProcessTrace
  InitTrace
  (process [{:keys [flow-id form-id form ns timestamp]}]
    (when-not (contains? (:flows @dbg-state/*state) flow-id)
      ;; flow doesn't exist, create one
      (swap! dbg-state/*state create-empty-flow flow-id timestamp)
      (ui-flows/create-empty-flow flow-id))
    (swap! dbg-state/*state
           (fn [state]
             (let [new-form (create-form form-id ns form)]
               (assoc-in state [:flows flow-id :flow/forms form-id] new-form)))))

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
