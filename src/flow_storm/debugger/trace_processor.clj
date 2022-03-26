(ns flow-storm.debugger.trace-processor
  (:require [flow-storm.debugger.state :as state :refer [dbg-state]]
            [flow-storm.debugger.trace-indexer.protos :as indexer]
            [flow-storm.debugger.ui.main :as ui-main]
            [flow-storm.debugger.ui.utils :as ui-utils]
            [flow-storm.debugger.ui.flows :as ui-flows]
            [flow-storm.debugger.trace-indexer.immutable.impl :as imm-trace-indexer]
            [flow-storm.debugger.trace-indexer.mutable.impl :as mut-trace-indexer]
            [clojure.pprint :as pp]
            flow-storm.tracer)
  (:import [flow_storm.tracer InitTrace ExecTrace FnCallTrace BindTrace]))

(defprotocol ProcessTrace
  (process [_]))

;; ----------------------------------------------------------
;; TODO change all like this trace-processor, ui, etc
;; ----------------------------------------------------------



(defn increment-trace-counter []
  (state/increment-trace-counter dbg-state)
  #_(ui-main/update-trace-counter (:trace-counter @state/*state)))

(defn first-exec-trace-init [flow-id thread-id form-id]
  (state/set-trace-idx dbg-state flow-id thread-id 0)
  (ui-utils/run-now
   (ui-flows/highlight-form flow-id thread-id form-id)))

(extend-protocol ProcessTrace
  InitTrace
  (process [{:keys [flow-id form-id thread-id form ns timestamp] :as t}]
    ;; if flow doesn't exist, create one
    (when-not (state/get-flow dbg-state flow-id)
      (state/create-flow dbg-state flow-id timestamp)
      (ui-flows/create-empty-flow flow-id))

    ;; if thread doesn't exist, create one
    (when-not (state/get-thread dbg-state flow-id thread-id)
      (state/create-thread dbg-state flow-id thread-id
                           #_(imm-trace-indexer/make-indexer)
                           (mut-trace-indexer/make-indexer)
                           )
      (ui-flows/create-empty-thread flow-id thread-id))

    ;; add the form
    (indexer/add-form (state/thread-trace-indexer dbg-state flow-id thread-id)
                      form-id
                      ns
                      form))

  ExecTrace
  (process [{:keys [flow-id thread-id form-id] :as trace}]
    (let [indexer (state/thread-trace-indexer dbg-state flow-id thread-id)]

      (when (zero? (indexer/thread-exec-count indexer))
        (first-exec-trace-init flow-id thread-id form-id))

      (indexer/add-exec-trace indexer trace)))

  FnCallTrace
  (process [{:keys [flow-id thread-id form-id] :as trace}]
    (let [indexer (state/thread-trace-indexer dbg-state flow-id thread-id)]

      (when (zero? (indexer/thread-exec-count indexer))
        (first-exec-trace-init flow-id thread-id form-id))

      (indexer/add-fn-call-trace indexer trace)))

  BindTrace
  (process [{:keys [flow-id thread-id] :as trace}]
    (let [indexer (state/thread-trace-indexer dbg-state flow-id thread-id)]
      (indexer/add-bind-trace indexer trace))))

(defn dispatch-trace [trace]
  (process trace)
  (increment-trace-counter))
