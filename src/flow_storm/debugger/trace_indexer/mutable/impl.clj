(ns flow-storm.debugger.trace-indexer.mutable.impl
  (:require [flow-storm.debugger.trace-indexer.protos :refer [TraceIndex]]
            [flow-storm.debugger.trace-indexer.mutable.callstack-tree :as callstack-tree]
            [flow-storm.debugger.ui.state-vars :as state-vars]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [flow-storm.utils :as utils])
  (:import [java.util ArrayList HashMap]))

(deftype MutableTraceIndexer [^ArrayList traces
                               callstack-tree
                              ^HashMap forms
                              ^HashMap forms-hot-traces]

  TraceIndex

  (thread-exec-count [this]
    (.size traces))

  (add-form [this form-id form-ns def-kind mm-dispatch-val form]
    (.put forms form-id (cond-> {:form/id form-id
                                 :form/ns form-ns
                                 :form/form form
                                 :form/def-kind def-kind}
                          (= def-kind :defmethod)
                          (assoc :multimethod/dispatch-val mm-dispatch-val))))

  (get-form [this form-id]
    (.get forms form-id))

  (add-fn-call-trace [this trace]
    (let [next-idx (.size traces)]

      (callstack-tree/process-fn-call-trace callstack-tree next-idx trace)

      (.add traces trace)))

  (add-exec-trace [this {:keys [form-id]:as trace}]
    (let [next-idx (.size traces)]
      (callstack-tree/process-exec-trace callstack-tree next-idx trace)
      (.add traces trace)

      (when-not (.get forms-hot-traces form-id)
        (.put forms-hot-traces form-id (ArrayList.)))

      (.add ^ArrayList (.get forms-hot-traces form-id)
            (with-meta trace {:trace-idx next-idx}))))

  (add-bind-trace [this trace]
    (callstack-tree/process-bind-trace callstack-tree trace))

  (get-trace [this idx]
    (.get traces idx))

  (bindings-for-trace [this trace-idx]
    (let [{:keys [timestamp]} (.get traces trace-idx)
          bind-traces (callstack-tree/bind-traces-for-trace callstack-tree trace-idx)
          applicable-binds (keep (fn [bt]
                                   (when (<= (:timestamp bt) timestamp)
                                     [(:symbol bt) (:value bt)]))
                                 bind-traces)]
      (into {} applicable-binds)))

  (interesting-expr-traces [this form-id trace-idx]
    (let [[min-trace-idx max-trace-idx] (callstack-tree/frame-min-max-traces callstack-tree trace-idx)
          hot-traces (.get forms-hot-traces form-id)]
      (->> hot-traces
           (filter (fn [t]
                     (<= min-trace-idx (:trace-idx (meta t)) max-trace-idx))))))

  (callstack-tree-root [this]
    (callstack-tree/get-tree-root callstack-tree))

  (callstack-node-frame [this node]
    (callstack-tree/get-node-info node))

  (callstack-tree-childs [this node]
    (callstack-tree/tree-node-childs node))

  (callstack-frame-call-trace-idx [this trace-idx]
    (callstack-tree/frame-call-trace-index callstack-tree trace-idx))

  (search-next-fn-call-trace [this search-str from-idx print-level on-result-cb on-progress]

    (let [indexer-obj this]
      (let [search-thread (Thread.
                           (fn []
                             (binding [clojure.core/*print-level* print-level]
                               (let [total-traces (count traces)
                                     match-stack (loop [i 0
                                                        stack ()]
                                                   (when (and (< i total-traces)
                                                              (not (.isInterrupted (Thread/currentThread))))


                                                     (when (zero? (mod i 10000))
                                                       (on-progress (* 100 (/ i total-traces))))

                                                     (let [{:keys [fn-name args-vec] :as t} (.get traces i)]
                                                       (if (utils/fn-call-trace? t)


                                                         (if (and (> i from-idx)
                                                                  (or (str/includes? fn-name search-str)
                                                                      (str/includes? (pr-str args-vec) search-str)))

                                                           ;; if matches
                                                           (conj stack i)

                                                           ;; else
                                                           (recur (inc i) (conj stack i)))
                                                         ;; it is a exec-trace, check if it is returning
                                                         (if (:outer-form? t)
                                                           (recur (inc i) (pop stack))
                                                           (recur (inc i) stack))))))]
                                 (when (.isInterrupted (Thread/currentThread))
                                   (tap> "Search stopped"))
                                 (on-result-cb match-stack)))))]
        (reset! state-vars/long-running-task-thread search-thread)
        (.start search-thread)))))

(defn make-indexer []
  (->MutableTraceIndexer (ArrayList.)
                         (callstack-tree/make-callstack-tree)
                         (HashMap.)
                         (HashMap.)))
