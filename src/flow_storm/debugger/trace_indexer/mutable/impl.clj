(ns flow-storm.debugger.trace-indexer.mutable.impl
  (:require [flow-storm.debugger.trace-indexer.protos :refer [TraceIndex]]
            [flow-storm.debugger.trace-indexer.mutable.callstack-tree :as callstack-tree]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [flow-storm.utils :as utils])
  (:import [java.util ArrayList HashMap]))

(deftype MutableTraceIndexer [^ArrayList traces
                              ^:unsynchronized-mutable callstack-tree
                              ^HashMap forms
                              ^HashMap forms-hot-traces]

  TraceIndex

  (thread-exec-count [_]
    (.size traces))

  (add-form [_ form-id form-ns def-kind form]
    (.put forms form-id {:form/id form-id
                         :form/ns form-ns
                         :form/form form
                         :form/def-kind def-kind}))

  (get-form [_ form-id]
    (.get forms form-id))

  (add-fn-call-trace [this trace]
    (let [next-idx (.size traces)]

      (if (zero? next-idx)
        (set! callstack-tree (callstack-tree/make-callstack-tree trace next-idx))
        (callstack-tree/process-fn-call-trace callstack-tree next-idx trace))

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

  (get-trace [_ idx]
    (.get traces idx))

  (bindings-for-trace [this trace-idx]
    (let [{:keys [timestamp]} (.get traces trace-idx)
          bind-traces (callstack-tree/bind-traces-for-trace callstack-tree trace-idx)
          applicable-binds (keep (fn [bt]
                                   (when (<= (:timestamp bt) timestamp)
                                     [(:symbol bt) (:value bt)]))
                                 bind-traces)]
      (into {} applicable-binds)))

  (interesting-expr-traces [_ form-id trace-idx]
    (let [[min-trace-idx max-trace-idx] (callstack-tree/frame-min-max-traces callstack-tree trace-idx)
          hot-traces (.get forms-hot-traces form-id)]
      (->> hot-traces
           (filter (fn [t]
                     (<= min-trace-idx (:trace-idx (meta t)) max-trace-idx))))))

  (callstack-tree-root [_]
    (callstack-tree/get-tree-root callstack-tree))

  (callstack-node-frame [_ node]
    (callstack-tree/get-node-info node))

  (callstack-tree-childs [_ node]
    (callstack-tree/tree-node-childs node))

  (callstack-frame-call-trace-idx [_ trace-idx]
    (callstack-tree/frame-call-trace-index callstack-tree trace-idx))

  (search-next-fn-call-trace [_ search-str from-idx]
    (loop [i 0
           stack ()]
      (when (< i (count traces))
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
              (recur (inc i) stack))))))))

(defn make-indexer []
  (->MutableTraceIndexer (ArrayList.)
                         nil
                         (HashMap.)
                         (HashMap.)))
