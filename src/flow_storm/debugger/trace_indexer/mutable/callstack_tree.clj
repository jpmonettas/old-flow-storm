(ns flow-storm.debugger.trace-indexer.mutable.callstack-tree
  (:import [java.util ArrayList ArrayDeque HashMap]))

(defprotocol CallStackFrameProto
  (set-ret [_ r])
  (update-trace [_ trace-idx])
  (add-binding [_ binding-trace])
  (get-bindings [_])
  (get-min-trace-idx [_])
  (get-max-trace-idx [_])
  (get-fn-name [_])
  (get-fn-ns [_])
  (get-call-trace-idx [_])
  (get-args [_])
  (get-timestamp [_])
  (get-form-id [_])
  (get-ret [_]))

(deftype CallStackFrame [fn-name
                         fn-ns
                         call-trace-idx
                         args
                         timestamp
                         form-id
                         ^ArrayList bindings
                         ^:unsynchronized-mutable ret
                         ^:unsynchronized-mutable min-trace-idx
                         ^:unsynchronized-mutable max-trace-idx]

  CallStackFrameProto

  (set-ret [_ r]
    (set! ret r))

  (update-trace [_ trace-idx]
    (set! min-trace-idx (min trace-idx min-trace-idx))
    (set! max-trace-idx (max trace-idx max-trace-idx)))

  (add-binding [_ binding-trace]
    (.add bindings binding-trace))

  (get-bindings [_] bindings)
  (get-min-trace-idx [_] min-trace-idx)
  (get-max-trace-idx [_] max-trace-idx)
  (get-fn-name [_] fn-name)
  (get-fn-ns [_] fn-ns)
  (get-call-trace-idx [_] call-trace-idx)
  (get-args [_] args)
  (get-timestamp [_] timestamp)
  (get-form-id [_] form-id)
  (get-ret [_] ret))

(defn make-callstack-frame [{:keys [fn-name fn-ns args-vec timestamp form-id]} trace-idx]
  (->CallStackFrame fn-name
                    fn-ns
                    trace-idx
                    args-vec
                    timestamp
                    form-id
                    (ArrayList.)
                    nil
                    Long/MAX_VALUE
                    0))

(defprotocol TreeNode
  (get-node-info [_])
  (get-node-frame [_])
  (has-childs? [_])
  (add-child [_ node])
  (tree-node-childs [_]))

(defrecord CallStackTreeNode [^CallStackFrame node-frame ^ArrayList childs]

  TreeNode

  (get-node-info [_]
    {:fn-name (get-fn-name node-frame)
     :fn-ns (get-fn-ns node-frame)
     :call-trace-idx (get-call-trace-idx node-frame)
     :args (get-args node-frame)
     :timestamp (get-timestamp node-frame)
     :form-id (get-form-id node-frame)
     :ret (get-ret node-frame)})

  (get-node-frame [_]
    node-frame)

  (has-childs? [_]
    (.isEmpty childs))

  (add-child [_ node]
    (.add childs node))

  (tree-node-childs [_]
    childs))

(defn make-callstack-tree-node [frame]
  (map->CallStackTreeNode {:node-frame frame
                           :childs     (ArrayList.)}))

(defprotocol CallStackTreeProto
  (get-tree-root [_])
  (process-fn-call-trace [_ trace-idx fn-call-trace])
  (process-exec-trace [_ trace-idx exec-trace])
  (process-bind-trace [_ bind-trace])
  (bind-traces-for-trace [_ trace-idx])
  (frame-min-max-traces [_ trace-idx])
  (frame-call-trace-index [_ trace-idx]))

(defrecord CallStackTree [^CallStackTreeNode root-node ^ArrayDeque node-stack ^HashMap trace-idx->frame]

  CallStackTreeProto

  (get-tree-root [_]
    root-node)

  (process-fn-call-trace [_ trace-idx fn-call-trace]
    (let [new-frame (make-callstack-frame fn-call-trace trace-idx)
          new-node (make-callstack-tree-node new-frame)
          curr-frame (.peek node-stack)]

      (add-child curr-frame new-node)
      (.push node-stack new-node)
      (.put trace-idx->frame trace-idx new-frame)))

  (process-exec-trace [_ trace-idx exec-trace]
    (let [^CallStackTreeNode curr-node (.peek node-stack)
          ^CallStackFrame curr-frame (get-node-frame curr-node)]
      (update-trace curr-frame trace-idx)
      (when (:outer-form? exec-trace)
        (set-ret curr-frame (:ret exec-trace))
        (.pop node-stack))
      (.put trace-idx->frame trace-idx curr-frame)))

  (process-bind-trace [_ bind-trace]
    (let [^CallStackTreeNode curr-node (.peek node-stack)
          ^CallStackFrame curr-frame (get-node-frame curr-node)]
      (add-binding curr-frame bind-trace)))

  (bind-traces-for-trace [_ trace-idx]
    (let [trace-frame (.get trace-idx->frame (int trace-idx))]
      (get-bindings trace-frame)))

  (frame-min-max-traces [_ trace-idx]
    (let [trace-frame (.get trace-idx->frame (int trace-idx))]
      [(get-min-trace-idx trace-frame)
       (get-max-trace-idx trace-frame)]))

  (frame-call-trace-index [_ trace-idx]
    (let [trace-frame (.get trace-idx->frame (int trace-idx))]
      (get-call-trace-idx trace-frame))))

(defn make-callstack-tree [fn-call-trace trace-idx]
  (let [first-frame (make-callstack-frame fn-call-trace trace-idx)
        first-node (make-callstack-tree-node first-frame)
        stack (ArrayDeque.)
        trace->frame (HashMap.)] ;; Keys on this map are Integers, don't search for longs
    (.push stack first-node)
    (.put trace->frame trace-idx first-frame)
    (->CallStackTree first-node stack trace->frame)))
