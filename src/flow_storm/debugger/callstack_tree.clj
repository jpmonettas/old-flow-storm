(ns flow-storm.debugger.callstack-tree
  (:require [clojure.zip :as zip]))

#_{:calstack-tree [{:fn org.some/fun
                  :args [...]
                  :ret 5
                  :bindings [BindTrace{}, BindTrace{}, ...]
                  :calls [{:fn org.some/other-fun
                           :args [...]
                           :ret 3
                           :bindings [BindTrace{}, BindTrace{}, ...]
                           :calls []
                           }
                          ...]
                  }]
   :curr-path [0 ]}

(defn- make-tree-node [{:keys [form-id fn-name fn-ns args-vec timestamp]}]
  {:fn-name fn-name
   :fn-ns fn-ns
   :args args-vec
   :timestamp timestamp
   :expr-traces []
   :form-id form-id
   :bindings {}
   :calls []})

(defn make-call-tree [fn-call-trace]
  (let [frame (make-tree-node fn-call-trace)]
    {:zipper (zip/zipper :calls
                         :calls
                         (fn [node children] (assoc node :calls children))
                         frame)
     :trace-idx->frame {0 frame}}))

(defn process-fn-call-trace [callstack-tree trace-idx fn-call-trace]
  (let [frame (make-tree-node fn-call-trace)]
    (-> callstack-tree
        (update :zipper (fn [z]
                          (-> z
                              (zip/insert-child frame)
                              zip/down)))
        (update :trace-idx->frame assoc trace-idx frame))))

(defn pricess-bind-trace [callstack-tree {:keys [symbol value]}]
  (-> callstack-tree
      (update :zipper (fn [z]
                        (-> z
                            (zip/edit (fn [node]
                                        (update node :bindings assoc symbol value))))))))

(defn process-exec-trace [callstack-tree trace-idx {:keys [result outer-form?] :as trace}]
  (-> callstack-tree
      ;; IMPORTANT the order here matters
      (update :trace-idx->frame assoc trace-idx (zip/node (:zipper callstack-tree)))
      (update :zipper (fn [z]
                        ;; alway add the trace to :expr-traces
                        ;; if it is outer-form? then set :ret and move up (return)
                        ;; unless you are already at the root
                        (let [z' (-> z
                                     (zip/edit (fn [node]
                                                 (cond-> node
                                                   true        (update :expr-traces conj trace)
                                                   outer-form? (assoc :ret result)))))]
                          (if outer-form?
                            (if-let [up (zip/up z')]
                              up
                              z')
                            z'))))))

(defn callstack-tree [{:keys [zipper]}]
  (zip/root zipper))

(defn find-frame [{:keys [trace-idx->frame]} trace-idx]
  (trace-idx->frame trace-idx))
