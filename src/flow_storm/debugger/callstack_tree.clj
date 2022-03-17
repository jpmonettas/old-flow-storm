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

(defn process-bind-trace [callstack-tree {:keys [symbol value]}]
  (-> callstack-tree
      (update :zipper (fn [z]
                        (-> z
                            (zip/edit (fn [node]
                                        (update node :bindings assoc symbol value))))))))

(defn process-exec-trace [callstack-tree trace-idx {:keys [result outer-form?] :as trace}]
  (let [callstack-tree-1 (update callstack-tree :zipper
                                 (fn [z]
                                   (-> z
                                       (zip/edit (fn [node]
                                                   (cond-> node
                                                     outer-form? (assoc :ret result)))))))
        callstack-tree-2 (update callstack-tree-1 :trace-idx->frame assoc trace-idx (zip/node (:zipper callstack-tree-1)))
        callstack-tree-3 (update callstack-tree-2 :zipper
                                 (fn [z]
                                   (if outer-form?
                                     (if-let [up (zip/up z)]
                                       up
                                       z)
                                     z)))]
    callstack-tree-3))

(defn callstack-tree [{:keys [zipper]}]
  (zip/root zipper))

(defn find-frame [{:keys [trace-idx->frame]} trace-idx]
  (trace-idx->frame trace-idx))
