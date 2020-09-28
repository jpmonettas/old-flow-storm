(ns flow-storm.api
  (:require [flow-storm.instrument :as i]
            [flow-storm.tracer :as t]
            [clojure.pprint :as pp]))

(def connect t/connect)

(defmacro trace [form]
  (let [form-id (hash form)
        form-flow-id (rand-int 100000)
        ctx {:instrument-fn    'flow-storm.tracer/trace-and-return
             :on-bind-fn       'flow-storm.tracer/bound-trace
             :on-outer-form-fn 'flow-storm.tracer/init-trace
             :form-id       form-id
             :form-flow-id  form-flow-id}
        inst-code (binding [i/*environment* &env]
                    (-> form
                        (i/tag-form-recursively) ;; tag all forms adding ::i/coor
                        (i/instrument-tagged-code ctx)))
        inst-code' (if (i/fn-def-form? (second inst-code))
                     (let [[_ fn-name fn-form] (second inst-code)]
                       (list 'def fn-name (i/wrap-fn-bodies fn-form (partial i/wrap-dyn-bindings form ctx))))
                     (i/wrap-dyn-bindings form ctx (list inst-code)))]

    ;; Uncomment to debug
    #_(binding [*out* *err*] (pp/pprint inst-code'))

    inst-code'))

(defn read-trace-tag [form]
  `(flow-storm.api/trace ~form))
