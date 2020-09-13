(ns flow-storm.api
  (:require [flow-storm.instrument :as i]
            [flow-storm.tracer :as t]))

(def connect t/connect)

(defmacro trace [form]
  (let [inst-code (binding [i/*environment* &env]
                    (-> form
                        (i/tag-form-recursively 'flow-storm.tracer/trace-and-return)
                        (i/instrument-tagged-code)))]
    (if (i/fn-def-form? (second inst-code))
      (let [[_ fn-name fn-form] (second inst-code)]
        (list 'def fn-name (i/wrap-fn-bodies fn-form (partial i/wrap-dyn-bindings form))))
      (i/wrap-dyn-bindings form (list inst-code)))))
