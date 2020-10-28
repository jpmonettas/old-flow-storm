(ns flow-storm.api
  "This is the only namespace intended for users.
  Provides functionality to connect to the debugger and instrument forms."
  (:require [flow-storm.instrument :as i]
            [flow-storm.tracer :as t]
            [clojure.pprint :as pp]))

(def connect
  "Connects to flow-storm debugger.
  Once connected, all generated traces are sent to the debugger thru
  a websocket connection.
  Optionally you can provide a map with :host and :port keys."
  t/connect)

(defmacro trace
  "Recursively instrument a form for tracing."
  ([form] `(trace nil ~form)) ;; need to do this so multiarity macros work
  ([flow-id form]
   (binding [i/*environment* &env]
     (let [form-id (hash form)
           form-flow-id (rand-int 100000)
           ctx {:instrument-fn    'flow-storm.tracer/trace-and-return
                :on-bind-fn       'flow-storm.tracer/bound-trace
                :on-outer-form-fn 'flow-storm.tracer/init-trace
                :compiler         (i/target-from-env &env)
                :flow-id          flow-id
                :form-id          form-id
                :form-flow-id     form-flow-id}
           
           inst-code (-> form
                         (i/tag-form-recursively) ;; tag all forms adding ::i/coor
                         (i/instrument-tagged-code ctx))
           ;; this is a super hacky way of figuring out if the outer instrumented form is a defn
           ;; we are doing this because if it is, we need to "un-instrument it" and instrument it again
           ;; a little different
           inst-code' (if (i/fn-def-form? (i/unwrap-form inst-code))
                        (let [[_ fn-name fn-form] (i/unwrap-form inst-code)]
                          (list 'def fn-name (i/instrument-function-bodies fn-form
                                                                           (assoc ctx
                                                                                  :orig-form form
                                                                                  :fn-name (name fn-name))
                                                                           i/instrument-outer-forms)))
                        (i/instrument-outer-forms (assoc ctx :orig-form form) (list inst-code)))]

       ;; Uncomment to debug
       ;; Printing on the *err* stream is important since
       ;; printing on standard output messes  with clojurescript macroexpansion
       #_(binding [*out* *err*] (pp/pprint inst-code'))

       inst-code'))))

(defn read-trace-tag [form]
  `(flow-storm.api/trace ~form))

(defn read-ztrace-tag [form]
  `(flow-storm.api/trace 0 ~form))
