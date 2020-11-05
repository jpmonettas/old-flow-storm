(ns flow-storm.api
  "This is the only namespace intended for users.
  Provides functionality to connect to the debugger and instrument forms."
  (:require [flow-storm.instrument :as i]
            [flow-storm.tracer :as t]
            [clojure.pprint :as pp]
            [clojure.repl :as clj.repl]
            [cljs.repl :as cljs.repl]))

(def connect
  "Connects to flow-storm debugger.
  Once connected, all generated traces are sent to the debugger thru
  a websocket connection.
  Optionally you can provide a map with :host and :port keys."
  t/connect)

(def traced-vars-orig-fns (atom {}))

(defn- initial-ctx [form env]
  (let [form-id (hash form)
        form-flow-id (rand-int 100000)]
    {:instrument-fn    'flow-storm.tracer/trace-and-return
     :on-bind-fn       'flow-storm.tracer/bound-trace
     :on-outer-form-fn 'flow-storm.tracer/init-trace
     :compiler         (i/target-from-env env)
     :form-id          form-id
     :form-flow-id     form-flow-id}))

(defn- pprint-on-err [x]
  (binding [*out* *err*] (pp/pprint x)))

(defmacro trace
  "Recursively instrument a form for tracing."
  ([form] `(trace nil ~form)) ;; need to do this so multiarity macros work
  ([flow-id form]
   (binding [i/*environment* &env]
     (let [ctx (-> (initial-ctx form &env)
                   (assoc :flow-id flow-id))
           inst-code (-> form
                         (i/instrument-all ctx)
                         (i/fix-outer-form-instrumentation form ctx))]

       ;; Uncomment to debug
       ;; Printing on the *err* stream is important since
       ;; printing on standard output messes  with clojurescript macroexpansion
       #_(pprint-on-err (i/macroexpand-all form))
       #_(pprint-on-err inst-code')

       inst-code))))

(defmacro trace-var [var-symb]
  (binding [i/*environment* &env]
    (let [compiler (i/target-from-env &env)
          form (read-string (case compiler
                              :clj  (clj.repl/source-fn var-symb)
                              :cljs (cljs.repl/source-fn &env var-symb)))
          ctx (initial-ctx form &env)
          inst-code (-> form
                        (i/instrument-all ctx)
                        (i/redefine-vars var-symb form ctx))]
      inst-code)))

(defmacro untrace-var [var-symb]
  (let [compiler (i/target-from-env &env)]
    (case compiler
      :clj `(do
              (alter-var-root (var ~var-symb) (constantly (get @flow-storm.api/traced-vars-orig-fns (quote ~var-symb))))
              (swap! flow-storm.api/traced-vars-orig-fns dissoc (quote ~var-symb)))
      :cljs `(do
               (set! ~var-symb (get @flow-storm.api/traced-vars-orig-fns (quote ~var-symb)))
               (swap! flow-storm.api/traced-vars-orig-fns dissoc (quote ~var-symb))))))

(defn read-trace-tag [form]
  `(flow-storm.api/trace ~form))

(defn read-ztrace-tag [form]
  `(flow-storm.api/trace 0 ~form))

(comment
  (connect)
  (trace-var clojure.core/map)
  (map inc (range 1 2 3))

  (macroexpand '(trace (defn foo [])))
  (do
    (require '[flow-storm.api :as fsa])
    (fsa/connect)

    (fsa/trace-var cljs.core/odd?)
    (fsa/trace-var cljs.core/map)
    (fsa/trace-var cljs.core/take-last)
    (macroexpand-1 '(fsa/trace-var cljs.core/some))
    )

  )
