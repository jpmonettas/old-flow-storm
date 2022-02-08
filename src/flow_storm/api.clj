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

(def trace-ref
  "Adds a watch to ref with ref-name that traces its value changes.
  The first argument is the ref to watch for.
  The second argument is a options map. Available options are :
  - :ref-name A string name for the ref.
  - :ignore-keys A collection of keys that will be skipped in traces.

  :ignore-keys only works for maps and does NOT ignore nested maps keys."
  t/trace-ref)

(def untrace-ref
  "Removes the watch added by trace-ref."
  t/untrace-ref)

(def traced-vars-orig-fns (atom {}))

(defn- initial-ctx [form env]
  (let [form-id (hash form)]
    {:on-expr-exec-fn  'flow-storm.tracer/expr-exec-trace
     :on-bind-fn       'flow-storm.tracer/bound-trace
     :on-fn-call-fn    'flow-storm.tracer/fn-call-trace
     :on-outer-form-fn 'flow-storm.tracer/init-trace
     :compiler         (i/target-from-env env)
     :form-id          form-id
     :disable          #{}}))

(defn- pprint-on-err [x]
  (binding [*out* *err*] (pp/pprint x)))

(defmacro trace
  "Recursively instrument a form for tracing."
  ([form] `(trace nil ~form)) ;; need to do this so multiarity macros work
  ([flow-id form]
   (binding [i/*environment* &env]
     (let [ctx (-> (initial-ctx form &env)
                   (assoc :flow-id flow-id
                          :form-ns (str (ns-name *ns*))))
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
          form (some-> (case compiler
                         :clj  (clj.repl/source-fn var-symb)
                         :cljs (cljs.repl/source-fn &env var-symb))
                       read-string)]
      (if form
        (let [ctx (initial-ctx form &env)
              inst-code (-> form
                            (i/instrument-all ctx)
                            (i/redefine-vars var-symb form ctx))]
          inst-code)

        (println "Couldn't find source for " var-symb)))))

(defmacro untrace-var [var-symb]
  (let [compiler (i/target-from-env &env)
        var-ns (when-let [vns (namespace var-symb)] (symbol vns))]
    (case compiler
      :clj `(let [current-ns# (ns-name *ns*)]
              (in-ns (quote ~var-ns))
              (alter-var-root (var ~var-symb) (constantly (get @flow-storm.api/traced-vars-orig-fns (quote ~var-symb))))
              (swap! flow-storm.api/traced-vars-orig-fns dissoc (quote ~var-symb))
              (in-ns current-ns#))
      :cljs `(do
               (set! ~var-symb (get @flow-storm.api/traced-vars-orig-fns (quote ~var-symb)))
               (swap! flow-storm.api/traced-vars-orig-fns dissoc (quote ~var-symb))))))

(defn read-trace-tag [form]
  `(flow-storm.api/trace ~form))

(defn read-ztrace-tag [form]
  `(flow-storm.api/trace 0 ~form))

(comment
  (connect)
  #trace
  (defn factorial [n]
    (if (zero? n)
      1
      (* n (factorial (dec n)))))

  #trace
  (defn boo [xs]
    (reduce + (pmap factorial xs)))

  (boo [2 4 5])

  (factorial 5)
  )
