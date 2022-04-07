(ns flow-storm.api
  "This is the only namespace intended for users.
  Provides functionality to connect to the debugger and instrument forms."
  (:require [flow-storm.tracer :as tracer]
            [flow-storm.instrument.namespaces :as inst-ns]
            [flow-storm.commands :as commands]))

(defn local-connect

  "Start a debugger under this same JVM process and connect to it.

  This is the recommended way of using the debugger for debugging code that
  generates a lot of data since data doesn't need to serialize/deserialize it like
  in a remote debugging session case."

  []

  (require '[flow-storm.debugger.trace-processor])
  (require '[flow-storm.debugger.main])
  (let [local-dispatch-trace (resolve 'flow-storm.debugger.trace-processor/dispatch-trace)
        start-debugger       (resolve 'flow-storm.debugger.main/start-debugger)]
    (start-debugger)
    (tracer/connect {:send-fn (fn [trace]
                                (try
                                  (local-dispatch-trace trace)
                                  (catch Exception e
                                    (tap> (str "Exception dispatching trace " (.getMessage e)))
                                    (tap> e))))})))

(def instrument-var

  "Instruments any var.

  (instrument-var var-symb opts)

  Lets say you are interested in debugging clojure.core/interpose you can do :

  (instrument-var clojure.core/interpose {})

  #rtrace (interpose :a [1 2 3])

  Be careful instrumenting clojure.core functions or any functions that are being used
  by repl system code since can be called constantly and generate a lot of noise.

  Use `uninstrument-var` to remove instrumentation.

  `opts` is a map that support :flow-id and :disable
  See `instrument-forms-for-namespaces` for :disable"

  commands/trace-var)

(def uninstrument-var

  "Remove instrumentation given a var symbol.

  (uninstrument-var var-symb)"

  commands/untrace-var)

(def uninstrument-vars

  "Bulk version of `uninstrument-var`.

  (uninstrument-vars var-symbols)"

  commands/untrace-vars)

;; TODO: deduplicate code between `run` and `runi`
(defn- run*
  ([form] `(run {} ~form))
  ([{:keys [ns flow-id]} form]
   `(let [flow-id# ~(or flow-id 0)
          curr-ns# ~(or ns `(str (ns-name *ns*)))]
      (binding [tracer/*runtime-ctx* (tracer/empty-runtime-ctx flow-id#)]
        (tracer/trace-flow-init-trace flow-id# curr-ns# ~(list 'quote form))
        ~form))))

(defmacro run

  "Start a flow and run form for tracing.
  Will setup the execution context so all instrumented functions tracing
  derived from running `form` will end under the same flow.

  (run opts form)

  `opts` is a map that support the same keys as `instrument-var`."

  [& args] (apply run* args))

(defn- runi*
  ([form] `(runi {} ~form))
  ([{:keys [ns flow-id] :as opts} form]
   `(let [flow-id# ~(or flow-id 0)
          curr-ns# ~(or ns `(str (ns-name *ns*)))]
      (binding [tracer/*runtime-ctx* (tracer/empty-runtime-ctx flow-id#)]
        (tracer/trace-flow-init-trace flow-id# curr-ns# ~(list 'quote form))
        ((commands/trace ~opts (fn [] ~form)))))))

(defmacro runi

  "Run instrumented.

  (runi opts form)

  Instrument form and run it for tracing.

  Same as doing #rtrace `form`.

  `opts` is a map that support the same keys as `instrument-var`. "

  [& args] (apply runi* args))

(def instrument-forms-for-namespaces

  "Instrument all forms, in all namespaces that matches `prefixes`.

  (instrument-forms-for-namespaces prefixes opts)

  `prefixes` is a set of ns prefixes like #{\"cljs.compiler\" \"cljs.analyzer\"}

  `opts` is a map containing :
       - :excluding-ns  a set of strings with namespaces that should be excluded
       - :disable is a set containing any of #{:expr :binding :anonymous-fn}
                  useful for disabling unnecesary traces in code that generate too many traces
  "



  inst-ns/trace-files-for-namespaces)
