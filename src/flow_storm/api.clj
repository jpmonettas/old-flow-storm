(ns flow-storm.api
  "This is the only namespace intended for users.
  Provides functionality to connect to the debugger and instrument forms."
  (:require [flow-storm.instrument.forms :as inst-forms]
            [flow-storm.instrument.namespaces :as inst-ns]
            [flow-storm.tracer :as tracer]
            [flow-storm.debugger.trace-processor :as trace-processor]
            [flow-storm.debugger.main :as dbg-main]
            [clojure.pprint :as pp]
            [cljs.main :as cljs-main]
            [clojure.repl :as clj.repl]
            [cljs.repl :as cljs.repl]
            [clojure.instant :as inst]))

(def start-debugger dbg-main/start-debugger)

(defn local-connect []
  (start-debugger)
  (tracer/connect {:send-fn (fn [trace]
                              (try
                                (trace-processor/dispatch-trace trace)
                                (catch Exception e
                                  (tap> (str "Exception dispatching trace " (.getMessage e)))
                                  (tap> e))))}))

(defn ws-connect [opts]
  (let [{:keys [send-fn]} (tracer/build-ws-sender opts)]
    (tracer/connect {:send-fn send-fn})))

(defn file-connect [opts]
  (let [{:keys [send-fn]} (tracer/build-file-sender opts)]
    (tracer/connect {:send-fn send-fn})))


#_(def trace-ref
  "Adds a watch to ref with ref-name that traces its value changes.
  The first argument is the ref to watch for.
  The second argument is a options map. Available options are :
  - :ref-name A string name for the ref.
  - :ignore-keys A collection of keys that will be skipped in traces.

  :ignore-keys only works for maps and does NOT ignore nested maps keys."
  tracer/trace-ref)

#_(def untrace-ref
  "Removes the watch added by trace-ref."
  tracer/untrace-ref)

(defn- pprint-on-err [x]
  (binding [*out* *err*] (pp/pprint x)))

(defmacro trace
  "Recursively instrument a form for tracing."
  ;; TODO: make it possible with the trace macro to set a flow id
  ([form] `(trace {:disable #{}} ~form)) ;; need to do this so multiarity macros work
  ([config form]
   (let [form-ns (str (ns-name *ns*))
         ctx (inst-forms/build-form-instrumentation-ctx config form-ns form &env)
         inst-code (-> form
                       (inst-forms/instrument-all ctx)
                       (inst-forms/maybe-unwrap-outer-form-instrumentation ctx))]

     ;; Uncomment to debug
     ;; Printing on the *err* stream is important since
     ;; printing on standard output messes  with clojurescript macroexpansion
     #_(pprint-on-err (inst-forms/macroexpand-all form))
     #_(pprint-on-err inst-code)

     inst-code)))

(defn trace-var [var-symb config]
  (let [form (some-> (clj.repl/source-fn var-symb)
                     read-string)]
    (if form

      (inst-ns/trace-form (find-ns (symbol (namespace var-symb))) form config)

      (println "Couldn't find source for " var-symb))))

(defn untrace-var [var-symb]
  (let [form (some->> (clj.repl/source-fn var-symb)
                      (read-string {:read-cond :allow}))
        expanded-form (inst-forms/macroexpand-all macroexpand-1 form ::original-form)]
    (if form

      (if (inst-forms/expanded-def-form? expanded-form)
        (let [ns-name (namespace var-symb)
              [v vval] (inst-ns/expanded-defn-parse ns-name expanded-form)]
          (binding [*ns* (find-ns (symbol ns-name))]
            (alter-var-root v (fn [_] (eval vval)))))
        (tap> (format "Don't know howto untrace %s" (pr-str expanded-form))))

      (println "Couldn't find source for " var-symb))))

(def trace-files-for-namespaces inst-ns/trace-files-for-namespaces)

(defn read-trace-tag [form]
  `(flow-storm.api/trace ~form))

(defn read-ztrace-tag [form]
  `(flow-storm.api/trace 0 ~form))

(defmacro run-with-execution-ctx
  [{:keys [orig-form ns flow-id]} form]
  `(let [flow-id# ~(or flow-id 0)
         curr-ns# ~(or ns `(str (ns-name *ns*)))]
     (alter-var-root #'tracer/*init-traced-forms* (constantly (atom #{})))
     (alter-var-root #'tracer/*flow-id* (constantly flow-id#))
     (tracer/trace-flow-init-trace flow-id# curr-ns# ~(or orig-form (list 'quote form)))
     ~form))

(defn re-run-flow [flow-id {:keys [ns form]}]
  (binding [*ns* (find-ns (symbol ns))]
    (run-with-execution-ctx
     {:flow-id flow-id
      :orig-form form}
     (eval form))))

(comment

  (local-connect)

  (trace {:flow-id 0 :disable #{}}
   (defn factorial [n]
     (if (zero? n)
       1
       (* n (factorial (dec n))))))


  (trace {:flow-id 0 :disable #{}}
   (defn boo [xs]
     (reduce + (map factorial xs))))

  (run-with-execution-ctx
   {}
   (factorial 5)
   #_(boo [2 3 4]))

  (flow-storm.debugger.state/init-state!)
  flow-storm.debugger.state/*state
  )

;; Run with : clj -X flow-storm.api/cljs-test
#_(defn cljs-test [& args]

  (local-connect)

  (time
   (trace-files-for-namespaces "cljs." {:disable #{:expr :binding}})
   )

  (time
   (run-with-execution-ctx
    {:flow-id 0}
    (cljs-main/-main "-t" "nodejs" "/home/jmonetta/tmp/cljstest/foo/script.cljs")))
    )
