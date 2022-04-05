(ns dev
  (:require [flow-storm.debugger.ui.main :as ui-main]
            [flow-storm.debugger.main :as dbg-main]
            [flow-storm.api :as fs-api]
            [flow-storm.tracer :as tracer]
            [clojure.tools.namespace.repl :refer [refresh]]
            [clojure.pprint :as pp]
            [clj-async-profiler.core :as prof]
            [dev-tester]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities for reloading everything ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn start-and-add-data [& _]

  ;; this will restart the debugger (state and ui), the send-thread and the trace-queue
  (fs-api/local-connect)

  (fs-api/trace-files-for-namespaces #{"dev-tester"}
                                     {:disable #{} #_#{:expr :anonymous-fn :binding}})

  ;; add some data for dev
  (fs-api/run-with-execution-ctx
   {:flow-id 0
    :ns "dev"}
   (dev-tester/boo [2 "hello" 8]))

  )

(add-tap (bound-fn* pp/pprint))

(defn local-restart-everything []
  (tracer/stop-send-thread)
  (ui-main/close-stage)

  ;; reload all namespaces
  (refresh :after 'dev/start-and-add-data))



(Thread/setDefaultUncaughtExceptionHandler
   (reify
     Thread$UncaughtExceptionHandler
     (uncaughtException [this thread throwable]
       (tap> (str "Unhandled exception " thread throwable))
       (tap> throwable))))
