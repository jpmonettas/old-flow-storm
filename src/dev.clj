(ns dev
  (:require [flow-storm.debugger.ui.main :as ui-main]
            [flow-storm.debugger.main :as dbg-main]
            [flow-storm.api :as fs-api]
            [flow-storm.tracer :as tracer]
            [clojure.tools.namespace.repl :refer [refresh]]))

;;;;;;;;;;;;;;;;;;;;;;;
;; Some testing code ;;
;;;;;;;;;;;;;;;;;;;;;;;
(fs-api/trace
 {:flow-id 0 :disable #{}}
 (defn factorial [n]
   (if (zero? n)
     1
     (* n (factorial (dec n))))))


(fs-api/trace
 {:flow-id 0 :disable #{}}
 (defn boo [xs]
   (reduce + (map factorial xs))))

(defn run-some-traced []
  (fs-api/run-with-execution-ctx
   {}
   (factorial 5)
   #_(boo [2 3 4])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities for reloading everything ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-tap (bound-fn* clojure.pprint/pprint))

(defn start-and-add-data []
  ;; this will restart the debugger (state and ui), the send-thread and the trace-queue
  (fs-api/local-connect)

  ;; add some data for dev
  (run-some-traced))

(defn local-restart-everything []
  (tracer/stop-send-thread)
  (ui-main/close-stage)

  ;; reload all namespaces
  (refresh :after 'dev/start-and-add-data))
