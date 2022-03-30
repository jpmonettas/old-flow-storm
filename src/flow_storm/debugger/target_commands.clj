(ns flow-storm.debugger.target-commands)

;; All this is now implemented by calling directly to flow-storm.api
;; but needs to be decoulpled so it can be used in remote debuggers

(defn run-command [command & params]
  (let [fn-symb (case command
                  :instrument-fn        'flow-storm.api/trace-var
                  :uninstrument-fn      'flow-storm.api/untrace-var
                  :uninstrument-fn-bulk 'flow-storm.api/untrace-vars
                  :eval-form-bulk       'flow-storm.api/eval-form-bulk
                  :instrument-form-bulk 'flow-storm.api/trace-form-bulk
                  :re-run-flow          'flow-storm.api/re-run-flow)
        f (resolve fn-symb)]
    ;; need to run this in a different thread so it doesn't block the UI thread
    (.start (Thread.
             (fn []
               (apply f params))))))
