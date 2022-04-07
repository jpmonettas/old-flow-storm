(ns flow-storm.debugger.target-commands)

;; All this is now implemented by calling directly to flow-storm.commands
;; but needs to be decoulpled so it can be used in remote debuggers

(defn run-command [command & params]
  (let [fn-symb (case command
                  :instrument-fn        'flow-storm.commands/trace-var
                  :uninstrument-fn      'flow-storm.commands/untrace-var
                  :uninstrument-fn-bulk 'flow-storm.commands/untrace-vars
                  :eval-form-bulk       'flow-storm.commands/eval-form-bulk
                  :instrument-form-bulk 'flow-storm.commands/trace-form-bulk
                  :re-run-flow          'flow-storm.commands/re-run-flow)
        f (resolve fn-symb)]
    ;; need to run this in a different thread so it doesn't block the UI thread
    (.start (Thread.
             (fn []
               (apply f params))))))
