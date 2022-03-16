(ns flow-storm.debugger.ui.utils)

(defn run-later*
  [f]
  (javafx.application.Platform/runLater f))

(defmacro run-later
  [& body]
  `(run-later* (fn []
                 (try
                   ~@body
                   (catch Exception e#
                     (tap> (str "Exception in UI thread @1 " (.getMessage e#)))
                     (tap> e#))))))

(defn run-now*
  [f]
  (let [result (promise)]
    (run-later
     (deliver result (try (f)
                          (catch Exception e
                            (tap> (str "Exception in UI thread @2" (.getMessage e)))
                            (tap> e)
                            (tap> (.getCause e))))))
    @result))

(defmacro run-now
  [& body]
  `(run-now* (fn [] ~@body)))

(defn event-handler*
  [f]
  (reify javafx.event.EventHandler
    (handle [this e] (f e))))

(defmacro event-handler [arg & body]
  `(event-handler* (fn ~arg ~@body)))
