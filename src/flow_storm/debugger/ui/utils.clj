(ns flow-storm.debugger.ui.utils
  (:import [javafx.scene.control ContextMenu MenuItem ScrollPane]
           [javafx.scene Node]))

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

(defn make-context-menu [items]
  (let [cm (ContextMenu.)
        cm-items (->> items
                      (map (fn [{:keys [text on-click]}]
                             (doto (MenuItem. text)
                               (.setOnAction (event-handler [_] (on-click)))))))]
    (-> cm
        .getItems
        (.addAll (into-array MenuItem cm-items)))
    cm))

(defn center-node-in-scroll-pane [^ScrollPane scroll-pane ^Node node]
  (let [h (-> scroll-pane .getContent .getBoundsInLocal .getHeight)
        y (/ (+ (-> node .getBoundsInParent .getMaxY)
                (-> node .getBoundsInParent .getMinY))
             2.0)
        v (-> scroll-pane .getViewportBounds .getHeight)]
    (.setVvalue scroll-pane (* (.getVmax scroll-pane)
                               (/ (- y (* v 0.5))
                                  (- h v))))))
