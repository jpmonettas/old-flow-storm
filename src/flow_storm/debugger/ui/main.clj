(ns flow-storm.debugger.ui.main
  (:require [flow-storm.debugger.ui.utils :as ui-utils :refer [event-handler run-later]]
            [flow-storm.debugger.ui.styles :as styles]
            [flow-storm.debugger.ui.flows :as ui-flows]
            [flow-storm.debugger.ui.state-vars :refer [main-pane stage scene store-obj obj-lookup]]
            [flow-storm.debugger.ui.state-vars :as state-vars])
  (:import [javafx.scene Scene]
           [javafx.stage Stage]
           [javafx.scene.layout BorderPane GridPane HBox Pane VBox]
           [javafx.scene.control Alert Alert$AlertType Button ButtonType ComboBox ContextMenu Dialog Label MenuItem TabPane TabPane$TabClosingPolicy Tab TableColumn TableView TextArea TextField]
           [javafx.geometry Side]
           [javafx.application Platform]))

(javafx.embed.swing.JFXPanel.)

(defn update-trace-counter [cnt]
  (let [[^Label trace-cnt-lbl] (obj-lookup "trace_count_label")]
    (run-later
     (.setText trace-cnt-lbl (str cnt)))))

(defn trace-counter-box []
  (let [box (HBox.) ; spacing
        trace-cnt-label (Label. "0")]
    (store-obj "trace_count_label" trace-cnt-label)
    (-> box
        .getChildren
        (.addAll [(Label. "Processed traces:")
                  trace-cnt-label]))
    box))

(defn main-tabs-pane []
  (let [tabs-p (TabPane.)
        tabs (.getTabs tabs-p)
        flows-tab (doto (Tab. "Flows")
                    (.setContent (ui-flows/main-pane)))
        refs-tab (doto (Tab. "Refs")
                   (.setContent (Label. "Refs comming soon")))
        taps-tab (doto (Tab. "Taps")
                   (.setContent (Label. "Taps comming soon")))
        timeline-tab (doto (Tab. "Timeline")
                       (.setContent (Label. "Timeline comming soon")))
        browser-tab (doto (Tab. "Browser")
                       (.setContent (Label. "Browser comming soon")))
        docs-tab (doto (Tab. "Docs")
                       (.setContent (Label. "Docs comming soon")))]
    (doto tabs-p
      (.setTabClosingPolicy TabPane$TabClosingPolicy/UNAVAILABLE)

      (.setRotateGraphic true)
      (.setSide (Side/LEFT)))

    (.addAll tabs [flows-tab refs-tab taps-tab
                   timeline-tab browser-tab docs-tab])

    tabs-p))

(defn close-stage []
  (when stage
    (ui-utils/run-now
     (.close stage))))

(defn build-main-pane []
  (let [mp (doto (BorderPane.)
             (.setCenter (main-tabs-pane))
             (.setBottom (trace-counter-box)))]
    mp))

(defn reset-scene-main-pane []
  (let [mp (build-main-pane)]
    (alter-var-root #'main-pane (constantly mp))
    (.setRoot scene mp)))

(defn start-ui []
  ;; Initialize the JavaFX toolkit
  (javafx.embed.swing.JFXPanel.)
  (Platform/setImplicitExit false)

  (ui-utils/run-now
   (try
     (let [scene (Scene. (build-main-pane) 1024 768)]
       (doto scene
         (.setOnKeyPressed (event-handler
                            [kev]
                            (let [key-name (.getName (.getCode kev))]
                              (cond
                                (and (.isControlDown kev)
                                     (= key-name "G"))
                                (do
                                  (tap> "Interrupting long running task")
                                  (.interrupt @state-vars/long-running-task-thread))

                                :else
                                (tap> (format "Unhandled keypress %s" key-name)))))))

       (alter-var-root #'scene (constantly scene))
       (alter-var-root #'stage (constantly (doto (Stage.)
                                             (.setTitle "Flowstorm debugger")
                                             (.setScene scene)))))

     (reset-scene-main-pane)

     (-> stage .show)

     (catch Exception e
       (tap> (str "UI Thread exception"
                  (with-out-str (.printStackTrace e))))))))
