(ns flow-storm.debugger.ui.main
  (:require [flow-storm.debugger.ui.utils :as ui-utils :refer [event-handler run-later]]
            [flow-storm.debugger.ui.styles :as styles]
            [flow-storm.debugger.ui.flows :as ui-flows]
            [flow-storm.debugger.ui.state-vars :refer [main-pane stage scene store-obj obj-lookup]])
  (:import [javafx.scene Scene]
           [javafx.stage Stage]
           [javafx.scene.layout BorderPane GridPane HBox Pane VBox]
           [javafx.scene.control Alert Alert$AlertType Button ButtonType ComboBox ContextMenu Dialog Label MenuItem TabPane TabPane$TabClosingPolicy Tab TableColumn TableView TextArea TextField]
           [javafx.geometry Side]
           [javafx.application Platform]))

(javafx.embed.swing.JFXPanel.)

(defn update-trace-counter [cnt]
  (run-later
   (-> (first (obj-lookup "trace_count_label"))
       (.setText (str cnt)))))

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
                       (.setContent (Label. "Timeline comming soon")))]
    (doto tabs-p
      (.setTabClosingPolicy TabPane$TabClosingPolicy/UNAVAILABLE)

      (.setRotateGraphic true)
      (.setSide (Side/LEFT)))

    (.addAll tabs [flows-tab refs-tab taps-tab timeline-tab])

    tabs-p))

(defn close-stage []
  (when stage
    (ui-utils/run-now
     (.close stage))))

(defn build-main-pane []
  (let [mp (doto (BorderPane.)
                    #_(.setStyle styles/backpane)
                    #_(.setTop (doto (Button. "Click me")
                               (.setOnAction (event-handler [ev] (println "Clicked")))))
                    (.setCenter (main-tabs-pane))
                    (.setBottom (trace-counter-box)))]


    (let [ctx-menu (make-context-menu [{:text "Menu1" :on-click #(println "Menu1")}
                                       {:text "Quit" :on-click  #(close-stage)}])]
      (.setOnContextMenuRequested mp
                                  (event-handler
                                   [ev]
                                   (.show ctx-menu
                                          mp
                                          (.getScreenX ev)
                                          (.getScreenY ev)))))
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
                            [ke]
                            (let [key (.getName (.getCode ke))]
                              (println "Key pressed" key)))))

       (alter-var-root #'scene (constantly scene))
       (alter-var-root #'stage (constantly (doto (Stage.)
                                             (.setTitle "Flowstorm debugger")
                                             (.setScene scene)))))

     (reset-scene-main-pane)

     (-> stage .show)

     (catch Exception e
       (tap> (str "UI Thread exception"
                  (with-out-str (.printStackTrace e))))))))

(defn -main
  ""
  [& args]

  )

(comment

  )
