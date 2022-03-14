(ns flow-storm.debugger.ui.main
  (:require [flow-storm.debugger.ui.utils :as ui-utils :refer [event-handler run-later]]
            [flow-storm.debugger.ui.styles :as styles])
  (:import [javafx.scene Scene]
           [javafx.stage Stage]
           [javafx.scene.layout BorderPane GridPane HBox Pane VBox]
           [javafx.scene.control Alert Alert$AlertType Button ButtonType ComboBox ContextMenu Dialog Label MenuItem TableColumn TableView TextArea TextField]))

(javafx.embed.swing.JFXPanel.)

(defonce main-pane nil)
(defonce ctx-menu nil)
(defonce scene nil)
(defonce stage nil)

(defn update-trace-counter [cnt]
  (run-later
   (-> (.lookup scene "#trace_count_label")
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
        trace-cnt-label (doto (Label. "0")
                          (.setId "trace_count_label"))]
    (-> box
        .getChildren
        (.addAll [(Label. "Processed traces:")
                  trace-cnt-label]))
    box))

(defn build-main-pane []
  (let [mp (doto (BorderPane.)
                    #_(.setStyle styles/backpane)
                    (.setTop (doto (Button. "Click me")
                               (.setOnAction (event-handler [ev] (println "Clicked")))))
                    (.setCenter (doto (Pane.)
                                  (.setStyle "-fx-background-color: red;")))
                    (.setBottom (trace-counter-box)))]

    #_(.setOnContextMenuRequested mp
                                (event-handler
                                 [ev]
                                 (.show ctx-menu
                                        mp
                                        (.getScreenX ev)
                                        (.getScreenY ev))))
    mp))

(defn reset-scene-main-pane []
  (let [mp (build-main-pane)]
    (alter-var-root #'main-pane (constantly mp))
    (.setRoot scene mp)))

(defn start-ui []
  ;; Initialize the JavaFX toolkit
  (javafx.embed.swing.JFXPanel.)

  (alter-var-root #'ctx-menu
                  (constantly (make-context-menu [{:text "Menu1" :on-click #(println "Menu1")}
                                                  {:text "Quit" :on-click  #(do
                                                                              (println "Bye bye")
                                                                              (System/exit 0))}])))

  (ui-utils/run-now

   (let [scene (Scene. (build-main-pane) 800 600)]
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

   (-> stage .show)))

(defn -main
  ""
  [& args]

  )

(defn refresh-ui [] (ui-utils/run-now (reset-scene-main-pane)))

(comment

  )
