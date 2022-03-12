(ns flow-storm.debugger.ui.main
  (:require [flow-storm.debugger.ui.utils :as ui-utils :refer [event-handler]]
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

(def state (atom 0))

(defn button-click-action []
  (swap! state inc)
  (-> (.lookup scene "#counter_label")
      (.setText (str @state)))
  )

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

(defn build-main-pane []
  (doto (BorderPane.)
    (.setTop (doto (Button. "Click me")
               (.setOnAction (event-handler [ev] (button-click-action)))))
    (.setCenter (doto (Label. (str "Helloooooo WOOOOOOOOOOORLD" @state))
                  (.setId "counter_label")
                  ))))

(defn rebuild-main-pane-content []
  (let [children (.getChildren main-pane)]
    (doto children
      (.clear)
      (.add (build-main-pane)))))

(defn -main
  ""
  [& args]

  ;; Initialize the JavaFX toolkit
  (javafx.embed.swing.JFXPanel.)

  (alter-var-root #'ctx-menu
                  (constantly (make-context-menu [{:text "Menu1" :on-click #(println "Menu1")}
                                                  {:text "Quit" :on-click  #(do
                                                                              (println "Bye bye")
                                                                              (System/exit 0))}])))

  (alter-var-root #'main-pane (constantly (doto (Pane.)
                                            (.setStyle styles/backpane))))

  (rebuild-main-pane-content)

  (.setOnContextMenuRequested main-pane
                               (event-handler
                                [ev]
                                (.show ctx-menu
                                       main-pane
                                       (.getScreenX ev)
                                       (.getScreenY ev))))

  (ui-utils/run-now

   (let [scene (Scene. main-pane 800 600)]
     (doto scene
       (.setOnKeyPressed (event-handler
                          [ke]
                          (let [key (.getName (.getCode ke))]
                            (println "Key pressed" key)))))

     (alter-var-root #'scene (constantly scene))
     (alter-var-root #'stage (constantly (doto (Stage.)
                                           (.setTitle "Flowstorm debugger")
                                           (.setScene scene)))))

   (-> stage .show)))


(comment
  (ui-utils/run-now (rebuild-main-pane-content))
  )
