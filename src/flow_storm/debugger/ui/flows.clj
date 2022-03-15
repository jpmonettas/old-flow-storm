(ns flow-storm.debugger.ui.flows
  (:require [flow-storm.debugger.ui.state-vars :refer [scene-lookup]]
            [flow-storm.debugger.ui.utils :as ui-utils :refer [event-handler run-later]])
  (:import [javafx.scene.layout BorderPane GridPane HBox Pane VBox]
           [javafx.scene.control Label Tab TabPane TabPane$TabClosingPolicy]))

(defn create-empty-flow [flow-id]
  (run-later
   (let [tabs-pane (scene-lookup "#flows_tabs_pane")
         flow-tab (doto (Tab. (str "flow-" flow-id))
                    (.setId (str "flow-tab-" flow-id)))]
     (-> tabs-pane
         .getTabs
         (.addAll [flow-tab])))))

(defn main-pane []
  (doto (TabPane.) ;;TODO: make flows closable
    (.setId "flows_tabs_pane")
    (.setTabClosingPolicy TabPane$TabClosingPolicy/UNAVAILABLE)))
