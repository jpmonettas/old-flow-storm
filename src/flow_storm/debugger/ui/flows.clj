(ns flow-storm.debugger.ui.flows
  (:require [flow-storm.debugger.ui.state-vars :refer [scene-lookup]]
            [flow-storm.debugger.ui.utils :as ui-utils :refer [event-handler run-later run-now]])
  (:import [javafx.scene.layout BorderPane GridPane HBox Pane VBox]
           [javafx.scene.control Label Tab TabPane TabPane$TabClosingPolicy SplitPane]
           [javafx.geometry Side Orientation]))

(defn create-empty-flow [flow-id]
  (run-now
   (let [flows-tabs-pane (scene-lookup "#flows_tabs_pane")
         threads-tab-pane (doto (TabPane.)
                            (.setId (str "threads_tabs_pane_" flow-id))
                            (.setTabClosingPolicy TabPane$TabClosingPolicy/UNAVAILABLE))
         flow-tab (doto (Tab. (str "flow-" flow-id))
                    (.setContent threads-tab-pane))]
     (-> flows-tabs-pane
         .getTabs
         (.addAll [flow-tab])))))

(defn create-forms-pane []
  (Label. "FORMS"))

(defn create-result-pane []
  (Label. "Result"))

(defn create-locals-pane []
  (Label. "Locals"))

(defn create-code-pane []
  (let [left-right-pane (doto (SplitPane.)
                          (.setOrientation (Orientation/HORIZONTAL)))
        locals-result-pane (doto (SplitPane.)
                             (.setOrientation (Orientation/VERTICAL)))
        forms-pane (create-forms-pane)
        result-pane (create-result-pane)
        locals-pane (create-locals-pane)]

    (-> locals-result-pane
        .getItems
        (.addAll [result-pane locals-pane]))
    (-> left-right-pane
        .getItems
        (.addAll [forms-pane locals-result-pane]))
    left-right-pane))

(defn create-call-stack-tree-pane []
  (Label. "Call stack tree"))

(defn create-thread-pane [flow-id thread-id]
  (let [thred-tools-tab-pane (doto (TabPane.)
                               (.setTabClosingPolicy TabPane$TabClosingPolicy/UNAVAILABLE)
                               (.setSide (Side/BOTTOM)))
        code-tab (doto (Tab. "Code")
                   (.setContent (create-code-pane)))
        callstack-tree-tab (doto (Tab. "Call stack")
                             (.setContent (create-call-stack-tree-pane)))]
    (-> thred-tools-tab-pane
        .getTabs
        (.addAll [code-tab callstack-tree-tab]))
    thred-tools-tab-pane))

(defn create-empty-thread [flow-id thread-id]
  (run-now
   (let [threads-tabs-pane (scene-lookup (str "#threads_tabs_pane_" flow-id))
         _ (tap> (str "Searching for id " (str "#threads_tabs_pane_" flow-id) " GOT " threads-tabs-pane))
         thread-tab-pane (create-thread-pane flow-id thread-id)
         thread-tab (doto (Tab. (str "thread-" thread-id))
                      (.setContent thread-tab-pane))]
     (-> threads-tabs-pane
           .getTabs
           (.addAll [thread-tab])))))

(defn main-pane []
  (doto (TabPane.) ;;TODO: make flows closable
    (.setId "flows_tabs_pane")
    (.setTabClosingPolicy TabPane$TabClosingPolicy/UNAVAILABLE)))
