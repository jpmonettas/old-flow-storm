(ns flow-storm.debugger.ui.flows
  (:require [flow-storm.debugger.ui.state-vars :refer [store-obj obj-lookup]]
            [flow-storm.debugger.ui.utils :as ui-utils :refer [event-handler run-later run-now]])
  (:import [javafx.scene.layout BorderPane GridPane HBox Pane VBox]
           [javafx.scene.control Label Tab TabPane TabPane$TabClosingPolicy SplitPane]
           [javafx.scene.text TextFlow Text]
           [javafx.scene Node]
           [javafx.geometry Side Orientation]))

(defn create-empty-flow [flow-id]
  (run-now
   (let [flows-tabs-pane (obj-lookup "flows_tabs_pane")
         threads-tab-pane (doto (TabPane.)
                            (.setTabClosingPolicy TabPane$TabClosingPolicy/UNAVAILABLE))
         _ (store-obj flow-id "threads_tabs_pane" threads-tab-pane)
         flow-tab (doto (Tab. (str "flow-" flow-id))
                    (.setContent threads-tab-pane))]
     (-> flows-tabs-pane
         .getTabs
         (.addAll [flow-tab])))))

(defn- create-forms-pane [flow-id thread-id]
  (let [box (doto (VBox.)
              (.setSpacing 1))]
    (store-obj flow-id (format "forms_box_%d" thread-id) box)
    box))

(defn- create-result-pane []
  (Label. "Result"))

(defn- create-locals-pane []
  (Label. "Locals"))

(defn- create-code-pane [flow-id thread-id]
  (let [left-right-pane (doto (SplitPane.)
                          (.setOrientation (Orientation/HORIZONTAL)))
        locals-result-pane (doto (SplitPane.)
                             (.setOrientation (Orientation/VERTICAL)))
        forms-pane (create-forms-pane flow-id thread-id)
        result-pane (create-result-pane)
        locals-pane (create-locals-pane)]

    (-> locals-result-pane
        .getItems
        (.addAll [result-pane locals-pane]))
    (-> left-right-pane
        .getItems
        (.addAll [forms-pane locals-result-pane]))
    left-right-pane))

(defn- create-call-stack-tree-pane []
  (Label. "Call stack tree"))

(defn- create-thread-pane [flow-id thread-id]
  (let [thred-tools-tab-pane (doto (TabPane.)
                               (.setTabClosingPolicy TabPane$TabClosingPolicy/UNAVAILABLE)
                               (.setSide (Side/BOTTOM)))
        code-tab (doto (Tab. "Code")
                   (.setContent (create-code-pane flow-id thread-id)))
        callstack-tree-tab (doto (Tab. "Call stack")
                             (.setContent (create-call-stack-tree-pane)))]
    (-> thred-tools-tab-pane
        .getTabs
        (.addAll [code-tab callstack-tree-tab]))
    thred-tools-tab-pane))

(defn create-empty-thread [flow-id thread-id]
  (run-now
   (let [threads-tabs-pane (obj-lookup flow-id "threads_tabs_pane")
         thread-tab-pane (create-thread-pane flow-id thread-id)
         thread-tab (doto (Tab. (str "thread-" thread-id))
                      (.setContent thread-tab-pane))]
     (-> threads-tabs-pane
           .getTabs
           (.addAll [thread-tab])))))

(defn add-form [flow-id thread-id form-id form-ns print-tokens]
  (run-now
   (tap> (str "Add form id " form-id))
   (let [forms-box (obj-lookup flow-id (format "forms_box_%d" thread-id))
         tokens-texts (->> print-tokens
                           (map (fn [tok]
                                  (Text.
                                   (case tok
                                     :nl   "\n"
                                     :sp   " "
                                     (first tok))))))
         form-text-flow (doto (TextFlow. (into-array Text tokens-texts))
                          (.setStyle "-fx-padding: 10"))]

     (-> forms-box
         .getChildren
         (.addAll [form-text-flow])))))

(defn main-pane []

  (let [tab-pane (doto (TabPane.) ;;TODO: make flows closable
                   (.setTabClosingPolicy TabPane$TabClosingPolicy/UNAVAILABLE))]
    (store-obj "flows_tabs_pane" tab-pane)
    tab-pane))
