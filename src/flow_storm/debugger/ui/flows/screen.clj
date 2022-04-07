(ns flow-storm.debugger.ui.flows.screen
  (:require [flow-storm.debugger.ui.flows.code :as flow-code]
            [flow-storm.debugger.ui.flows.call-tree :as flow-tree]
            [flow-storm.debugger.ui.flows.functions :as flow-fns]
            [flow-storm.debugger.ui.state-vars :refer [store-obj obj-lookup] :as ui-vars]
            [flow-storm.debugger.ui.utils :as ui-utils :refer [event-handler run-later run-now v-box h-box label]]
            [flow-storm.debugger.trace-indexer.protos :as indexer]
            [clojure.pprint :as pp]
            [flow-storm.debugger.state :as state :refer [dbg-state]]
            [flow-storm.debugger.form-pprinter :as form-pprinter]
            [clojure.string :as str]
            [flow-storm.utils :as utils]
            [flow-storm.debugger.target-commands :as target-commands])
  (:import [javafx.scene.layout BorderPane Background BackgroundFill CornerRadii GridPane Priority Pane HBox VBox]
           [javafx.scene.control Button CheckBox ComboBox Label ListView ListCell ScrollPane SelectionMode SelectionModel
            TreeCell TextArea TextField Tab TabPane TabPane$TabClosingPolicy Tooltip TreeView TreeItem  SplitPane]
           [javafx.scene.text TextFlow Text Font]
           [ javafx.beans.value ChangeListener]
           [javafx.scene Node]
           [javafx.scene.paint Color]
           [javafx.util StringConverter]
           [javafx.geometry Insets Side Orientation Pos]
           [javafx.collections FXCollections ObservableList]
           [javafx.scene.input MouseEvent MouseButton]))

(defn remove-flow [flow-id]
  (let [[^TabPane flows-tabs-pane] (obj-lookup "flows_tabs_pane")
        [flow-tab] (obj-lookup flow-id "flow_tab")]

    (when flow-tab
      ;; remove the tab from flows_tabs_pane
      (-> flows-tabs-pane
          .getTabs
          (.remove flow-tab)))

    ;; clean ui state vars
    (ui-vars/clean-flow-objs flow-id)))

(defn create-empty-flow [flow-id]
  (let [[^TabPane flows-tabs-pane] (obj-lookup "flows_tabs_pane")
        threads-tab-pane (doto (TabPane.)
                           (.setTabClosingPolicy TabPane$TabClosingPolicy/UNAVAILABLE))
        _ (store-obj flow-id "threads_tabs_pane" threads-tab-pane)
        flow-tab (doto (Tab. (str "flow-" flow-id))
                   (.setOnCloseRequest (event-handler
                                        [ev]
                                        (state/remove-flow dbg-state flow-id)
                                        (remove-flow flow-id)
                                        ;; since we are destroying this tab, we don't need
                                        ;; this event to propagate anymore
                                        (.consume ev)))
                   (.setContent threads-tab-pane))]
    (store-obj flow-id "flow_tab" flow-tab)
    (-> flows-tabs-pane
        .getTabs
        (.addAll [flow-tab]))))

(defn- create-thread-controls-pane [flow-id thread-id]
  (let [prev-btn (doto (ui-utils/icon-button "mdi-chevron-left")
                   (.setOnAction (event-handler
                                  [ev]
                                  (flow-code/jump-to-coord flow-id
                                                 thread-id
                                                 (dec (state/current-trace-idx dbg-state flow-id thread-id))))))
        curr-trace-lbl (label "0")
        separator-lbl (label "/")
        thread-trace-count-lbl (label "-")
        _ (store-obj flow-id (ui-vars/thread-curr-trace-lbl-id thread-id) curr-trace-lbl)
        _ (store-obj flow-id (ui-vars/thread-trace-count-lbl-id thread-id) thread-trace-count-lbl)
        next-btn (doto (ui-utils/icon-button "mdi-chevron-right")
                   (.setOnAction (event-handler
                                  [ev]
                                  (flow-code/jump-to-coord flow-id
                                                 thread-id
                                                 (inc (state/current-trace-idx dbg-state flow-id thread-id))))))
        re-run-flow-btn (doto (ui-utils/icon-button "mdi-reload")
                          (.setOnAction (event-handler
                                         [_]
                                         (let [{:keys [flow/execution-expr]} (state/get-flow dbg-state flow-id)]
                                           (target-commands/run-command :re-run-flow flow-id execution-expr)))))
        trace-pos-box (doto (h-box [curr-trace-lbl separator-lbl thread-trace-count-lbl] "trace-position-box")
                        (.setSpacing 2.0))
        controls-box (doto (h-box [prev-btn next-btn re-run-flow-btn])
                       (.setSpacing 2.0))]

    (doto (h-box [controls-box trace-pos-box] "thread-controls-pane")
      (.setSpacing 2.0))))

(defn- create-thread-pane [flow-id thread-id]
  (let [thread-pane (v-box [])
        thread-controls-pane (create-thread-controls-pane flow-id thread-id)
        thread-tools-tab-pane (doto (TabPane.)
                               (.setTabClosingPolicy TabPane$TabClosingPolicy/UNAVAILABLE)
                               (.setSide (Side/BOTTOM)))
        code-tab (doto (Tab. "Code")
                   (.setContent (flow-code/create-code-pane flow-id thread-id)))
        callstack-tree-tab (doto (Tab. "Call tree")
                             (.setContent (flow-tree/create-call-stack-tree-pane flow-id thread-id))
                             (.setOnSelectionChanged (event-handler [_] (flow-tree/update-call-stack-tree-pane flow-id thread-id))))
        instrument-tab (doto (Tab. "Functions")
                             (.setContent (flow-fns/create-functions-pane flow-id thread-id))
                             (.setOnSelectionChanged (event-handler [_] (flow-fns/update-functions-pane flow-id thread-id))))]

    ;; make thread-tools-tab-pane take the full height
    (-> thread-tools-tab-pane
        .prefHeightProperty
        (.bind (.heightProperty thread-pane)))

    (-> thread-tools-tab-pane
        .getTabs
        (.addAll [code-tab callstack-tree-tab instrument-tab]))

    (-> thread-pane
        .getChildren
        (.addAll [thread-controls-pane thread-tools-tab-pane]))

    thread-pane))

(defn create-empty-thread [flow-id thread-id]
  (run-now
   (let [[threads-tabs-pane] (obj-lookup flow-id "threads_tabs_pane")
         thread-tab-pane (create-thread-pane flow-id thread-id)
         thread-tab (doto (Tab. (str "thread-" thread-id))
                      (.setContent thread-tab-pane))]
     (-> threads-tabs-pane
           .getTabs
           (.addAll [thread-tab])))))

(defn main-pane []
  (let [tab-pane (doto (TabPane.)
                   (.setTabClosingPolicy TabPane$TabClosingPolicy/ALL_TABS))]
    (store-obj "flows_tabs_pane" tab-pane)
    tab-pane))
