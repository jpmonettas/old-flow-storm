(ns flow-storm.debugger.ui.flows.call-tree
  (:require [flow-storm.debugger.ui.flows.code :as flow-code]
            [flow-storm.debugger.ui.flows.components :as flow-cmp]
            [flow-storm.debugger.trace-indexer.protos :as indexer]
            [flow-storm.utils :refer [log]]
            [flow-storm.debugger.ui.state-vars :refer [store-obj obj-lookup] :as ui-vars]
            [flow-storm.debugger.state :as state :refer [dbg-state]]
            [flow-storm.debugger.ui.utils :as ui-utils :refer [event-handler v-box h-box label]])
  (:import [javafx.collections ObservableList]
           [javafx.scene.control SelectionModel TreeCell TextField  Tooltip TreeView TreeItem]
           [javafx.scene.input MouseEvent MouseButton]
           [ javafx.beans.value ChangeListener]
           [javafx.geometry Insets Pos]
           [javafx.scene.layout HBox Priority VBox]))

(defn update-call-stack-tree-pane [flow-id thread-id]
  (let [indexer (state/thread-trace-indexer dbg-state flow-id thread-id)
        lazy-tree-item (fn lazy-tree-item [tree-node]
                         (let [calls (indexer/callstack-tree-childs indexer tree-node)]
                           (proxy [TreeItem] [tree-node]
                             (getChildren []
                               (let [^ObservableList super-childrens (proxy-super getChildren)]
                                 (if (.isEmpty super-childrens)
                                   (let [new-children (->> calls
                                                           (remove (fn [child-node]
                                                                     (let [{:keys [fn-name fn-ns]} (indexer/callstack-node-frame indexer child-node)]
                                                                       (state/callstack-tree-hidden? dbg-state flow-id thread-id fn-name fn-ns))))
                                                           (map lazy-tree-item)
                                                           (into-array TreeItem))]
                                     (.setAll super-childrens new-children)
                                     super-childrens)
                                   super-childrens)))
                             (isLeaf [] (empty? calls)))))
        tree-root-node (indexer/callstack-tree-root indexer)
        root-item (lazy-tree-item tree-root-node)
        [tree-view] (obj-lookup flow-id (ui-vars/thread-callstack-tree-view-id thread-id))]

    (.setRoot ^TreeView tree-view root-item)))

(defn format-tree-fn-call-args [args-vec]
  (let [step-1 (flow-cmp/format-value-short args-vec)]
    (if (= \. (.charAt step-1 (dec (count step-1))))
      (subs step-1 1 (count step-1))
      (subs step-1 1 (dec (count step-1))))))

(defn- create-call-stack-tree-node [{:keys [call-trace-idx form-id fn-name fn-ns args]} flow-id thread-id]
  ;; Important !
  ;; this will be called for all visible tree nodes after any expansion
  ;; so it should be fast
  (if-not call-trace-idx
    (doto (ui-utils/icon-button "mdi-reload")
      (.setOnAction (event-handler
                     [_]
                     (update-call-stack-tree-pane flow-id thread-id))))

    (let [indexer (state/thread-trace-indexer dbg-state flow-id thread-id)
          {:keys [multimethod/dispatch-val form/def-kind]} (indexer/get-form indexer form-id)
          ns-lbl (label (str fn-ns "/") "fn-ns")
          fn-name-lbl (flow-cmp/def-kind-colored-label fn-name def-kind)
          args-lbl (label (str " " (format-tree-fn-call-args args)) "fn-args")
          fn-call-box (if dispatch-val
                        (h-box [(label "(") ns-lbl fn-name-lbl (label (str dispatch-val)) args-lbl (label ")")])
                        (h-box [(label "(") ns-lbl fn-name-lbl args-lbl (label ")")]))
          ctx-menu-options [{:text (format "Goto trace %d" call-trace-idx)
                             :on-click #(flow-code/jump-to-coord flow-id thread-id call-trace-idx)}
                            {:text (format "Hide %s/%s from this tree" fn-ns fn-name)
                             :on-click #(do
                                          (state/callstack-tree-hide-fn dbg-state flow-id thread-id fn-name fn-ns)
                                          (update-call-stack-tree-pane flow-id thread-id))}]
          ctx-menu (ui-utils/make-context-menu ctx-menu-options)]
      (doto fn-call-box
        (.setOnMouseClicked (event-handler
                             [^MouseEvent mev]
                             (when (= MouseButton/SECONDARY (.getButton mev))
                               (.show ctx-menu
                                      fn-call-box
                                      (.getScreenX mev)
                                      (.getScreenY mev)))))))))

(defn- select-call-stack-tree-node [flow-id thread-id match-trace-idx]
  (let [[tree-view] (obj-lookup flow-id (ui-vars/thread-callstack-tree-view-id thread-id))
        [^TreeItem tree-item] (obj-lookup flow-id (ui-vars/thread-callstack-tree-item thread-id match-trace-idx))
        tree-selection-model (.getSelectionModel tree-view)]
    (.select ^SelectionModel tree-selection-model tree-item)))

(defn- create-tree-search-pane [flow-id thread-id]
  (let [indexer (state/thread-trace-indexer dbg-state flow-id thread-id)
        search-txt (doto (TextField.)
                     (.setPromptText "Search"))
        search-from-txt (doto (TextField. "0")
                          (.setPrefWidth 70)
                          (.setAlignment Pos/CENTER))
        search-lvl-txt (doto (TextField. "2")
                         (.setPrefWidth 30)
                         (.setAlignment Pos/CENTER))
        search-match-lbl (label "")
        search-btn (ui-utils/icon-button "mdi-magnify" "tree-search")
        _ (doto search-btn
              (.setOnAction (event-handler
                             [_]
                             (log "Searching")
                             (.setDisable search-btn true)
                             (state/callstack-tree-collapse-all-calls dbg-state flow-id thread-id)
                             (indexer/search-next-fn-call-trace
                              indexer
                              (.getText search-txt)
                              (Integer/parseInt (.getText search-from-txt))
                              (Integer/parseInt (.getText search-lvl-txt))
                              (fn [next-match-path]
                                (if next-match-path
                                  (let [[match-trace-idx] next-match-path]
                                    (log (format "Next match at %s" next-match-path))
                                    (state/callstack-tree-select-path dbg-state
                                                                      flow-id
                                                                      thread-id
                                                                      next-match-path)
                                    (ui-utils/run-later
                                     (update-call-stack-tree-pane flow-id thread-id)
                                     (select-call-stack-tree-node flow-id thread-id match-trace-idx)
                                     (.setText search-match-lbl (format "Match idx %d" match-trace-idx))
                                     (.setText search-from-txt (str match-trace-idx))))
                                  (do
                                    (ui-utils/run-later (.setText search-match-lbl ""))
                                    (log "No match found")))
                                (ui-utils/run-later (.setDisable search-btn false)))
                              (fn [progress-perc]
                                (ui-utils/run-later
                                 (.setText search-match-lbl (format "%.2f %%" (double progress-perc))))))
                             )))]
    (doto (h-box [search-match-lbl
                  search-txt
                  (label "From Idx: ")   search-from-txt
                  (label "*print-level* : ") search-lvl-txt
                  search-btn])
      (.setSpacing 3.0)
      (.setAlignment Pos/CENTER_RIGHT)
      (.setPadding (Insets. 4.0)))))

(defn create-call-stack-tree-pane [flow-id thread-id]
  (let [indexer (state/thread-trace-indexer dbg-state flow-id thread-id)

        cell-factory (proxy [javafx.util.Callback] []
                       (call [tv]
                         (proxy [TreeCell] []
                           (updateItem [tree-node empty?]
                             (proxy-super updateItem tree-node empty?)
                             (if empty?

                               (.setGraphic this nil)

                               (let [frame (indexer/callstack-node-frame indexer tree-node)
                                     frame-trace-idx (:call-trace-idx frame)
                                     expanded? (or (nil? frame-trace-idx)
                                                   (state/callstack-tree-item-expanded? dbg-state flow-id thread-id frame-trace-idx))
                                     tree-item (.getTreeItem this)]

                                 (doto this
                                   (.setGraphic (create-call-stack-tree-node
                                                 frame
                                                 flow-id
                                                 thread-id))
                                   (.setTooltip (Tooltip. (format "Trace Idx : %d" frame-trace-idx))))

                                 (store-obj flow-id (ui-vars/thread-callstack-tree-item thread-id frame-trace-idx) tree-item)

                                 (doto tree-item
                                   (.addEventHandler (TreeItem/branchCollapsedEvent)
                                                     (event-handler
                                                      [ev]
                                                      (when (= (.getTreeItem ev) tree-item)
                                                        (state/callstack-tree-collapse-calls dbg-state flow-id thread-id #{frame-trace-idx}))))
                                   (.addEventHandler (TreeItem/branchExpandedEvent)
                                                     (event-handler
                                                      [ev]
                                                      (when (= (.getTreeItem ev) tree-item)
                                                        (state/callstack-tree-expand-calls dbg-state flow-id thread-id #{frame-trace-idx}))))
                                   (.setExpanded expanded?))))))))
        search-pane (create-tree-search-pane flow-id thread-id)
        tree-view (doto (TreeView.)
                    (.setEditable false)
                    (.setCellFactory cell-factory))
        tree-view-sel-model (.getSelectionModel tree-view)
        callstack-fn-args-pane   (flow-cmp/create-pprint-pane flow-id thread-id "fn_args")
        callstack-fn-ret-pane (flow-cmp/create-pprint-pane flow-id thread-id "fn_ret")
        labeled-args-pane  (v-box [(label "Args:") callstack-fn-args-pane])
        labeled-ret-pane (v-box [(label "Ret:") callstack-fn-ret-pane])
        args-ret-pane (doto (h-box [labeled-args-pane labeled-ret-pane])
                        (.setSpacing 5.0))]
    (HBox/setHgrow labeled-args-pane Priority/ALWAYS)
    (HBox/setHgrow labeled-ret-pane Priority/ALWAYS)
    (.addListener (.selectedItemProperty tree-view-sel-model)
                  (proxy [ChangeListener] []
                    (changed [changed old-val new-val]
                      (when new-val
                        (let [{:keys [args ret]} (indexer/callstack-node-frame indexer (.getValue new-val))]
                          (flow-cmp/update-pprint-pane flow-id thread-id "fn_args" args)
                          (flow-cmp/update-pprint-pane flow-id thread-id "fn_ret" ret))))))

    (store-obj flow-id (ui-vars/thread-callstack-tree-view-id thread-id) tree-view)
    (VBox/setVgrow tree-view Priority/ALWAYS)
    (v-box [search-pane
            tree-view
            args-ret-pane])))
