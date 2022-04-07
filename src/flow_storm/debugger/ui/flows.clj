(ns flow-storm.debugger.ui.flows
  (:require [flow-storm.debugger.ui.state-vars :refer [store-obj obj-lookup] :as ui-vars]
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

(declare jump-to-coord)

(defn def-kind-colored-label [text kind]
  (case kind
    :defmethod       (label text "defmethod")
    :extend-protocol (label text "extend-protocol")
    :extend-type     (label text "extend-type")
    :defn            (label text "defn")
    (label text "anonymous")))

(defn- format-value-short [v]
  (let [max-len 80
        s (binding [clojure.core/*print-level* 3
                    clojure.core/*print-length* 3]
            (pr-str v))
        len (count s)]
    (cond-> (subs s 0 (min max-len len))
      (> len max-len) (str " ... "))))

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

(defn- create-forms-pane [flow-id thread-id]
  (let [box (doto (v-box [])
              (.setSpacing 5))
        scroll-pane (ScrollPane.)]
    (.setContent scroll-pane box)
    (store-obj flow-id (ui-vars/thread-forms-box-id thread-id) box)
    (store-obj flow-id (ui-vars/thread-forms-scroll-id thread-id) scroll-pane)
    scroll-pane))

(defn create-pprint-pane [flow-id thread-id pane-id]
  (let [result-text-area (doto (TextArea.)
                           (.setEditable false))]
    (store-obj flow-id (ui-vars/thread-pprint-text-area-id thread-id pane-id) result-text-area)
    result-text-area))

(defn create-result-tree-pane [flow-id thread-id]
  (label "TREE")
  )

(defn update-pprint-pane [flow-id thread-id pane-id val]
  ;; TODO: find and update the tree
  (let [[^TextArea text-area] (obj-lookup flow-id (ui-vars/thread-pprint-text-area-id thread-id pane-id))
        val-str (with-out-str
                  (binding [clojure.core/*print-level* 7
                            clojure.core/*print-length* 50]
                    (pp/pprint val)))]
    (.setText text-area val-str)))

(defn- create-result-pane [flow-id thread-id]
  (let [tools-tab-pane (doto (TabPane.)
                         (.setTabClosingPolicy TabPane$TabClosingPolicy/UNAVAILABLE))
        pprint-tab (doto (Tab. "Pprint")
                     (.setContent (create-pprint-pane flow-id thread-id "expr_result")))
        tree-tab (doto (Tab. "Tree")
                   (.setDisable true)
                   (.setContent (create-result-tree-pane flow-id thread-id)))]
    (-> tools-tab-pane
        .getTabs
        (.addAll [pprint-tab tree-tab]))

    tools-tab-pane))

(defn- create-fns-list-pane [flow-id thread-id]
  (let [observable-fns-list (FXCollections/observableArrayList)
        cell-factory (proxy [javafx.util.Callback] []
                       (call [lv]
                         (ui-utils/create-list-cell-factory
                          (fn [list-cell {:keys [form-def-kind fn-name fn-ns form-id dispatch-val cnt]}]
                            (let [fn-lbl (doto (case form-def-kind
                                                 :defmethod       (def-kind-colored-label (format "%s/%s %s" fn-ns fn-name dispatch-val) form-def-kind)
                                                 :extend-protocol (def-kind-colored-label (format "%s/%s" fn-ns fn-name) form-def-kind)
                                                 :extend-type     (def-kind-colored-label (format "%s/%s" fn-ns fn-name) form-def-kind)
                                                 :defn            (def-kind-colored-label (format "%s/%s" fn-ns fn-name) form-def-kind)
                                                 (def-kind-colored-label (format "F %s/%s" fn-ns fn-name) form-def-kind))
                                           (.setPrefWidth 500))
                                  cnt-lbl (doto (label (str cnt))
                                            (.setPrefWidth 100))
                                  hbox (h-box [fn-lbl cnt-lbl])]
                              (.setGraphic ^Node list-cell hbox))))))
        fns-list-view (doto (ListView. observable-fns-list)
                               (.setEditable false)
                               (.setCellFactory cell-factory))
        fns-list-selection (.getSelectionModel fns-list-view)
        ctx-menu-un-instrument-item {:text "Un-instrument seleced functions"
                                     :on-click (fn []
                                                 (let [groups (->> (.getSelectedItems fns-list-selection)
                                                                   (group-by (fn [{:keys [form-def-kind]}]
                                                                               (cond
                                                                                 (#{:defn} form-def-kind) :vars
                                                                                 (#{:defmethod :extend-protocol :extend-type} form-def-kind) :forms
                                                                                 :else nil))))]

                                                   (let [vars-symbs (->> (:vars groups)
                                                                         (map (fn [{:keys [fn-name fn-ns]}]
                                                                                (symbol fn-ns fn-name))))]
                                                     (target-commands/run-command :uninstrument-fn-bulk vars-symbs))

                                                   (let [forms (->> (:forms groups)
                                                                    (map (fn [{:keys [fn-ns form]}]
                                                                           {:form-ns fn-ns
                                                                            :form form})))]
                                                     (target-commands/run-command :eval-form-bulk forms))))}
        ctx-menu-show-similar-fn-call-item {:text "Show function calls"
                                            :on-click (fn []
                                                        (let [indexer (state/thread-trace-indexer dbg-state flow-id thread-id)
                                                              {:keys [form-id fn-ns fn-name]} (first (.getSelectedItems fns-list-selection))
                                                              [observable-fn-calls-list] (obj-lookup flow-id (ui-vars/thread-instrument-fn-calls-list-id thread-id))
                                                              fn-call-traces (indexer/find-fn-calls indexer fn-ns fn-name form-id)]
                                                          (doto observable-fn-calls-list
                                                            .clear
                                                            (.addAll (into-array Object fn-call-traces)))))}]

    (.setOnMouseClicked fns-list-view
                        (event-handler
                         [mev]
                         (when (= MouseButton/SECONDARY (.getButton mev))
                           (let [sel-cnt (count (.getSelectedItems fns-list-selection))
                                 ctx-menu (if (= 1 sel-cnt)
                                            (ui-utils/make-context-menu [ctx-menu-un-instrument-item ctx-menu-show-similar-fn-call-item])
                                            (ui-utils/make-context-menu [ctx-menu-un-instrument-item]))]
                             (.show ctx-menu
                                    fns-list-view
                                    (.getScreenX mev)
                                    (.getScreenY mev))))))

    (.setSelectionMode fns-list-selection SelectionMode/MULTIPLE)
    (store-obj flow-id (ui-vars/thread-instrument-list-id thread-id) observable-fns-list)
    fns-list-view))

(defn- create-fn-calls-list-pane [flow-id thread-id]
  (let [observable-fn-calls-list (FXCollections/observableArrayList)
        list-cell-factory (proxy [javafx.util.Callback] []
                            (call [lv]
                              (ui-utils/create-list-cell-factory
                               (fn [list-cell {:keys [args-vec]}]
                                 (let [[args-print-type-combo] (obj-lookup flow-id (ui-vars/thread-fn-args-print-combo thread-id))
                                       [print-args-type _] (.getSelectedItem (.getSelectionModel args-print-type-combo))
                                       arg-selector (fn [n]
                                                      (when (< n (count args-vec))
                                                        (str "... " (format-value-short (nth args-vec n)) " ...")))
                                       args-lbl (label (case print-args-type
                                                         :all-args (format-value-short args-vec)
                                                         :a0       (arg-selector 0)
                                                         :a1       (arg-selector 1)
                                                         :a2       (arg-selector 2)
                                                         :a3       (arg-selector 3)
                                                         :a4       (arg-selector 4)
                                                         :a5       (arg-selector 5)
                                                         :a6       (arg-selector 6)
                                                         :a7       (arg-selector 7)
                                                         :a8       (arg-selector 8)
                                                         :a9       (arg-selector 9)))]
                                   (.setGraphic ^Node list-cell args-lbl))))))
        combo-cell-factory (proxy [javafx.util.Callback] []
                             (call [lv]
                               (ui-utils/create-list-cell-factory
                                (fn [cell [_ text]]
                                  (.setText cell text)))))
        args-print-type-combo (doto (ComboBox.)
                                (.setItems (doto (FXCollections/observableArrayList)
                                             (.addAll (into-array Object [[:all-args "Print all args"]
                                                                          [:a0       "Print only arg 0"]
                                                                          [:a1       "Print only arg 1"]
                                                                          [:a2       "Print only arg 2"]
                                                                          [:a3       "Print only arg 3"]
                                                                          [:a4       "Print only arg 4"]
                                                                          [:a5       "Print only arg 5"]
                                                                          [:a6       "Print only arg 6"]
                                                                          [:a7       "Print only arg 7"]
                                                                          [:a8       "Print only arg 8"]
                                                                          [:a9       "Print only arg 9"]]))))
                                (.setConverter (proxy [StringConverter] []
                                                      (toString [[_ text]] text)))
                                (.setCellFactory combo-cell-factory))
        _ (.selectFirst (.getSelectionModel args-print-type-combo))
        _ (store-obj flow-id (ui-vars/thread-fn-args-print-combo thread-id) args-print-type-combo)
        fn-call-list-view (doto (ListView. observable-fn-calls-list)
                            (.setEditable false)
                            (.setCellFactory list-cell-factory))
        fn-call-list-selection (.getSelectionModel fn-call-list-view)
        fn-call-list-pane (v-box [args-print-type-combo fn-call-list-view])]

    (VBox/setVgrow fn-call-list-view Priority/ALWAYS)

    (.setOnMouseClicked fn-call-list-view
                        (event-handler
                         [mev]
                         (when (= MouseButton/SECONDARY (.getButton mev))
                           (let [trace-idx (-> (.getSelectedItems fn-call-list-selection)
                                               first
                                               meta
                                               :trace-idx)
                                 ctx-menu (ui-utils/make-context-menu [{:text (format "Goto trace %d" trace-idx)
                                                                        :on-click (fn []
                                                                                    (jump-to-coord flow-id thread-id trace-idx))}])]
                             (.show ctx-menu
                                    fn-call-list-view
                                    (.getScreenX mev)
                                    (.getScreenY mev))))))

    (.setSelectionMode fn-call-list-selection SelectionMode/SINGLE)
    (store-obj flow-id (ui-vars/thread-instrument-fn-calls-list-id thread-id) observable-fn-calls-list)
    fn-call-list-pane))

(defn- create-instrument-pane [flow-id thread-id]
  (let [fns-list-pane (create-fns-list-pane flow-id thread-id)
        fn-calls-list-pane (create-fn-calls-list-pane flow-id thread-id)]
    (HBox/setHgrow fns-list-pane Priority/ALWAYS)
    (HBox/setHgrow fn-calls-list-pane Priority/ALWAYS)
    (h-box [fns-list-pane fn-calls-list-pane])))

(defn- update-instrument-pane [flow-id thread-id]
  (let [indexer (state/thread-trace-indexer dbg-state flow-id thread-id)
        fn-call-stats (->> (state/fn-call-stats dbg-state flow-id thread-id)
                           (sort-by :cnt >))
        [^ObservableList observable-bindings-list] (obj-lookup flow-id (ui-vars/thread-instrument-list-id thread-id))]
    (.clear observable-bindings-list)
    (.addAll observable-bindings-list (into-array Object fn-call-stats))))

(defn- create-locals-pane [flow-id thread-id]
  (let [observable-bindings-list (FXCollections/observableArrayList)
        cell-factory (proxy [javafx.util.Callback] []
                       (call [lv]
                         (ui-utils/create-list-cell-factory
                          (fn [list-cell symb-val]
                            (let [symb-lbl (doto (label (first symb-val))
                                             (.setPrefWidth 100))
                                  val-lbl (label  (format-value-short (second symb-val)))
                                  hbox (h-box [symb-lbl val-lbl])]
                              (.setGraphic ^Node list-cell hbox))))))
        locals-list-view (doto (ListView. observable-bindings-list)
                           (.setEditable false)
                           (.setCellFactory cell-factory))]
    (store-obj flow-id (ui-vars/thread-locals-list-id thread-id) observable-bindings-list)
    locals-list-view))

(defn- update-locals-pane [flow-id thread-id bindings]
  (let [[^ObservableList observable-bindings-list] (obj-lookup flow-id (ui-vars/thread-locals-list-id thread-id))]
    (.clear observable-bindings-list)
    (.addAll observable-bindings-list (into-array Object bindings))))

(defn- create-code-pane [flow-id thread-id]
  (let [left-right-pane (doto (SplitPane.)
                          (.setOrientation (Orientation/HORIZONTAL)))
        locals-result-pane (doto (SplitPane.)
                             (.setOrientation (Orientation/VERTICAL)))
        forms-pane (create-forms-pane flow-id thread-id)
        result-pane (create-result-pane flow-id thread-id)
        locals-pane (create-locals-pane flow-id thread-id)]

    (-> locals-result-pane
        .getItems
        (.addAll [result-pane locals-pane]))
    (-> left-right-pane
        .getItems
        (.addAll [forms-pane locals-result-pane]))
    left-right-pane))

(defn- update-call-stack-tree-pane [flow-id thread-id]
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
  (let [step-1 (format-value-short args-vec)]
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
          fn-name-lbl (def-kind-colored-label fn-name def-kind)
          args-lbl (label (str " " (format-tree-fn-call-args args)) "fn-args")
          fn-call-box (if dispatch-val
                        (h-box [(label "(") ns-lbl fn-name-lbl (label (str dispatch-val)) args-lbl (label ")")])
                        (h-box [(label "(") ns-lbl fn-name-lbl args-lbl (label ")")]))
          ctx-menu-options [{:text (format "Goto trace %d" call-trace-idx)
                             :on-click #(jump-to-coord flow-id thread-id call-trace-idx)}
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
                             (tap> "Searching")
                             (.setDisable search-btn true)
                             (state/callstack-tree-collapse-all-calls dbg-state flow-id thread-id)
                             (let [next-match-path (indexer/search-next-fn-call-trace
                                                    indexer
                                                    (.getText search-txt)
                                                    (Integer/parseInt (.getText search-from-txt))
                                                    (Integer/parseInt (.getText search-lvl-txt))
                                                    (fn [next-match-path]
                                                      (if next-match-path
                                                        (let [[match-trace-idx] next-match-path]
                                                          (tap> (format "Next match at %s" next-match-path))
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
                                                          (tap> "No match found")))
                                                      (ui-utils/run-later (.setDisable search-btn false)))
                                                    (fn [progress-perc]
                                                      (ui-utils/run-later
                                                       (.setText search-match-lbl (format "%.2f %%" (double progress-perc))))))]
                               ))))]
    (doto (h-box [search-match-lbl
                  search-txt
                  (label "From Idx: ")   search-from-txt
                  (label "*print-level* : ") search-lvl-txt
                  search-btn])
      (.setSpacing 3.0)
      (.setAlignment Pos/CENTER_RIGHT)
      (.setPadding (Insets. 4.0)))))

(defn- create-call-stack-tree-pane [flow-id thread-id]
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
        callstack-fn-args-pane   (create-pprint-pane flow-id thread-id "fn_args")
        callstack-fn-ret-pane (create-pprint-pane flow-id thread-id "fn_ret")
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
                          (update-pprint-pane flow-id thread-id "fn_args" args)
                          (update-pprint-pane flow-id thread-id "fn_ret" ret))))))

    (store-obj flow-id (ui-vars/thread-callstack-tree-view-id thread-id) tree-view)
    (VBox/setVgrow tree-view Priority/ALWAYS)
    (v-box [search-pane
            tree-view
            args-ret-pane])))


(defn- highlight-executing [token-text]
  (ui-utils/rm-class token-text "interesting")
  (ui-utils/add-class token-text "executing"))

(defn- highlight-interesting [token-text]
  (ui-utils/rm-class token-text "executing")
  (ui-utils/add-class token-text "interesting"))

(defn- arm-interesting [^Text token-text traces]
  (let [{:keys [flow-id thread-id]} (first traces)]

    (if (> (count traces) 1)
      (let [ctx-menu-options (->> traces
                              (map (fn [t]
                                     (let [tidx (-> t meta :trace-idx)]
                                       {:text (format "%d | %s" tidx (format-value-short (:result t)))
                                        :on-click #(jump-to-coord flow-id thread-id tidx)}))))
            ctx-menu (ui-utils/make-context-menu ctx-menu-options)]
        (.setOnMouseClicked token-text (event-handler
                                        [^MouseEvent ev]
                                        (.show ctx-menu
                                               token-text
                                               (.getScreenX ev)
                                               (.getScreenY ev)))))

      (.setOnMouseClicked token-text (event-handler
                                      [ev]
                                      (jump-to-coord flow-id thread-id (-> traces first meta :trace-idx)))))))

(defn- add-form [flow-id thread-id form-id]
  (let [indexer (state/thread-trace-indexer dbg-state flow-id thread-id)
        form (indexer/get-form indexer form-id)
        print-tokens (binding [pp/*print-right-margin* 80]
                       (form-pprinter/pprint-tokens (:form/form form)))
        [forms-box] (obj-lookup flow-id (ui-vars/thread-forms-box-id thread-id))
        tokens-texts (->> print-tokens
                          (map (fn [tok]
                                 (let [text (Text.
                                             (case tok
                                               :nl   "\n"
                                               :sp   " "
                                               (first tok)))
                                       _ (ui-utils/add-class text "code-token")
                                       coord (when (vector? tok) (second tok))]
                                   (store-obj flow-id (ui-vars/form-token-id thread-id form-id coord) text)
                                   text))))
        ns-label (doto (label (format "ns: %s" (:form/ns form)))
                   (.setFont (Font. 10)))

        form-header (doto (h-box [ns-label])
                      (.setAlignment (Pos/TOP_RIGHT)))
        form-text-flow (TextFlow. (into-array Text tokens-texts))

        form-pane (v-box [form-header form-text-flow] "form-pane")
        ]
    (store-obj flow-id (ui-vars/thread-form-box-id thread-id form-id) form-pane)

    (-> forms-box
        .getChildren
        (.add 0 form-pane))

    form-pane))

(defn- update-thread-trace-count-lbl [flow-id thread-id cnt]
  (let [[^Label lbl] (obj-lookup flow-id (ui-vars/thread-trace-count-lbl-id thread-id))]
    (.setText lbl (str cnt))))

(defn- un-highlight [^Text token-text]
  (ui-utils/rm-class token-text "interesting")
  (ui-utils/rm-class token-text "executing")
  (.setOnMouseClicked token-text (event-handler [_])))

(defn highlight-form [flow-id thread-id form-id]
  (let [indexer (state/thread-trace-indexer dbg-state flow-id thread-id)
        form (indexer/get-form indexer form-id)
        [form-pane]          (obj-lookup flow-id (ui-vars/thread-form-box-id thread-id form-id))
        [thread-scroll-pane] (obj-lookup flow-id (ui-vars/thread-forms-scroll-id thread-id))

        ;; if the form we are about to highlight doesn't exist in the view add it first
        form-pane (or form-pane (add-form flow-id thread-id form-id))
        ctx-menu-options [{:text "Fully instrument this form"
                           :on-click (fn []

                                       (if (= :defn (:form/def-kind form))

                                         (let [curr-trace-idx (state/current-trace-idx dbg-state flow-id thread-id)
                                               curr-fn-call-trace-idx (indexer/callstack-frame-call-trace-idx indexer curr-trace-idx)
                                               {:keys [fn-name]} (indexer/get-trace indexer curr-fn-call-trace-idx)]
                                           (target-commands/run-command :instrument-fn (symbol (:form/ns form) fn-name) {}))

                                         (target-commands/run-command :instrument-form-bulk [{:form-ns (:form/ns form)
                                                                                              :form (:form/form form)}]
                                                                      {})))}]
        ctx-menu (ui-utils/make-context-menu ctx-menu-options)]

    (.setOnMouseClicked form-pane
                        (event-handler
                         [mev]
                         (when (= MouseButton/SECONDARY (.getButton mev))
                           (.show ctx-menu
                                  form-pane
                                  (.getScreenX mev)
                                  (.getScreenY mev)))))


    (ui-utils/center-node-in-scroll-pane thread-scroll-pane form-pane)
    (ui-utils/add-class form-pane "form-background-highlighted")))

(defn- unhighlight-form [flow-id thread-id form-id]
  (let [[form-pane] (obj-lookup flow-id (ui-vars/thread-form-box-id thread-id form-id))]
    (doto form-pane
      (.setOnMouseClicked (event-handler [_])))
    (ui-utils/rm-class form-pane "form-background-highlighted")))

(defn- jump-to-coord [flow-id thread-id next-trace-idx]
  (let [indexer (state/thread-trace-indexer dbg-state flow-id thread-id)
        trace-count (indexer/thread-exec-count indexer)]
    (when (<= 0 next-trace-idx (dec trace-count))
      (let [curr-idx (state/current-trace-idx dbg-state flow-id thread-id)
            curr-trace (indexer/get-trace indexer curr-idx)
            curr-form-id (:form-id curr-trace)
            next-trace (indexer/get-trace indexer next-trace-idx)
            next-form-id (:form-id next-trace)
            [^Label curr_trace_lbl] (obj-lookup flow-id (ui-vars/thread-curr-trace-lbl-id thread-id))
            ;; because how frames are cached by trace, their pointers can't be compared
            ;; so a content comparision is needed. Comparing :call-trace-idx is enough since it is
            ;; a frame
            changing-frame? (not= (indexer/callstack-frame-call-trace-idx indexer curr-idx)
                                  (indexer/callstack-frame-call-trace-idx indexer next-trace-idx))
            changing-form? (not= curr-form-id next-form-id)]

        ;; update thread current trace label and total traces
        (.setText curr_trace_lbl (str next-trace-idx))
        (update-thread-trace-count-lbl flow-id thread-id trace-count)

        (when changing-form?
          ;; we are leaving a form with this jump, so unhighlight all curr-form interesting tokens
          (let [curr-form-interesting-expr-traces (indexer/interesting-expr-traces indexer curr-form-id curr-idx)]

            (unhighlight-form flow-id thread-id curr-form-id)
            (highlight-form flow-id thread-id next-form-id)

            (doseq [{:keys [coor]} curr-form-interesting-expr-traces]
              (let [token-texts (obj-lookup flow-id (ui-vars/form-token-id thread-id curr-form-id coor))]
                (doseq [text token-texts]
                  (un-highlight text))))))

        (when (or changing-frame?
                  (zero? curr-idx))
          ;; we are leaving a frame with this jump, or its the first trace
          ;; highlight all interesting tokens for the form we are currently in
          (let [interesting-expr-traces-grps (->> (indexer/interesting-expr-traces indexer next-form-id next-trace-idx)
                                                  (group-by :coor))]

            (doseq [[coor traces] interesting-expr-traces-grps]
              (let [token-id (ui-vars/form-token-id thread-id next-form-id coor)
                    token-texts (obj-lookup flow-id token-id)]
                (doseq [text token-texts]
                  (arm-interesting text traces)
                  (highlight-interesting text))))))

        ;; "unhighlight" prev executing tokens
        (when (and (utils/exec-trace? curr-trace))
          (let [curr-token-texts (obj-lookup flow-id (ui-vars/form-token-id thread-id
                                                                               (:form-id curr-trace)
                                                                               (:coor curr-trace)))]
            (doseq [text curr-token-texts]
              (if (= curr-form-id next-form-id)
                (highlight-interesting text)
                (un-highlight text)))))

        ;; highlight executing tokens
        (when (utils/exec-trace? next-trace)
          (let [next-token-texts (obj-lookup flow-id (ui-vars/form-token-id thread-id
                                                                               (:form-id next-trace)
                                                                               (:coor next-trace)))]
            (doseq [text next-token-texts]
              (highlight-executing text))))

        ;; update reusult panel
        (update-pprint-pane flow-id thread-id "expr_result" (:result next-trace))

        ;; update locals panel
        (update-locals-pane flow-id thread-id (indexer/bindings-for-trace indexer next-trace-idx))

        (state/set-trace-idx dbg-state flow-id thread-id next-trace-idx)))))

(defn- create-thread-controls-pane [flow-id thread-id]
  (let [prev-btn (doto (ui-utils/icon-button "mdi-chevron-left")
                   (.setOnAction (event-handler
                                  [ev]
                                  (jump-to-coord flow-id
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
                                  (jump-to-coord flow-id
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
                   (.setContent (create-code-pane flow-id thread-id)))
        callstack-tree-tab (doto (Tab. "Call tree")
                             (.setContent (create-call-stack-tree-pane flow-id thread-id))
                             (.setOnSelectionChanged (event-handler [_] (update-call-stack-tree-pane flow-id thread-id))))
        instrument-tab (doto (Tab. "Functions")
                             (.setContent (create-instrument-pane flow-id thread-id))
                             (.setOnSelectionChanged (event-handler [_] (update-instrument-pane flow-id thread-id))))]

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
