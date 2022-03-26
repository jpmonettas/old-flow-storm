(ns flow-storm.debugger.ui.flows
  (:require [flow-storm.debugger.ui.state-vars :refer [store-obj obj-lookup] :as state-vars]
            [flow-storm.debugger.ui.utils :as ui-utils :refer [event-handler run-later run-now]]
            [flow-storm.debugger.trace-indexer.protos :as indexer]
            [clojure.pprint :as pp]
            [flow-storm.debugger.state :as state :refer [dbg-state]]
            [flow-storm.debugger.form-pprinter :as form-pprinter]
            [clojure.string :as str]
            [flow-storm.utils :as utils])
  (:import [javafx.scene.layout BorderPane Background BackgroundFill CornerRadii GridPane HBox Priority Pane VBox]
           [javafx.scene.control Button Label ListView ListCell ScrollPane TreeCell TextArea TextField Tab TabPane TabPane$TabClosingPolicy TreeView TreeItem  SplitPane]
           [javafx.scene.text TextFlow Text Font]
           [javafx.scene Node]
           [javafx.scene.paint Color]
           [javafx.geometry Insets Side Orientation Pos]
           [javafx.collections FXCollections ObservableList]
           [javafx.scene.input MouseEvent]))

(declare jump-to-coord)

(def form-background-normal (Background. (into-array BackgroundFill [(BackgroundFill. (Color/web "#eee")
                                                                                      (CornerRadii. 0)
                                                                                      (Insets. 0))])))

(def form-background-highlighted (Background. (into-array BackgroundFill [(BackgroundFill. (Color/web "#ffffab")
                                                                                      (CornerRadii. 0)
                                                                                      (Insets. 0))])))

(defn- format-value-short [v]
  (let [max-len 80
        s (binding [clojure.core/*print-level* 3
                    clojure.core/*print-length* 3]
            (pr-str v))
        len (count s)]
    (cond-> (subs s 0 (min max-len len))
      (> len max-len) (str " ... "))))

(defn create-empty-flow [flow-id]
  (run-now
   (let [[^TabPane flows-tabs-pane] (obj-lookup "flows_tabs_pane")
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
              (.setSpacing 5))
        scroll-pane (ScrollPane.)]
    (.setContent scroll-pane box)
    (store-obj flow-id (state-vars/thread-forms-box-id thread-id) box)
    (store-obj flow-id (state-vars/thread-forms-scroll-id thread-id) scroll-pane)
    scroll-pane))

(defn create-result-pprint-pane [flow-id thread-id]
  (let [result-text-area (doto (TextArea.)
                           (.setEditable false))]
    (store-obj flow-id (state-vars/thread-result-text-area-id thread-id) result-text-area)
    result-text-area))

(defn create-result-tree-pane [flow-id thread-id]
  (Label. "TREE")
  )

(defn update-result-pane [flow-id thread-id val]
  ;; TODO: find and update the tree
  (let [[^TextArea text-area] (obj-lookup flow-id (state-vars/thread-result-text-area-id thread-id))
        val-str (with-out-str (pp/pprint val))]
    (.setText text-area val-str)))

(defn- create-result-pane [flow-id thread-id]
  (let [tools-tab-pane (doto (TabPane.)
                         (.setTabClosingPolicy TabPane$TabClosingPolicy/UNAVAILABLE))
        pprint-tab (doto (Tab. "Pprint")
                     (.setContent (create-result-pprint-pane flow-id thread-id)))
        tree-tab (doto (Tab. "Tree")
                   (.setContent (create-result-tree-pane flow-id thread-id)))]
    (-> tools-tab-pane
        .getTabs
        (.addAll [pprint-tab tree-tab]))

    tools-tab-pane))

(defn- create-locals-pane [flow-id thread-id]
  (let [observable-bindings-list (FXCollections/observableArrayList)
        cell-factory (proxy [javafx.util.Callback] []
                       (call [lv]
                         (proxy [ListCell] []
                           (updateItem [symb-val empty?]
                             (proxy-super updateItem symb-val empty?)
                             (if empty?
                               (.setGraphic ^Node this nil)
                               (let [symb-lbl (doto (Label. (first symb-val))
                                                (.setPrefWidth 100))
                                     val-lbl (Label.  (format-value-short (second symb-val)))
                                     hbox (HBox. (into-array Node [symb-lbl val-lbl]))]
                                 (.setGraphic ^Node this hbox)))))))
        locals-list-view (doto (ListView. observable-bindings-list)
                           (.setEditable false)
                           (.setCellFactory cell-factory))]
    (store-obj flow-id (state-vars/thread-locals-list-id thread-id) observable-bindings-list)
    locals-list-view))

(defn- update-locals-pane [flow-id thread-id bindings]
  (let [[^ObservableList observable-bindings-list] (obj-lookup flow-id (state-vars/thread-locals-list-id thread-id))]
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
        [tree-view] (obj-lookup flow-id (state-vars/thread-callstack-tree-view-id thread-id))]

    (.setRoot ^TreeView tree-view root-item)))

(defn- create-call-stack-tree-node [{:keys [call-trace-idx fn-name fn-ns args]} flow-id thread-id]
  ;; Important !
  ;; this will be called for all visible tree nodes after any expansion
  ;; so it should be fast
  (let [ns-lbl (Label. (str fn-ns "/"))
        fn-name-lbl (doto (Label. fn-name)
                      (.setStyle "-fx-font-weight: bold;"))
        args-lbl (Label. (str " " (format-value-short args)))
        node-box (HBox. (into-array Node [(Label. "(") ns-lbl fn-name-lbl args-lbl (Label. ")")]))
        ctx-menu-options [{:text (format "Goto trace %d" call-trace-idx)
                           :on-click #(jump-to-coord flow-id thread-id call-trace-idx)}
                          {:text (format "Hide %s/%s from this tree" fn-ns fn-name)
                           :on-click #(do
                                        (state/callstack-tree-hide-fn dbg-state flow-id thread-id fn-name fn-ns)
                                        (update-call-stack-tree-pane flow-id thread-id))}]
        ctx-menu (ui-utils/make-context-menu ctx-menu-options)]
    (doto node-box
      (.setOnMouseClicked (event-handler
                           [^MouseEvent ev]
                           (.show ctx-menu
                                  node-box
                                  (.getScreenX ev)
                                  (.getScreenY ev)))))))

(defn- create-tree-search-pane [flow-id thread-id]
  (let [indexer (state/thread-trace-indexer dbg-state flow-id thread-id)
        search-txt (TextField.)
        search-from-txt (TextField. "0")
        search-btn (doto (Button. "Search")
                     (.setOnAction (event-handler
                                    [_]
                                    (tap> "Searching")
                                    (state/callstack-tree-collapse-all-calls dbg-state flow-id thread-id)
                                    (let [next-match-path (indexer/search-next-fn-call-trace
                                                           indexer
                                                           (.getText search-txt)
                                                           (Integer/parseInt (.getText search-from-txt)))]
                                      (if next-match-path
                                        (do
                                          (tap> (format "Next match at %s" next-match-path))
                                          (state/callstack-tree-expand-calls dbg-state flow-id thread-id next-match-path)
                                          (update-call-stack-tree-pane flow-id thread-id))
                                        (tap> "No match found"))))))]
    (HBox. (into-array Node [search-txt search-from-txt search-btn]))))

(defn- create-call-stack-tree-pane [flow-id thread-id]
  (let [indexer (state/thread-trace-indexer dbg-state flow-id thread-id)
        update-btn (doto (Button. "Update")
                     (.setOnAction (event-handler
                                    [_]
                                    (update-call-stack-tree-pane flow-id thread-id))))
        cell-factory (proxy [javafx.util.Callback] []
                       (call [tv]
                         (proxy [TreeCell] []
                           (updateItem [tree-node empty?]
                             (proxy-super updateItem tree-node empty?)
                             (if empty?

                               (.setGraphic this nil)

                               (let [frame (indexer/callstack-node-frame indexer tree-node)
                                     expanded? (state/callstack-tree-item-expanded? dbg-state flow-id thread-id (:call-trace-idx frame))
                                     tree-item (.getTreeItem this)]

                                 (.setGraphic this (create-call-stack-tree-node
                                                    frame
                                                    flow-id
                                                    thread-id))
                                 (doto tree-item
                                   (.addEventHandler (TreeItem/branchCollapsedEvent)
                                                     (event-handler
                                                      [ev]
                                                      (when (= (.getTreeItem ev) tree-item)
                                                        (state/callstack-tree-collapse-calls dbg-state flow-id thread-id #{(:call-trace-idx frame)}))))
                                   (.addEventHandler (TreeItem/branchExpandedEvent)
                                                     (event-handler
                                                      [ev]
                                                      (when (= (.getTreeItem ev) tree-item)
                                                        (state/callstack-tree-expand-calls dbg-state flow-id thread-id #{(:call-trace-idx frame)}))))
                                   (.setExpanded expanded?))))))))
        search-pane (create-tree-search-pane flow-id thread-id)
        tree-view (doto (TreeView.)
                    (.setEditable false)
                    (.setCellFactory cell-factory))]
    (store-obj flow-id (state-vars/thread-callstack-tree-view-id thread-id) tree-view)
    (VBox. (into-array Node [update-btn search-pane tree-view]))))


(defn- highlight-executing [^Text token-text]
  (doto token-text
    (.setFill (Color/RED))))

(defn- highlight-interesting [^Text token-text]
  (doto token-text
    (.setFill (Color/web "#32a852"))))

(defn- arm-interesting [^Text token-text traces]
  (let [{:keys [flow-id thread-id]} (first traces)]
    (.setStyle token-text "-fx-cursor: hand; -fx-font-weight: bold;")

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
        text-font (Font/font "monospaced" 13)
        [forms-box] (obj-lookup flow-id (state-vars/thread-forms-box-id thread-id))
        tokens-texts (->> print-tokens
                          (map (fn [tok]
                                 (let [text (Text.
                                             (case tok
                                               :nl   "\n"
                                               :sp   " "
                                               (first tok)))
                                       coord (when (vector? tok) (second tok))]
                                   (.setFont text text-font)
                                   (store-obj flow-id (state-vars/form-token-id thread-id form-id coord) text)
                                   text))))
        ns-label (doto (Label. (format "ns: %s" (:form/ns form)))
                   (.setFont (Font. 10)))

        form-header (doto (HBox. (into-array Node [ns-label]))
                      (.setAlignment (Pos/TOP_RIGHT)))
        form-text-flow (TextFlow. (into-array Text tokens-texts))
        form-pane (doto (VBox. (into-array Node [form-header form-text-flow]))
                    (.setBackground form-background-normal)
                    (.setStyle "-fx-padding: 10;"))]
    (store-obj flow-id (state-vars/thread-form-box-id thread-id form-id) form-pane)

    (-> forms-box
        .getChildren
        (.add 0 form-pane))

    form-pane))

(defn- update-thread-trace-count-lbl [flow-id thread-id cnt]
  (let [[^Label lbl] (obj-lookup flow-id (state-vars/thread-trace-count-lbl-id thread-id))]
    (.setText lbl (str cnt))))

(defn- un-highlight [^Text token-text]
  (doto token-text
    (.setFill (Color/BLACK))
    (.setStyle "-fx-cursor: pointer;")
    (.setOnMouseClicked (event-handler [_]))))

(defn highlight-form [flow-id thread-id form-id]
  (let [[form-pane]          (obj-lookup flow-id (state-vars/thread-form-box-id thread-id form-id))
        [thread-scroll-pane] (obj-lookup flow-id (state-vars/thread-forms-scroll-id thread-id))]

    ;; if the form we are about to highlight doesn't exist in the view add it first
    (let [form-pane (or form-pane (add-form flow-id thread-id form-id))]

      (ui-utils/center-node-in-scroll-pane thread-scroll-pane form-pane)
      (.setBackground form-pane form-background-highlighted))))

(defn- unhighlight-form [flow-id thread-id form-id]
  (let [[form-pane] (obj-lookup flow-id (state-vars/thread-form-box-id thread-id form-id))]
    (.setBackground form-pane form-background-normal)))

(defn- jump-to-coord [flow-id thread-id next-trace-idx]
  (let [indexer (state/thread-trace-indexer dbg-state flow-id thread-id)
        trace-count (indexer/thread-exec-count indexer)]
    (when (<= 0 next-trace-idx (dec trace-count))
      (let [curr-idx (state/current-trace-idx dbg-state flow-id thread-id)
            curr-trace (indexer/get-trace indexer curr-idx)
            curr-form-id (:form-id curr-trace)
            next-trace (indexer/get-trace indexer next-trace-idx)
            next-form-id (:form-id next-trace)
            [^Label curr_trace_lbl] (obj-lookup flow-id (state-vars/thread-curr-trace-lbl-id thread-id))
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
              (let [token-texts (obj-lookup flow-id (state-vars/form-token-id thread-id curr-form-id coor))]
                (doseq [text token-texts]
                  (un-highlight text))))))

        (when (or changing-frame?
                  (zero? curr-idx))
          ;; we are leaving a frame with this jump, or its the first trace
          ;; highlight all interesting tokens for the form we are currently in
          (let [interesting-expr-traces-grps (->> (indexer/interesting-expr-traces indexer next-form-id next-trace-idx)
                                                  (group-by :coor))]

            (doseq [[coor traces] interesting-expr-traces-grps]
              (let [token-id (state-vars/form-token-id thread-id next-form-id coor)
                    token-texts (obj-lookup flow-id token-id)]
                (doseq [text token-texts]
                  (arm-interesting text traces)
                  (highlight-interesting text))))))

        ;; "unhighlight" prev executing tokens
        (when (and (utils/exec-trace? curr-trace))
          (let [curr-token-texts (obj-lookup flow-id (state-vars/form-token-id thread-id
                                                                               (:form-id curr-trace)
                                                                               (:coor curr-trace)))]
            (doseq [text curr-token-texts]
              (if (= curr-form-id next-form-id)
                (highlight-interesting text)
                (un-highlight text)))))

        ;; highlight executing tokens
        (when (utils/exec-trace? next-trace)
          (let [next-token-texts (obj-lookup flow-id (state-vars/form-token-id thread-id
                                                                               (:form-id next-trace)
                                                                               (:coor next-trace)))]
            (doseq [text next-token-texts]
              (highlight-executing text))))

        ;; update reusult panel
        (update-result-pane flow-id thread-id (:result next-trace))

        ;; update locals panel
        (update-locals-pane flow-id thread-id (indexer/bindings-for-trace indexer next-trace-idx))

        (state/set-trace-idx dbg-state flow-id thread-id next-trace-idx)))))

(defn- create-thread-controls-pane [flow-id thread-id]
  (let [prev-btn (doto (Button. "<")
                   (.setOnAction (event-handler
                                  [ev]
                                  (jump-to-coord flow-id
                                                 thread-id
                                                 (dec (state/current-trace-idx dbg-state flow-id thread-id))))))
        curr-trace-lbl (Label. "0")
        separator-lbl (Label. "/")
        thread-trace-count-lbl (Label. "-")
        _ (store-obj flow-id (state-vars/thread-curr-trace-lbl-id thread-id) curr-trace-lbl)
        _ (store-obj flow-id (state-vars/thread-trace-count-lbl-id thread-id) thread-trace-count-lbl)
        next-btn (doto (Button. ">")
                   (.setOnAction (event-handler
                                  [ev]
                                  (jump-to-coord flow-id
                                                 thread-id
                                                 (inc (state/current-trace-idx dbg-state flow-id thread-id))))))]

    (doto (HBox. (into-array Node [prev-btn curr-trace-lbl separator-lbl thread-trace-count-lbl next-btn]))
      (.setStyle "-fx-background-color: #ddd; -fx-padding: 10;"))))

(defn- create-thread-pane [flow-id thread-id]
  (let [thread-pane (VBox.)
        thread-controls-pane (create-thread-controls-pane flow-id thread-id)
        thread-tools-tab-pane (doto (TabPane.)
                               (.setTabClosingPolicy TabPane$TabClosingPolicy/UNAVAILABLE)
                               (.setSide (Side/BOTTOM)))
        code-tab (doto (Tab. "Code")
                   (.setContent (create-code-pane flow-id thread-id)))
        callstack-tree-tab (doto (Tab. "Call stack")
                             (.setContent (create-call-stack-tree-pane flow-id thread-id))
                             (.setOnSelectionChanged (event-handler [_] (update-call-stack-tree-pane flow-id thread-id))))]

    ;; make thread-tools-tab-pane take the full height
    (-> thread-tools-tab-pane
        .prefHeightProperty
        (.bind (.heightProperty thread-pane)))

    (-> thread-tools-tab-pane
        .getTabs
        (.addAll [code-tab callstack-tree-tab]))

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

  (let [tab-pane (doto (TabPane.) ;;TODO: make flows closable
                   (.setTabClosingPolicy TabPane$TabClosingPolicy/UNAVAILABLE))]
    (store-obj "flows_tabs_pane" tab-pane)
    tab-pane))
