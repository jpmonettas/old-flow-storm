(ns flow-storm.debugger.ui.flows
  (:require [flow-storm.debugger.ui.state-vars :refer [store-obj obj-lookup] :as state-vars]
            [flow-storm.debugger.ui.utils :as ui-utils :refer [event-handler run-later run-now]]
            [clojure.pprint :as pp]
            [flow-storm.debugger.state :as state]
            [clojure.string :as str])
  (:import [javafx.scene.layout BorderPane Background BackgroundFill CornerRadii GridPane HBox Priority Pane VBox]
           [javafx.scene.control Button Label ListView ListCell ScrollPane TreeCell TextArea Tab TabPane TabPane$TabClosingPolicy TreeView TreeItem  SplitPane]
           [javafx.scene.text TextFlow Text Font]
           [javafx.scene Node]
           [javafx.scene.paint Color]
           [javafx.geometry Insets Side Orientation Pos]
           [javafx.collections FXCollections ObservableList]
           [javafx.scene.input MouseEvent]))

(declare jump-to-coord)

(defn- format-value-short [v]
  (let [max-len 80
        s (pr-str v)
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
  (let [state @state/*state
        lazy-tree-item (fn lazy-tree-item [frame]
                         (let [;; TODO: get :ret here from the mut-ref
                               {:keys [calls]} frame]
                           (proxy [TreeItem] [frame]
                             (getChildren []
                               (let [^ObservableList super-childrens (proxy-super getChildren)]
                                 (if (.isEmpty super-childrens)
                                   (let [new-children (->> calls
                                                           (remove (fn [{:keys [fn-name fn-ns]}]
                                                                     (state/callstack-tree-hidden? state flow-id thread-id fn-name fn-ns)))
                                                           (map lazy-tree-item)
                                                           (into-array TreeItem))]
                                     (.setAll super-childrens new-children)
                                     super-childrens)
                                   super-childrens)))
                             (isLeaf [] (empty? calls)))))
        root-item (lazy-tree-item (state/thread-callstack-tree @state/*state flow-id thread-id))
        [tree-view] (obj-lookup flow-id (state-vars/thread-callstack-tree-view-id thread-id))]

    (.setRoot ^TreeView tree-view root-item)))

(defn- create-call-stack-tree-node [{:keys [call-trace-idx fn-name fn-ns args calls]} flow-id thread-id]
  (let [ns-lbl (Label. (str fn-ns "/"))
        fn-name-lbl (doto (Label. fn-name)
                      (.setStyle "-fx-font-weight: bold;"))
        args-lbl (Label. (str " " (format-value-short args)))
        node-box (HBox. (into-array Node [(Label. "(") ns-lbl fn-name-lbl args-lbl (Label. ")")]))
        ctx-menu-options [{:text "Goto trace"
                           :on-click #(jump-to-coord flow-id thread-id call-trace-idx)}
                          {:text (format "Hide %s/%s from this tree" fn-ns fn-name)
                           :on-click #(do
                                        (swap! state/*state state/callstack-tree-hide-fn flow-id thread-id fn-name fn-ns)
                                        (update-call-stack-tree-pane flow-id thread-id))}]
        ctx-menu (ui-utils/make-context-menu ctx-menu-options)]
    (doto node-box
      (.setOnMouseClicked (event-handler
                           [^MouseEvent ev]
                           (.show ctx-menu
                                  node-box
                                  (.getScreenX ev)
                                  (.getScreenY ev)))))))

(defn- create-call-stack-tree-pane [flow-id thread-id]
  (let [update-btn (doto (Button. "Update")
                     (.setOnAction (event-handler
                                    [_]
                                    (update-call-stack-tree-pane flow-id thread-id))))
        cell-factory (proxy [javafx.util.Callback] []
                       (call [tv]
                         (proxy [TreeCell] []
                           (updateItem [frame empty?]
                             (proxy-super updateItem frame empty?)
                             (if empty?
                               (.setGraphic this nil)
                               (.setGraphic this (create-call-stack-tree-node frame flow-id thread-id)))))))
        tree-view (doto (TreeView.)
                    (.setEditable false)
                    (.setCellFactory cell-factory))]
    (store-obj flow-id (state-vars/thread-callstack-tree-view-id thread-id) tree-view)
    (VBox. (into-array Node [update-btn tree-view]))))


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

(defn- un-highlight [^Text token-text]
  (doto token-text
    (.setFill (Color/BLACK))
    (.setStyle "-fx-cursor: pointer;")
    (.setOnMouseClicked (event-handler [_]))))

(defn- jump-to-coord [flow-id thread-id new-trace-idx]
  (let [state @state/*state
        trace-count (state/thread-exec-trace-count state flow-id thread-id)]
    (when (<= 0 new-trace-idx (dec trace-count))
      (let [curr-idx (state/thread-curr-trace-idx state flow-id thread-id)
            curr-trace (state/thread-trace state flow-id thread-id curr-idx)
            curr-form-id (:form-id curr-trace)
            next-trace (state/thread-trace state flow-id thread-id new-trace-idx)
            next-form-id (:form-id next-trace)
            [^Label curr_trace_lbl] (obj-lookup flow-id (state-vars/thread-curr-trace-lbl-id thread-id))
            curr-frame (state/thread-find-frame state flow-id thread-id curr-idx)
            next-frame (state/thread-find-frame state flow-id thread-id new-trace-idx)
            changing-frame? (not= curr-frame next-frame)
            changing-form? (not= curr-form-id next-form-id)]

        ;; update thread current trace lable
        (.setText curr_trace_lbl (str new-trace-idx))

        (when changing-form?
          ;; we are leaving a frame with this jump, so unhighlight all prev-form interesting tokens
          (let [curr-form-interesting-expr-traces (state/interesting-expr-traces state flow-id thread-id curr-form-id curr-idx)]
            (doseq [{:keys [coor]} curr-form-interesting-expr-traces]
              (let [token-texts (obj-lookup flow-id (state-vars/form-token-id thread-id curr-form-id coor))]
                (doseq [text token-texts]
                  (un-highlight text))))))

        (when (or changing-form?
                  changing-frame?
                  (zero? curr-idx))
          ;; we are leaving a frame with this jump, or its the first trace
          ;; highlight all interesting tokens for the form we are currently in and also scroll to that form
          (let [interesting-expr-traces-grps (->> (state/interesting-expr-traces state flow-id thread-id next-form-id new-trace-idx)
                                                  (group-by :coor))
                [next-form-pane]        (obj-lookup flow-id (state-vars/thread-form-box-id thread-id next-form-id))
                [thread-scroll-pane] (obj-lookup flow-id (state-vars/thread-forms-scroll-id thread-id))]
            (ui-utils/center-node-in-scroll-pane thread-scroll-pane next-form-pane)
            (doseq [[coor traces] interesting-expr-traces-grps]
              (let [token-id (state-vars/form-token-id thread-id next-form-id coor)
                    token-texts (obj-lookup flow-id token-id)]
                (doseq [text token-texts]
                  (arm-interesting text traces)
                  (highlight-interesting text))))))

        ;; "unhighlight" prev executing tokens
        (when (and (state/exec-trace? curr-trace))
          (let [curr-token-texts (obj-lookup flow-id (state-vars/form-token-id thread-id
                                                                               (:form-id curr-trace)
                                                                               (:coor curr-trace)))]
            (doseq [text curr-token-texts]
              (if (= curr-form-id next-form-id)
                (highlight-interesting text)
                (un-highlight text)))))

        ;; highlight executing tokens
        (when (state/exec-trace? next-trace)
          (let [next-token-texts (obj-lookup flow-id (state-vars/form-token-id thread-id
                                                                               (:form-id next-trace)
                                                                               (:coor next-trace)))]
            (doseq [text next-token-texts]
              (highlight-executing text))))

        ;; update reusult panel
        (update-result-pane flow-id thread-id (:result next-trace))

        ;; update locals panel
        (update-locals-pane flow-id thread-id (state/bindings-for-trace state flow-id thread-id new-trace-idx))

        (swap! state/*state state/set-thread-curr-trace-idx flow-id thread-id new-trace-idx)))))

(defn- create-thread-controls-pane [flow-id thread-id]
  (let [prev-btn (doto (Button. "<")
                   (.setOnAction (event-handler
                                  [ev]
                                  (jump-to-coord flow-id
                                                 thread-id
                                                 (dec (state/thread-curr-trace-idx @state/*state flow-id thread-id))))))
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
                                                 (inc (state/thread-curr-trace-idx @state/*state flow-id thread-id))))))]

    (doto (HBox. (into-array Node [prev-btn curr-trace-lbl separator-lbl thread-trace-count-lbl next-btn]))
      (.setStyle "-fx-background-color: #ddd; -fx-padding: 10;"))))

(defn update-thread-trace-count-lbl [flow-id thread-id cnt]
  (run-later
   (let [[^Label lbl] (obj-lookup flow-id (state-vars/thread-trace-count-lbl-id thread-id))]
     (.setText lbl (str cnt)))))

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

(defn add-form [flow-id thread-id form-id form-ns print-tokens]
  (run-now
   (let [text-font (Font/font "monospaced" 14)
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
         ns-label (doto (Label. (format "ns: %s" form-ns))
                    (.setFont (Font. 10)))

         form-header (doto (HBox. (into-array Node [ns-label]))
                       (.setAlignment (Pos/TOP_RIGHT)))
         form-text-flow (TextFlow. (into-array Text tokens-texts))
         form-pane (doto (VBox. (into-array Node [form-header form-text-flow]))
                          (.setStyle "-fx-padding: 10; -fx-background-color: #eee;"))]
     (store-obj flow-id (state-vars/thread-form-box-id thread-id form-id) form-pane)
     (-> forms-box
         .getChildren
         (.add 0 form-pane)))))

(defn main-pane []

  (let [tab-pane (doto (TabPane.) ;;TODO: make flows closable
                   (.setTabClosingPolicy TabPane$TabClosingPolicy/UNAVAILABLE))]
    (store-obj "flows_tabs_pane" tab-pane)
    tab-pane))
