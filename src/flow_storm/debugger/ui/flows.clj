(ns flow-storm.debugger.ui.flows
  (:require [flow-storm.debugger.ui.state-vars :refer [store-obj obj-lookup] :as state-vars]
            [flow-storm.debugger.ui.utils :as ui-utils :refer [event-handler run-later run-now]]
            [clojure.pprint :as pp]
            [flow-storm.debugger.state :as state])
  (:import [javafx.scene.layout BorderPane GridPane HBox Priority Pane VBox]
           [javafx.scene.control Button Label ListView ListCell TextArea Tab TabPane TabPane$TabClosingPolicy SplitPane]
           [javafx.scene.text TextFlow Text Font]
           [javafx.scene Node]
           [javafx.scene.paint Color]
           [javafx.geometry Side Orientation Pos]
           [javafx.collections FXCollections]))

(defn create-empty-flow [flow-id]
  (run-now
   (let [[flows-tabs-pane] (obj-lookup "flows_tabs_pane")
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
              (.setSpacing 5))]
    (store-obj flow-id (state-vars/thread-forms-box-id thread-id) box)
    box))

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
  (let [[text-area] (obj-lookup flow-id (state-vars/thread-result-text-area-id thread-id))
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

(defn- format-local-val [v]
  (let [s (pr-str v)]
    (subs s 0 (min 80 (count s)))))

(defn- create-locals-pane [flow-id thread-id]
  (let [observable-bindings-list (FXCollections/observableArrayList)
        cell-factory (proxy [javafx.util.Callback] []
                       (call [lv]
                         (proxy [ListCell] []
                           (updateItem [symb-val empty?]
                             (proxy-super updateItem symb-val empty?)
                             (if empty?
                               (.setText this nil)
                               (let [symb-lbl (doto (Label. (first symb-val))
                                                (.setPrefWidth 100))
                                     val-lbl (Label.  (format-local-val (second symb-val)))
                                     hbox (HBox. (into-array Node [symb-lbl val-lbl]))]
                                 (.setGraphic this hbox)))))))
        locals-list-view (doto (ListView. observable-bindings-list)
                           (.setEditable false)
                           (.setCellFactory cell-factory))]
    (store-obj flow-id (state-vars/thread-locals-list-id thread-id) observable-bindings-list)
    locals-list-view))

(defn- update-locals-pane [flow-id thread-id bindings]
  (let [[observable-bindings-list] (obj-lookup flow-id (state-vars/thread-locals-list-id thread-id))]
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

(defn- create-call-stack-tree-pane []
  (Label. "Call stack tree"))

(defn- highlight [token-text]
  (doto token-text
    (.setFill (Color/RED))))

(defn- un-highlight [token-text]
  (doto token-text
    (.setFill (Color/BLACK))))

(defn- jump-to-coord [flow-id thread-id new-trace-idx]
  (let [state @state/*state
        trace-count (state/thread-exec-trace-count state flow-id thread-id)]
    (when (<= 0 new-trace-idx (dec trace-count))
      (let [curr-idx (state/thread-curr-trace-idx state flow-id thread-id)
            from-trace (state/thread-trace state flow-id thread-id curr-idx)
            to-trace (state/thread-trace state flow-id thread-id new-trace-idx)
            [curr_trace_lbl] (obj-lookup flow-id (state-vars/thread-curr-trace-lbl-id thread-id))]

        ;; update thread current trace lable
        (.setText curr_trace_lbl (str new-trace-idx))

        ;; highlight executing tokens
        (when (state/exec-trace? from-trace)
          (let [from-token-texts (obj-lookup flow-id (state-vars/form-token-id thread-id
                                                                              (:form-id from-trace)
                                                                              (:coor from-trace)))]
            (doseq [text from-token-texts]
              (un-highlight text))))

        ;; unhighlight executing tokens
        (when (state/exec-trace? to-trace)
          (let [to-token-texts (obj-lookup flow-id (state-vars/form-token-id thread-id
                                                                            (:form-id to-trace)
                                                                            (:coor to-trace)))]

            (doseq [text to-token-texts]
              (highlight text))))

        ;; update reusult panel
        (update-result-pane flow-id thread-id (:result to-trace))

        ;; update locals panel
        (update-locals-pane flow-id thread-id (state/bindings-for-trace state flow-id thread-id new-trace-idx))

        ;; TODO update form clickable tokens

        (swap! state/*state state/set-thread-curr-trace-idx flow-id thread-id new-trace-idx)))))

(defn- create-thread-controls-pane [flow-id thread-id]
  (let [prev-btn (doto (Button. "<")
                   (.setOnAction (event-handler
                                  [_]
                                  )))
        curr-trace-lbl (Label. "0")
        _ (store-obj flow-id (state-vars/thread-curr-trace-lbl-id thread-id) curr-trace-lbl)
        next-btn (doto (Button. ">")
                   (.setOnAction (event-handler
                                  [ev]
                                  (jump-to-coord flow-id
                                                 thread-id
                                                 (inc (state/thread-curr-trace-idx @state/*state flow-id thread-id))))))]

    (doto (HBox. (into-array Node [prev-btn curr-trace-lbl next-btn]))
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
                             (.setContent (create-call-stack-tree-pane)))]

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

     (-> forms-box
         .getChildren
         (.add 0 form-pane)))))

(defn main-pane []

  (let [tab-pane (doto (TabPane.) ;;TODO: make flows closable
                   (.setTabClosingPolicy TabPane$TabClosingPolicy/UNAVAILABLE))]
    (store-obj "flows_tabs_pane" tab-pane)
    tab-pane))
