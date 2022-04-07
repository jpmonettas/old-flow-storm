(ns flow-storm.debugger.ui.flows.code
  (:require [clojure.pprint :as pp]
            [flow-storm.debugger.form-pprinter :as form-pprinter]
            [flow-storm.debugger.trace-indexer.protos :as indexer]
            [flow-storm.debugger.ui.flows.components :as flow-cmp]
            [flow-storm.debugger.ui.utils :as ui-utils :refer [event-handler v-box h-box label]]
            [flow-storm.debugger.ui.state-vars :refer [store-obj obj-lookup] :as ui-vars]
            [flow-storm.debugger.state :as state :refer [dbg-state]]
            [flow-storm.debugger.target-commands :as target-commands]
            [flow-storm.trace-types :as trace-types])
  (:import [javafx.scene.control Label ListView ScrollPane Tab TabPane TabPane$TabClosingPolicy SplitPane]
           [javafx.collections FXCollections ObservableList]
           [javafx.scene Node]
           [javafx.geometry Orientation Pos]
           [javafx.scene.text TextFlow Text Font]
           [javafx.scene.input MouseEvent MouseButton]))

(declare jump-to-coord)

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

(defn- create-locals-pane [flow-id thread-id]
  (let [observable-bindings-list (FXCollections/observableArrayList)
        cell-factory (proxy [javafx.util.Callback] []
                       (call [lv]
                         (ui-utils/create-list-cell-factory
                          (fn [list-cell symb-val]
                            (let [symb-lbl (doto (label (first symb-val))
                                             (.setPrefWidth 100))
                                  val-lbl (label  (flow-cmp/format-value-short (second symb-val)))
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

(defn- update-thread-trace-count-lbl [flow-id thread-id cnt]
  (let [[^Label lbl] (obj-lookup flow-id (ui-vars/thread-trace-count-lbl-id thread-id))]
    (.setText lbl (str cnt))))

(defn- highlight-executing [token-text]
  (ui-utils/rm-class token-text "interesting")
  (ui-utils/add-class token-text "executing"))

(defn- highlight-interesting [token-text]
  (ui-utils/rm-class token-text "executing")
  (ui-utils/add-class token-text "interesting"))

(defn- unhighlight-form [flow-id thread-id form-id]
  (let [[form-pane] (obj-lookup flow-id (ui-vars/thread-form-box-id thread-id form-id))]
    (doto form-pane
      (.setOnMouseClicked (event-handler [_])))
    (ui-utils/rm-class form-pane "form-background-highlighted")))

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

(defn- un-highlight [^Text token-text]
  (ui-utils/rm-class token-text "interesting")
  (ui-utils/rm-class token-text "executing")
  (.setOnMouseClicked token-text (event-handler [_])))

(defn- arm-interesting [^Text token-text traces]
  (let [{:keys [flow-id thread-id]} (first traces)]

    (if (> (count traces) 1)
      (let [ctx-menu-options (->> traces
                              (map (fn [t]
                                     (let [tidx (-> t meta :trace-idx)]
                                       {:text (format "%d | %s" tidx (flow-cmp/format-value-short (:result t)))
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

(defn jump-to-coord [flow-id thread-id next-trace-idx]
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
        (when (trace-types/exec-trace? curr-trace)
          (let [curr-token-texts (obj-lookup flow-id (ui-vars/form-token-id thread-id
                                                                            (:form-id curr-trace)
                                                                            (:coor curr-trace)))]
            (doseq [text curr-token-texts]
              (if (= curr-form-id next-form-id)
                (highlight-interesting text)
                (un-highlight text)))))

        ;; highlight executing tokens
        (when (trace-types/exec-trace? next-trace)
          (let [next-token-texts (obj-lookup flow-id (ui-vars/form-token-id thread-id
                                                                            (:form-id next-trace)
                                                                            (:coor next-trace)))]
            (doseq [text next-token-texts]
              (highlight-executing text))))

        ;; update reusult panel
        (flow-cmp/update-pprint-pane flow-id thread-id "expr_result" (:result next-trace))

        ;; update locals panel
        (update-locals-pane flow-id thread-id (indexer/bindings-for-trace indexer next-trace-idx))

        (state/set-trace-idx dbg-state flow-id thread-id next-trace-idx)))))

(defn- create-forms-pane [flow-id thread-id]
  (let [box (doto (v-box [])
              (.setSpacing 5))
        scroll-pane (ScrollPane.)]
    (.setContent scroll-pane box)
    (store-obj flow-id (ui-vars/thread-forms-box-id thread-id) box)
    (store-obj flow-id (ui-vars/thread-forms-scroll-id thread-id) scroll-pane)
    scroll-pane))

(defn- create-result-pane [flow-id thread-id]
  (let [tools-tab-pane (doto (TabPane.)
                         (.setTabClosingPolicy TabPane$TabClosingPolicy/UNAVAILABLE))
        pprint-tab (doto (Tab. "Pprint")
                     (.setContent (flow-cmp/create-pprint-pane flow-id thread-id "expr_result")))
        tree-tab (doto (Tab. "Tree")
                   (.setDisable true)
                   (.setContent (flow-cmp/create-result-tree-pane flow-id thread-id)))]
    (-> tools-tab-pane
        .getTabs
        (.addAll [pprint-tab tree-tab]))

    tools-tab-pane))

(defn create-code-pane [flow-id thread-id]
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
