(ns flow-storm.debugger.ui.state-vars)

(defonce main-pane nil)
(defonce ctx-menu nil)
(defonce stage nil)
(defonce scene nil)

;; Because scene.lookup doesn't work if you lookup before a layout pass
;; So adding a node and looking it up quickly sometimes it doesn't work

;; ui objects references with a static application lifetime
;; objects stored here will not be collected ever
(defonce ui-objs (atom {}))

;; ui objects with a flow lifetime
;; when a flow is discarded all this objects should be collected after
;; `clean-flow-objs` is called
(defonce flows-ui-objs (atom {}))

(defn store-obj

  ([obj-id obj-ref]
   (swap! ui-objs update obj-id conj obj-ref)
   #_(tap> (format "Stored obj at key: %s" obj-id)))

  ([flow-id obj-id obj-ref]
   (swap! flows-ui-objs update flow-id (fn [flow-objs]
                                         (update flow-objs obj-id conj obj-ref)))
   #_(tap> (format "Stored %d flow obj at key %s" flow-id obj-id))))

(defn obj-lookup
  ([obj-id]
   (let [all-objs @ui-objs
         objs (get all-objs obj-id)]
     (when-not objs
       (tap> (format "Object not found %s" obj-id))
       (tap> (keys all-objs)))
     objs))

  ([flow-id obj-id]

   (let [all-objs @flows-ui-objs
         objs (get-in all-objs [flow-id obj-id])]
     (when-not objs
       (tap> (format "Flow object not found flow-id: %d obj-id: %s" flow-id obj-id))
       (tap> (keys (get all-objs flow-id))))
     objs)))

(defn clean-flow-objs [flow-id]
  (update flows-ui-objs dissoc flow-id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for creating ui components ids ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn form-token-id [thread-id form-id coord]
  (format "form_token_%d_%d_%d" thread-id form-id (hash coord)))

(defn thread-curr-trace-lbl-id [thread-id]
  (format "thread_curr_trace_lbl_%d" thread-id))

(defn thread-trace-count-lbl-id [thread-id]
  (format "thread_trace_count_lbl_%d" thread-id))

(defn thread-forms-box-id [thread-id]
  (format "forms_box_%d" thread-id))

(defn thread-form-box-id [thread-id form-id]
  (format "form_box_%d_%d" thread-id form-id))

(defn thread-forms-scroll-id [thread-id]
  (format "forms_scroll_%d" thread-id))

(defn thread-result-text-area-id [thread-id]
  (format "result_text_area_%d" thread-id))

(defn thread-locals-list-id [thread-id]
  (format "locals_list_%d" thread-id))

(defn thread-callstack-tree-view-id [thread-id]
  (format "callstack_tree_view_%s" thread-id))
