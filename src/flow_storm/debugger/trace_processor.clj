(ns flow-storm.debugger.trace-processor
  (:require [flow-storm.debugger.state :as dbg-state]
            [flow-storm.debugger.ui.main :as ui-main]
            [flow-storm.debugger.ui.flows :as ui-flows]
            flow-storm.tracer)
  (:import [flow_storm.tracer InitTrace ExecTrace FnCallTrace BindTrace]))

(defprotocol ProcessTrace
  (process [_]))


(defn increment-trace-counter []
  (swap! dbg-state/*state update :trace-counter inc)
  (ui-main/update-trace-counter (:trace-counter @dbg-state/*state)))

    ;; pub fn add_flow_form(&mut self, flow_id: FlowId, form_id: FormId, form: Form, timestamp: u64) {
    ;;     if let hash_map::Entry::Vacant(e) = self.flows.entry(flow_id) {
    ;;         // Initialize the flow, then add form
    ;;         let mut flow = Flow {
    ;;             flow_id,
    ;;             forms: SortedForms::new(),
    ;;             threads: HashMap::new(),
    ;;             selected_thread_id: None,
    ;;             timestamp,
    ;;         };
    ;;         flow.forms.insert(form_id, form);
    ;;         e.insert(flow);

    ;;         // make this flow selected
    ;;         self.selected_flow_id = Some(flow_id);
    ;;     } else {
    ;;         // Add form to the flow
    ;;         if let Some(flow) = self.flows.get_mut(&flow_id) {
    ;;             flow.forms.insert(form_id, form);
    ;;         };
    ;;     }
    ;;}

(defn create-empty-flow [state flow-id timestamp]
  (assoc-in state [:flows flow-id] {:flow/id flow-id
                                    :flow/forms {}
                                    :flow/threads {}
                                    :timestamp timestamp
                                    }))

(defn form-pprint-tokens [form-str]
  ;; TODO: print tokens
  [])

(defn create-form [id ns form-str]
  {:form/id id
   :form/ns ns
   :form/str form-str
   :form/pprint-tokens (form-pprint-tokens form-str)})

(extend-protocol ProcessTrace
  InitTrace
  (process [{:keys [flow-id form-id form ns timestamp]}]
    (when-not (contains? (:flows @dbg-state/*state) flow-id)
      ;; flow doesn't exist, create one
      (swap! dbg-state/*state create-empty-flow flow-id timestamp)
      (ui-flows/create-empty-flow flow-id))

    (swap! dbg-state/*state
           (fn [state]
             (let [new-form (create-form form-id ns form)]
               (-> state
                  (assoc-in [:flows flow-id :flow/forms form-id] new-form))))))

  ExecTrace
  (process [trace]
    )

  FnCallTrace
  (process [trace]
    )

  BindTrace
  (process [trace]
    ))

(defn dispatch-trace [trace]
  (process trace)
  (increment-trace-counter))
