(ns flow-storm.debugger.state
  (:require [clojure.spec.alpha :as s]
            [flow-storm.debugger.callstack-tree :as callstack-tree]
            [flow-storm.tracer])
  (:import [flow_storm.tracer InitTrace BindTrace FnCallTrace ExecTrace]))

(s/def ::timestamp number?)
(s/def :flow/id number?)
(s/def :form/id number?)
(s/def :thread/id number?)
(s/def :form/coord (s/coll-of number? :kind vector?))

(s/def ::init-trace    #(instance? InitTrace %))
(s/def ::bind-trace    #(instance? BindTrace %))
(s/def ::fn-call-trace #(instance? FnCallTrace %))
(s/def ::exec-trace    #(instance? ExecTrace %))

(s/def :form/ns string?)
(s/def :form/form any?)

(s/def :thread/form (s/keys :req [:form/id
                                  :form/ns
                                  :form/form]))
(s/def :thread/forms (s/map-of :form/id :thread/form))

(s/def :thread/exec-trace (s/or :fn-call ::fn-call-trace :expr ::exec-trace))

(s/def :thread/traces (s/coll-of :thread/exec-trace :kind vector?))

(s/def :thread/curr-trace-idx (s/nilable number?))

(s/def :thread/execution (s/keys :req [:thread/traces
                                       :thread/curr-trace-idx]))

(s/def :thread/bind-traces (s/coll-of ::bind-trace :kind vector?))

(s/def :thread/callstack-tree any?) ;; TODO: finish this

(s/def :thread/forms-hot-traces (s/map-of :form/id (s/coll-of ::exec-trace)))

(s/def ::thread (s/keys :req [:thread/id
                              :thread/forms
                              :thread/execution
                              :thread/callstack-tree
                              :thread/forms-hot-traces]))

(s/def :flow/threads (s/map-of :thread/id ::thread))

(s/def ::flow (s/keys :req [:flow/id
                            :flow/threads]
                      :req-un [::timestamp]))

(s/def :state/flows (s/map-of :flow/id ::flow))
(s/def :state/selected-flow-id (s/nilable :flow/id))
(s/def :state/trace-counter number?)

(s/def ::state (s/keys :req-un [:state/flows
                                :state/selected-flow-id
                                :state/trace-counter]))

(def initial-state {:trace-counter 0
                    :flows {}
                    :selected-flow-id nil})

(def *state (atom initial-state

                  :validator (fn [next-state]
                               (if-not (s/valid? ::state next-state)
                                 (do
                                   (tap> (str "STATE Error" (with-out-str (s/explain ::state next-state))))
                                   false)

                                 true))))

(defn init-state! []
  (reset! *state initial-state))

;;;;;;;;;;;
;; Utils ;;
;;;;;;;;;;;

(defn exec-trace? [trace]
  (instance? ExecTrace trace))

;;;;;;;;;;;
;; Flows ;;
;;;;;;;;;;;

(defn flow [state flow-id]
  (get-in state [:flows flow-id]))

(defn add-flow [state flow]
  (assoc-in state [:flows (:flow/id flow)] flow))

(defn empty-flow [flow-id timestamp]
  {:flow/id flow-id
   :flow/threads {}
   :timestamp timestamp})

;;;;;;;;;;;;;
;; Threads ;;
;;;;;;;;;;;;;

(defn thread [state flow-id thread-id]
  (get-in state [:flows flow-id :flow/threads thread-id]))

(defn add-thread [state flow-id thread]
  (assoc-in state [:flows flow-id :flow/threads (:thread/id thread)] thread))

(defn empty-thread [thread-id]
  {:thread/id thread-id
   :thread/forms {}
   :thread/execution {:thread/traces []
                      :thread/curr-trace-idx nil}
   :thread/callstack-tree nil
   :thread/forms-hot-traces {}})

(defn next-trace-idx [state flow-id thread-id]
  (-> state
      (get-in [:flows flow-id :flow/threads thread-id :thread/execution :thread/traces])
      count))

(defn add-execution-trace* [thread-execution trace]
  (-> thread-execution
      (update :thread/traces conj trace)
      (update :thread/curr-trace-idx #(or % 0))))

(defn add-execution-trace [state {:keys [flow-id thread-id form-id] :as trace}]
  (let [next-idx (next-trace-idx state flow-id thread-id)]
    (update-in state [:flows flow-id :flow/threads thread-id]
              (fn [thread]
                (-> thread
                    (update :thread/callstack-tree callstack-tree/process-exec-trace next-idx trace)
                    (update :thread/execution add-execution-trace* trace)
                    (update-in [:thread/forms-hot-traces form-id] conj trace))))))

(defn add-fn-call-trace [state {:keys [flow-id thread-id] :as trace}]
  (let [next-idx (next-trace-idx state flow-id thread-id)]
    (update-in state [:flows flow-id :flow/threads thread-id]
               (fn [thread]
                 (-> thread
                     (update :thread/callstack-tree (fn [cs-tree]
                                                      (if (zero? next-idx)
                                                        (callstack-tree/make-call-tree trace)
                                                        (callstack-tree/process-fn-call-trace cs-tree next-idx trace))))
                     (update :thread/execution add-execution-trace* trace))))))

(defn add-bind-trace [state {:keys [flow-id thread-id] :as trace}]
  (update-in state [:flows flow-id :flow/threads thread-id :thread/callstack-tree]
             callstack-tree/process-bind-trace trace))

(defn thread-curr-trace-idx [state flow-id thread-id]
  (get-in state [:flows flow-id :flow/threads thread-id :thread/execution :thread/curr-trace-idx]))

(defn set-thread-curr-trace-idx [state flow-id thread-id idx]
  (assoc-in state [:flows flow-id :flow/threads thread-id :thread/execution :thread/curr-trace-idx] idx))

(defn thread-trace [state flow-id thread-id idx]
  (get-in state [:flows flow-id :flow/threads thread-id :thread/execution :thread/traces idx]))

(defn thread-exec-trace-count [state flow-id thread-id]
  (-> state
      (get-in [:flows flow-id :flow/threads thread-id :thread/execution :thread/traces])
      count))

(defn thread-callstack-tree [state flow-id thread-id]
  (-> state
      (get-in [:flows flow-id :flow/threads thread-id :thread/callstack-tree])
      (callstack-tree/callstack-tree)))

(defn thread-find-frame [state flow-id thread-id trace-idx]
  (-> state
      (get-in [:flows flow-id :flow/threads thread-id :thread/callstack-tree])
      (callstack-tree/find-frame trace-idx)))

(defn bindings-for-trace [state flow-id thread-id trace-idx]
  (-> (thread-find-frame state flow-id thread-id trace-idx)
      :bindings))

(defn interesting-expr-traces [state flow-id thread-id form-id]
  (get-in state [:flows flow-id :flow/threads thread-id :thread/forms-hot-traces form-id]))

;;;;;;;;;;;
;; Forms ;;
;;;;;;;;;;;

(defn create-form [form-id form-ns form]
  {:form/id form-id
   :form/ns form-ns
   :form/form form})

(defn add-form [state flow-id thread-id form]
  (assoc-in state [:flows flow-id :flow/threads thread-id :thread/forms (:form/id form)] form))
