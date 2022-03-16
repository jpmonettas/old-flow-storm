(ns flow-storm.debugger.state
  (:require [clojure.spec.alpha :as s]
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

(s/def :thread/form (s/keys :req [:form/id :form/ns :form/form]))
(s/def :thread/forms (s/map-of :form/id :thread/form))

(s/def :thread/exec-trace (s/or :fn-call ::fn-call-trace :expr ::exec-trace))

(s/def :thread/traces (s/coll-of :thread/exec-trace :kind vector?))

(s/def :thread/curr-trace-idx (s/nilable number?))

(s/def :thread/execution (s/keys :req [:thread/traces
                                       :thread/curr-trace-idx]))

(s/def :thread/bind-traces (s/coll-of ::bind-trace :kind vector?))

(s/def ::thread (s/keys :req [:thread/id
                              :thread/forms
                              :thread/execution
                              :thread/bind-traces]))

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
   :thread/bind-traces []})

;;;;;;;;;;;
;; Forms ;;
;;;;;;;;;;;

(defn create-form [form-id form-ns form]
  {:form/id form-id
   :form/ns form-ns
   :form/form form})

(defn add-form [state flow-id thread-id form]
  (assoc-in state [:flows flow-id :flow/threads thread-id :thread/forms (:form/id form)] form))
