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
(s/def :form/str string?)
(s/def :form/pprint-tokens (s/coll-of any? :kind vector?)) ;; TODO: finish this
(s/def :flow/form (s/keys :req [:form/id :form/ns :form/str :form/pprint-tokens]))
(s/def :flow/forms (s/map-of :form/id :flow/form))

(s/def :thread/exec-trace (s/or :fn-call ::fn-call-trace :expr ::exec-trace))

(s/def :thread/traces (s/coll-of :thread/exec-trace :kind vector?))

(s/def :thread/curr-trace-idx number?)

(s/def :thread/execution (s/keys :req [:thread/traces
                                       :thread/curr-trace-idx]))

(s/def :thread/callstack-tree any?) ;; TODO: finish this

(s/def :thread/bind-traces (s/coll-of ::bind-trace :kind vector?))
(s/def :thread/hot-coords (s/coll-of :form/coord :kind set?))

(s/def ::thread (s/keys :req [:thread/id
                              :thread/execution
                              :thread/callstack-tree
                              :thread/bind-traces
                              :thread/hot-coords
                              ]))

(s/def :flow/threads (s/map-of :thread/id ::thread))

(s/def ::flow (s/keys :req [:flow/id
                            :flow/forms
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
                                   (s/explain ::state next-state)
                                   false)

                                 true))))

(defn init-state! []
  (reset! *state initial-state))
