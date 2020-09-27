(ns flow-storm.tracer
  (:require  [taoensso.sente  :as sente]
             [clojure.core.async :refer [take!]]))

(defonce send-fn-a (atom nil))
(defonce pre-conn-events-holder (atom []))
(def ^:dynamic *flow-id* nil)

(defn hold-event [event]
  (println "[Holding]" event)
  (swap! pre-conn-events-holder conj event))

(defn ws-send [event]
  ((or @send-fn-a hold-event) event))

(defn init-trace [traced-form-id form-flow-id form]
  (ws-send [:flow-storm/init-trace {:flow-id *flow-id* :form-id traced-form-id :form-flow-id form-flow-id :form (pr-str form)}]))

(defn trace-and-return [result {:keys [coor outer-form? form-id form-flow-id]} orig-form]
  (ws-send [:flow-storm/add-trace (cond-> {:flow-id *flow-id*
                                           :form-id form-id
                                           :form-flow-id form-flow-id
                                           :coor coor
                                           :result (binding [*print-length* (or *print-length* 50)]
                                                     (pr-str result))}
                                    outer-form? (assoc :outer-form? true))])
  result)

(defn bound-trace [symb val {:keys [coor form-id form-flow-id]}]
  (ws-send [:flow-storm/add-bind-trace {:flow-id *flow-id*
                                        :form-id form-id
                                        :form-flow-id form-flow-id
                                        :coor coor
                                        :symbol (name symb)
                                        :value (binding [*print-length* (or *print-length* 50)]
                                                                                                                      (pr-str val))}]))

(defn connect
  ([] (connect nil))
  ([{:keys [host port]}]
   (let [{:keys [chsk ch-recv send-fn state]} (sente/make-channel-socket-client! "/chsk"  nil {:type :ws
                                                                                               :client-id "tracer"
                                                                                               :host (or host "localhost")
                                                                                               :port (or port 7722)})
         holded-events @pre-conn-events-holder]
     (take! ch-recv (fn [{:keys [event]}]
                      (when (= (first event) :chsk/state)
                        (reset! send-fn-a send-fn)
                        (println "Ws connection ready, re playing " (count holded-events) "events")
                        (doseq [ev holded-events]
                          (send-fn ev))))))))
