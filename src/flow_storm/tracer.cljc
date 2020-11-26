(ns flow-storm.tracer
  (:require  [taoensso.sente  :as sente]
             [clojure.core.async :refer [take!]]
             [editscript.core :as edit.core]
             [editscript.edit :as edit.edit]))

(defonce send-fn-a (atom nil))
(defonce pre-conn-events-holder (atom []))
(def ^:dynamic *flow-id* nil)

(defn get-timestamp []
  #?(:cljs (.getTime (js/Date.))
     :clj (System/currentTimeMillis)))

(defn serialize-val [v]
  (binding [*print-length* (or *print-length* 50)]
    (pr-str v)))

(defn- hold-event
  "Collect the event in a internal atom (`pre-conn-events-holder`).
  Ment to be used by the websocket ws-send to hold events
  while a connection to the debugger is not ready."
  [event]
  (println "[Holding]" event)
  (swap! pre-conn-events-holder conj event))

(defn ws-send
  "Send the event thru the connected websocket. If the websocket
  connection is not ready, hold it in `pre-conn-events-holder`"
  [event]
  ((or @send-fn-a hold-event) event))

(defn init-trace
  "Instrumentation function. Sends the `:flow-storm/init-trace` trace"
  [{:keys [form-id form-flow-id flow-id args-vec fn-name]} form]
  (let [trace-data (cond-> {:flow-id *flow-id*
                            :form-id form-id
                            :form-flow-id form-flow-id
                            :form (pr-str form)
                            :timestamp (get-timestamp)}
                     args-vec (assoc :args-vec (serialize-val args-vec))
                     fn-name  (assoc :fn-name fn-name)
                     flow-id  (assoc :fixed-flow-id-starter? true))]
    (ws-send [:flow-storm/init-trace trace-data])))

(defn trace-and-return
  "Instrumentation function. Sends the `:flow-storm/add-trace` trace and returns the result."
  [result err {:keys [coor outer-form? form-id form-flow-id]} orig-form]
  (let [trace-data (cond-> {:flow-id *flow-id*
                            :form-id form-id
                            :form-flow-id form-flow-id
                            :coor coor
                            :timestamp (get-timestamp)}
                     (not err)   (assoc :result (serialize-val result))
                     err         (assoc :err {:error/message (:message err)})
                     outer-form? (assoc :outer-form? true))]

    (ws-send [:flow-storm/add-trace trace-data])

    result))

(defn bound-trace
  "Instrumentation function. Sends the `:flow-storm/add-bind-trace` trace"
  [symb val {:keys [coor form-id form-flow-id]}]
  (let [trace-data {:flow-id *flow-id*
                    :form-id form-id
                    :form-flow-id form-flow-id
                    :coor coor
                    :timestamp (get-timestamp)
                    :symbol (name symb)
                    :value (serialize-val val)}]
    (ws-send [:flow-storm/add-bind-trace trace-data])))

(defn ref-init-trace
  "Sends the `:flow-storm/ref-init-trace` trace"
  [ref-id ref-name init-val]
  (let [trace-data {:ref-id ref-id
                    :ref-name ref-name
                    :init-val (serialize-val init-val)
                    :timestamp (get-timestamp)}]
    (ws-send [:flow-storm/ref-init-trace trace-data])))

(defn ref-trace
  "Sends the `:flow-storm/ref-trace` trace"
  [ref-id patch]
  (let [trace-data {:ref-id ref-id
                    :patch (pr-str patch)
                    :timestamp (get-timestamp)}]
    (ws-send [:flow-storm/ref-trace trace-data])))

(defn trace-ref [ref {:keys [ref-name ignore-keys]}]
  (let [ref-id (hash ref)
        rm-ignored-keys (fn [v]
                          (if (and (seq ignore-keys) (map? v))
                            (apply (partial dissoc v) ignore-keys)
                            v))
        ref-init-val (-> @ref
                         rm-ignored-keys)]
    
    (ref-init-trace ref-id ref-name ref-init-val)
    
    (add-watch ref :flow-storm
               (fn [_ _ old-value new-value] 
                 (let [patch (-> (edit.core/diff (rm-ignored-keys old-value)
                                                 (rm-ignored-keys new-value))
                                 edit.edit/get-edits)]
                   (when (seq patch)
                     (ref-trace ref-id patch)))))))

(defn untrace-ref [ref]
  (remove-watch ref :flow-storm))

(defn trace-tap [tap-id tap-name v]
  (let [trace-data {:tap-id tap-id
                    :tap-name tap-name
                    :value (serialize-val v)
                    :timestamp (get-timestamp)}]
    (ws-send [:flow-storm/tap-trace trace-data])))

(defn init-tap
  ([] (let [rnd-id (rand-int 100000)] (init-tap rnd-id (str rnd-id))))
  ([tap-name] (init-tap (rand-int 100000) tap-name))
  ([tap-id tap-name]
   ;; we resolve add-tap like this so flow-storm can be used in older versions of clojure
   (when-let [add-tap-fn (resolve 'clojure.core/add-tap)]     
    (add-tap-fn (fn [v]
               (trace-tap tap-id tap-name v))))))

(defn connect
  "Connects to the flow-storm debugger.
  When connection is ready, replies any events hold in `pre-conn-events-holder`"
  ([] (connect nil))
  ([{:keys [host port protocol tap-name]}]
   (let [{:keys [chsk ch-recv send-fn state]} (sente/make-channel-socket-client! "/chsk"
                                                                                 "dummy-csrf-token" ;; to avoid warning
                                                                                 {:type :ws
                                                                                  :protocol (or protocol :http)
                                                                                  :host (or host "localhost")
                                                                                  :port (or port 7722)})]
     
     (init-tap tap-name)
     
     ;; take one event from ch-recv, since we just connected it should be :chsk/state for open
     ;; TODO: improve this. It should be a go-loop handling all events from ch-recv.
     ;; It is assuming that the :chsk/state is the first event, which is error prone
     (take! ch-recv (fn [{:keys [event]}]
                      (when (= (first event) :chsk/state)
                        (let [holded-events @pre-conn-events-holder]
                          (println "Ws connection ready, re playing " (count holded-events) "events")

                          ;; set the websocket send-fn globally so it can be
                          ;; used by the tracers
                          (reset! send-fn-a send-fn)

                          ;; replay all events we have on hold
                          (doseq [ev holded-events]
                            (send-fn ev))

                          ;; empty the events holder atom
                          (reset! pre-conn-events-holder []))))))))
