(ns flow-storm.tracer
  (:require [editscript.core :as edit.core]
            [editscript.edit :as edit.edit]
            [cognitect.transit :as transit]
            [jsonista.core :as json])
  (:import [java.net URI]
           [org.java_websocket.client WebSocketClient]
           [org.java_websocket.handshake ServerHandshake]))

(defonce send-fn-a (atom nil))
(defonce pre-conn-events-holder (atom []))

(defonce per-form-traces (atom {}))

(def ^:dynamic *flow-id* nil)
(def ^:dynamic *init-traced-forms* #{})

(defn get-timestamp []
  #?(:cljs (.getTime (js/Date.))
     :clj (System/currentTimeMillis)))

(defn serialize-val [v]  
  (try
    (binding [*print-length* (or *print-length* 50)]
      (pr-str v))
    (catch Exception e      
      (println "Can't serialize this, skipping " (type v))
      "")))

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
  [[_ {:keys [form-id]} :as event]]
  ((or @send-fn-a hold-event) event)
  #_(if (or (nil? form-id)
            (<= (get @per-form-traces form-id 0) 50))
    (do 
      (when form-id (swap! per-form-traces update form-id (fnil inc 0)))
      
      ((or @send-fn-a hold-event) event))

    (println "WARN hit trace limit for form id " form-id ". No more forms are going to be traced for it.")))

(defn init-trace
  "Instrumentation function. Sends the `:init-trace` trace"
  [{:keys [form-id flow-id args-vec fn-name ns]} form]
  (when-not (contains? *init-traced-forms* [flow-id form-id])
    (let [trace-data (cond-> {:flow-id *flow-id*
                              :form-id form-id
                              :form (pr-str form)
                              :ns ns
                              :timestamp (get-timestamp)}
                       flow-id  (assoc :fixed-flow-id-starter? true))]
      (ws-send [:init-trace trace-data]))))

(defn expr-exec-trace
  "Instrumentation function. Sends the `:exec-trace` trace and returns the result."
  [result err {:keys [coor outer-form? form-id]} orig-form]
  (let [trace-data (cond-> {:flow-id *flow-id*
                            :form-id form-id
                            :coor coor
                            :thread-id (.getId (Thread/currentThread))
                            :timestamp (get-timestamp)}
                     (not err)   (assoc :result (serialize-val result))
                     err         (assoc :err {:error/message (:message err)})
                     outer-form? (assoc :outer-form? true))]

    (ws-send [:exec-trace trace-data])

    result))

(defn fn-call-trace [form-id ns fn-name args-vec]
  (ws-send [:fn-call-trace {:flow-id *flow-id*
                            :form-id form-id
                            :fn-name fn-name
                            :fn-ns ns
                            :thread-id (.getId (Thread/currentThread))
                            :args-vec (serialize-val args-vec)
                            :timestamp (get-timestamp)}]))

(defn bound-trace
  "Instrumentation function. Sends the `:bind-trace` trace"
  [symb val {:keys [coor form-id]}]
  (let [trace-data {:flow-id *flow-id*
                    :form-id form-id
                    :coor (or coor [])
                    :thread-id (.getId (Thread/currentThread))
                    :timestamp (get-timestamp)
                    :symbol (name symb)
                    :value (serialize-val val)}]
    (ws-send [:bind-trace trace-data])))

(defn ref-init-trace
  "Sends the `:ref-init-trace` trace"
  [ref-id ref-name init-val]
  (let [trace-data {:ref-id ref-id
                    :ref-name ref-name
                    :init-val (serialize-val init-val)
                    :timestamp (get-timestamp)}]
    (ws-send [:ref-init-trace trace-data])))

(defn ref-trace
  "Sends the `:ref-trace` trace"
  [ref-id patch]
  (let [trace-data {:ref-id ref-id
                    :patch (pr-str patch)
                    :timestamp (get-timestamp)}]
    (ws-send [:ref-trace trace-data])))

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
    (ws-send [:tap-trace trace-data])))

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
   (let [wsc (proxy
                 [WebSocketClient]
                 [(URI. "ws://localhost:7722/ws")]
               (onOpen [^ServerHandshake handshake-data]
                 (println "Connection opened"))
               (onMessage [^String message])
               (onClose [code reason remote?]
                 (println "Connection closed"))
               (onError [^Exception e]
                 (println "WS ERROR" e)))]
     (.connect wsc)
     (reset! send-fn-a (fn [event]
                         (.send wsc (json/write-value-as-string event)))))))

#_(defn connect
  "Connects to the flow-storm debugger.
  When connection is ready, replies any events hold in `pre-conn-events-holder`"
  ([] (connect nil))
  ([{:keys [host port protocol tap-name]}]

   ;; don't connect if we already have a connection
   ;; this is so connect can be called multiple times, usefull in hot reload context
   ;; when the init function in called again without restarting everything
   (when-not @send-fn-a
    (let [{:keys [chsk ch-recv send-fn state]} (sente/make-channel-socket-client! "/ws"
                                                                                  "dummy-csrf-token" ;; to avoid warning
                                                                                  {:type :ws
                                                                                   :packer (sente-transit/->TransitPacker :json {} {})
                                                                                   :protocol (or protocol :http)
                                                                                   :host (or host "localhost")
                                                                                   :port (or port 7722)})
          batch-timespan 500
          ev-ch (async/chan 1000000)
          _ (async/go-loop [batch []
                            last-send (get-timestamp)]

              (let [timeout-ch (async/timeout batch-timespan)
                    [v p] (async/alts! [ev-ch timeout-ch]) ;; block until we have a event or a timeout
                    ts (get-timestamp)]

                (if (and (or (> (- ts last-send) batch-timespan)
                             (= p timeout-ch))
                         (seq batch))

                  ;; if we exceeded the batch time span, or we have a timeout
                  ;; and the batch is not empty
                  ;; send the batch and start a new one
                  (do
                    (send-fn [:flow-storm/batch batch])
                    (recur [] ts))

                  ;; else, if it wasn't a timeout accumulate the event in the batch
                  (if (not= p timeout-ch)
                    (recur (conj batch v) last-send)
                    (recur batch last-send)))))

          batch-send-fn (fn [e] (async/put! ev-ch e))]

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
                           (reset! send-fn-a batch-send-fn)

                           ;; replay all events we have on hold
                           (doseq [ev holded-events]
                             (batch-send-fn ev))

                           ;; empty the events holder atom
                           (reset! pre-conn-events-holder [])))))))))
