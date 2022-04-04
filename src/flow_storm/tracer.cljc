(ns flow-storm.tracer
  (:require [editscript.core :as edit.core]
            [editscript.edit :as edit.edit]
            [cognitect.transit :as transit]
            [jsonista.core :as json]
            [flow-storm.binary-serializer :as bin-serializer])
  (:import [java.net URI]
           [org.java_websocket.client WebSocketClient]
           [org.java_websocket.handshake ServerHandshake]
           [java.util.concurrent ArrayBlockingQueue]
           [java.io FileOutputStream ByteArrayOutputStream DataOutputStream]))

(def ^ArrayBlockingQueue trace-queue nil)
(defonce ^Thread send-thread nil)

(def ^:dynamic *flow-id* nil)
(def ^:dynamic *init-traced-forms* nil)

(defrecord FlowInitTrace [flow-id form-ns form timestamp])
(defrecord FormInitTrace [flow-id form-id thread-id form ns def-kind mm-dispatch-val timestamp])
(defrecord ExecTrace [flow-id form-id coor thread-id result outer-form?])
(defrecord FnCallTrace [flow-id form-id fn-name fn-ns thread-id args-vec timestamp])
(defrecord BindTrace [flow-id form-id coor thread-id timestamp symbol value])

(defn get-timestamp []
  #?(:cljs (.getTime (js/Date.))
     :clj (System/currentTimeMillis)))

#_(def ^:dynamic *print-length* nil)
#_(def ^:dynamic *print-level* nil)
#_(defn serialize-val [v]
  (try
    (binding [clojure.core/*print-length* (or *print-length* 50)
              clojure.core/*print-level* (or *print-level* 5)]
      (pr-str v))
    (catch Exception e      
      (println "Warning: can't serialize this, skipping " (type v))
      "ERROR_SERIALIZING")))

(defn trace-flow-init-trace [flow-id form-ns form]
  (let [trace (map->FlowInitTrace {:flow-id flow-id
                                   :form-ns form-ns
                                   :form form
                                   :timestamp (get-timestamp)})]
    (.put trace-queue trace)))

(defn trace-form-init-trace
  "Instrumentation function. Sends the `:init-trace` trace"
  [{:keys [form-id args-vec ns def-kind dispatch-val] :as t} form]  
  (let [thread-id (.getId (Thread/currentThread))]
    (when-not (contains? @*init-traced-forms* [*flow-id* thread-id form-id])
      (let [trace (map->FormInitTrace {:flow-id *flow-id*
                                       :form-id form-id
                                       :thread-id thread-id
                                       :form form
                                       :ns ns
                                       :def-kind def-kind
                                       :mm-dispatch-val dispatch-val
                                       :timestamp (get-timestamp)})]
        (.put trace-queue trace)
        (swap! *init-traced-forms* conj [*flow-id* thread-id form-id])))))

(defn trace-expr-exec-trace
  "Instrumentation function. Sends the `:exec-trace` trace and returns the result."
  [result err {:keys [coor outer-form? form-id]}]
  (let [trace (map->ExecTrace {:flow-id *flow-id*
                               :form-id form-id
                               :coor coor
                               :thread-id (.getId (Thread/currentThread))
                               :timestamp (get-timestamp)
                               :result result
                               :outer-form? outer-form?})]
    (.put trace-queue trace)
    result))

(defn trace-fn-call-trace [form-id ns fn-name args-vec]
  (let [trace (map->FnCallTrace {:flow-id *flow-id*
                                 :form-id form-id
                                 :fn-name fn-name
                                 :fn-ns ns
                                 :thread-id (.getId (Thread/currentThread))
                                 :args-vec args-vec
                                 :timestamp (get-timestamp)})]
    (.put trace-queue trace)))

(defn trace-bound-trace
  "Instrumentation function. Sends the `:bind-trace` trace"
  [symb val {:keys [coor form-id]}]
  (let [trace (map->BindTrace {:flow-id *flow-id*
                               :form-id form-id
                               :coor (or coor [])
                               :thread-id (.getId (Thread/currentThread))
                               :timestamp (get-timestamp)
                               :symbol (name symb)
                               :value val})]
    (.put trace-queue trace)))

;; (defn ref-init-trace
;;   "Sends the `:ref-init-trace` trace"
;;   [ref-id ref-name init-val]
;;   (let [trace-data {:ref-id ref-id
;;                     :ref-name ref-name
;;                     :init-val (serialize-val init-val)
;;                     :timestamp (get-timestamp)}]
;;     (traceit [:ref-init-trace trace-data])))

;; (defn ref-trace
;;   "Sends the `:ref-trace` trace"
;;   [ref-id patch]
;;   (let [trace-data {:ref-id ref-id
;;                     :patch (pr-str patch)
;;                     :timestamp (get-timestamp)}]
;;     (traceit [:ref-trace trace-data])))

;; (defn trace-ref [ref {:keys [ref-name ignore-keys]}]
;;   (let [ref-id (hash ref)
;;         rm-ignored-keys (fn [v]
;;                           (if (and (seq ignore-keys) (map? v))
;;                             (apply (partial dissoc v) ignore-keys)
;;                             v))
;;         ref-init-val (-> @ref
;;                          rm-ignored-keys)]

;;     (ref-init-trace ref-id ref-name ref-init-val)

;;     (add-watch ref :flow-storm
;;                (fn [_ _ old-value new-value]
;;                  (let [patch (-> (edit.core/diff (rm-ignored-keys old-value)
;;                                                  (rm-ignored-keys new-value))
;;                                  edit.edit/get-edits)]
;;                    (when (seq patch)
;;                      (ref-trace ref-id patch)))))))

;; (defn untrace-ref [ref]
;;   (remove-watch ref :flow-storm))

;; (defn trace-tap [tap-id tap-name v]
;;   (let [trace-data {:tap-id tap-id
;;                     :tap-name tap-name
;;                     :value (serialize-val v)
;;                     :timestamp (get-timestamp)}]
;;     (traceit [:tap-trace trace-data])))

;; (defn init-tap
;;   ([] (let [rnd-id (rand-int 100000)] (init-tap rnd-id (str rnd-id))))
;;   ([tap-name] (init-tap (rand-int 100000) tap-name))
;;   ([tap-id tap-name]
;;    ;; we resolve add-tap like this so flow-storm can be used in older versions of clojure
;;    (when-let [add-tap-fn (resolve 'clojure.core/add-tap)]
;;     (add-tap-fn (fn [v]
;;                   (trace-tap tap-id tap-name v))))))

(defn build-ws-sender [{:keys [host port]}]
  (let [wsc (proxy
                [WebSocketClient]
                [(URI. "ws://localhost:7722/ws")]
              (onOpen [^ServerHandshake handshake-data]
                (println "Connection opened"))
              (onMessage [^String message])
              (onClose [code reason remote?]
                (println "Connection closed" [code reason remote?]))
              (onError [^Exception e]
                (println "WS ERROR" e)))]

    (.setConnectionLostTimeout wsc 0)
    (.connect wsc)

    {:send-fn (fn [trace]
                (let [trace-json-str (json/write-value-as-string trace)]                                                                  
                  (.send wsc trace-json-str)))
     :ws-client wsc}))

(defn build-file-sender [{:keys [file-path]}]
  (let [file-dos (DataOutputStream. (FileOutputStream. ^String file-path))
        ;; _bos (ByteArrayOutputStream.)
        ;; file-dos (when to-file (DataOutputStream. bos))
        ]
    {:send-fn (fn [trace]
                (bin-serializer/serialize-trace file-dos trace))
     :file-output-stream file-dos}))


(defn connect
  "Connects to the flow-storm debugger. `"
  ([] (connect nil))
  ([{:keys [tap-name send-fn]}]

   ;; Stop the previous running `send-thread` if we have one
   ;; since we could be trying to reconnect
   (when send-thread (.stop send-thread))
   
   (let [ _ (alter-var-root #'trace-queue (constantly (ArrayBlockingQueue. 30000000)))
         *consumer-stats (atom {:cnt 0 :last-report-t (System/nanoTime) :last-report-cnt 0})
         
         send-thread (Thread.
                      (fn []
                        
                        (while (not (.isInterrupted (Thread/currentThread)))                          
                          (try
                            (let [trace (.take trace-queue)                                
                                  qsize (.size trace-queue)]

                              ;; Consumer stats
                              (let [{:keys [cnt last-report-t last-report-cnt]} @*consumer-stats]
                                (when (zero? (mod cnt 100000))                                 
                                  (tap> (format "CNT: %d, Q_SIZE: %d, Speed: %.1f tps"
                                                cnt
                                                qsize
                                                (quot (- cnt last-report-cnt)
                                                      (/ (double (- (System/nanoTime) last-report-t))
                                                         1000000000.0))))
                                  (swap! *consumer-stats
                                         assoc
                                         :last-report-t (System/nanoTime)
                                         :last-report-cnt cnt))
                                
                                (swap! *consumer-stats update :cnt inc))
                              
                              (send-fn trace))
                            (catch java.lang.InterruptedException ie nil)
                            (catch java.lang.IllegalMonitorStateException imse nil)
                            (catch Exception e
                              (tap> "SendThread consumer exception")
                              (tap> e))))
                        (tap> "Thread interrupted. Dying...")))]
     (alter-var-root #'send-thread (constantly send-thread))
     (.start send-thread)
     
     nil)))


(defn stop-send-thread []
  (when send-thread (.interrupt send-thread)))
