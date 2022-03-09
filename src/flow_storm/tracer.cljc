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

(defonce *trace-fn (atom nil))

(def ^:dynamic *print-length* nil)
(def ^:dynamic *print-level* nil)
(def ^:dynamic *flow-id* nil)
(def ^:dynamic *init-traced-forms* nil)

(defn get-timestamp []
  #?(:cljs (.getTime (js/Date.))
     :clj (System/currentTimeMillis)))

(defn serialize-val [v]    
  (if (or (= (type v) clojure.lang.LazySeq)
          (= (type v) clojure.lang.Cons))

    ;;@@@ HACK. TODO: figure this out, when trying to pr-str of some values
    ;; makes the JVM StackOverflowError
    ;; One example is cljs.analyzer:4334 inside forms-seq* fn
    "LAZY" 

    (try
      (binding [clojure.core/*print-length* (or *print-length* 50)
                clojure.core/*print-level* (or *print-level* 5)]
        (pr-str v))
      (catch Exception e      
        (println "Can't serialize this, skipping " (type v))
        ""))))

(defn traceit
  "Send the event thru the connected websocket."
  [[ttype m :as t]]
  (try
    (@*trace-fn t)
    (catch Exception e
      (println "WARN Couldn't trace" t))))

(defn init-trace
  "Instrumentation function. Sends the `:init-trace` trace"
  [{:keys [form-id args-vec fn-name ns]} form]
  (when-not (contains? @*init-traced-forms* [*flow-id* form-id])
    (let [trace-data (cond-> {:flow-id *flow-id*
                              :form-id form-id
                              :form form
                              :ns ns
                              :timestamp (get-timestamp)})]
      (traceit [:init-trace trace-data])
      (swap! *init-traced-forms* conj [*flow-id* form-id]))))

(defn expr-exec-trace
  "Instrumentation function. Sends the `:exec-trace` trace and returns the result."
  [result err {:keys [coor outer-form? form-id]}]
  (let [trace-data (cond-> {:flow-id *flow-id*
                            :form-id form-id
                            :coor coor
                            :thread-id (.getId (Thread/currentThread))
                            :timestamp (get-timestamp)
                            :result (serialize-val result)}                     
                     outer-form? (assoc :outer-form? true))]

    (traceit [:exec-trace trace-data])

    result))

(defn fn-call-trace [form-id ns fn-name args-vec]  
  (traceit [:fn-call-trace {:flow-id *flow-id*
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
    (traceit [:bind-trace trace-data])))

(defn ref-init-trace
  "Sends the `:ref-init-trace` trace"
  [ref-id ref-name init-val]
  (let [trace-data {:ref-id ref-id
                    :ref-name ref-name
                    :init-val (serialize-val init-val)
                    :timestamp (get-timestamp)}]
    (traceit [:ref-init-trace trace-data])))

(defn ref-trace
  "Sends the `:ref-trace` trace"
  [ref-id patch]
  (let [trace-data {:ref-id ref-id
                    :patch (pr-str patch)
                    :timestamp (get-timestamp)}]
    (traceit [:ref-trace trace-data])))

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
    (traceit [:tap-trace trace-data])))

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
  ([{:keys [host port protocol tap-name to-file]}]
   (let [wsc (proxy
                 [WebSocketClient]
                 [(URI. "ws://localhost:7722/ws")]
               (onOpen [^ServerHandshake handshake-data]
                 (println "Connection opened"))
               (onMessage [^String message])
               (onClose [code reason remote?]
                 (println "Connection closed" [code reason remote?]))
               (onError [^Exception e]
                 (println "WS ERROR" e)))
         trace-queue (ArrayBlockingQueue. 20000000)
         *consumer-stats (atom {:cnt 0 :last-report-t (System/nanoTime) :last-report-cnt 0})
         send-thread (Thread.
                      (fn []                        
                        (let [file-dos (when to-file (DataOutputStream. (FileOutputStream. to-file)))
                              bos (ByteArrayOutputStream.)
                              ;; file-dos (when to-file (DataOutputStream. bos))
                              ]
                          (while true                         
                           (let [trace (.take trace-queue)                                
                                 qsize (.size trace-queue)]

                             ;; Consumer stats
                             (let [{:keys [cnt last-report-t last-report-cnt]} @*consumer-stats]
                               (when (zero? (mod cnt 100000))                                 
                                 (println (format "CNT: %d, Q_SIZE: %d, Speed: %.1f tps BS: %d"
                                                  cnt
                                                  qsize
                                                  (quot (- cnt last-report-cnt)
                                                        (/ (double (- (System/nanoTime) last-report-t))
                                                           1000000000.0))
                                                  (.size bos)))
                                 (swap! *consumer-stats
                                        assoc
                                        :last-report-t (System/nanoTime)
                                        :last-report-cnt cnt))
                               
                               (swap! *consumer-stats update :cnt inc))
                             
                             
                             
                             (if to-file

                               (bin-serializer/serialize-trace file-dos trace)
                               
                               
                               ;; else
                               (let [trace-json-str (json/write-value-as-string trace)]                                                                  
                                 (.send wsc trace-json-str))))))))]
     
     (reset! *trace-fn (fn [trace]
                         (.put trace-queue trace)))
     
     (when-not to-file
       (.setConnectionLostTimeout wsc 0)
       (.connect wsc))
     
     (.start send-thread)
     
     nil)))
