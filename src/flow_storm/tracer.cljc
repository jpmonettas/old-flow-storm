(ns flow-storm.tracer
  (:require  [taoensso.sente  :as sente]))

(defonce send-fn-a (atom nil))

(def ^:dynamic *form-id* nil)

(defn init-trace [traced-form-id form]
  (let [send-fn (or @send-fn-a println)]
   (send-fn [:flow-storm/init-trace {:traced-form-id traced-form-id :form form}])))

(defn add-trace [& args]
  (if (= (count args) 3) ;; TODO: remove if we don't see this anymore
    (println "WARNING !!!!! Someone called add trace with 3 args" args)

    (let [[result {:keys [coor] :as extras}] args
          send-fn (or @send-fn-a println)]

      (send-fn [:flow-storm/add-trace {:traced-form-id *form-id* :coor coor :result (pr-str result)}])
      result)))

(defn connect
  ([] (connect nil))
  ([{:keys [host port]}]
   (let [{:keys [chsk ch-recv send-fn state]} (sente/make-channel-socket-client! "/chsk"  nil {:type :ws
                                                                                               :client-id "tracer"
                                                                                               :host (or host "localhost")
                                                                                               :port (or port 8080)})]
     (reset! send-fn-a send-fn))))
