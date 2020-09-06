(ns tracex.tracer
  (:require  [taoensso.sente  :as sente]))

(defonce send-fn-a (atom nil))

(def ^:dynamic *form-id* nil)

(defn init-trace [traced-form-id form]
  (let [send-fn (or @send-fn-a println)]
   (send-fn [:tracex/init-trace {:traced-form-id traced-form-id :form form}])))

(defn add-trace [result {:keys [coor] :as extras}]
  (let [send-fn (or @send-fn-a println)]
    (send-fn [:tracex/add-trace {:traced-form-id *form-id* :coor coor :result (pr-str result)}])
    result))

(defn connect []
  (let [{:keys [chsk ch-recv send-fn state]} (sente/make-channel-socket-client! "/chsk"  nil {:type :ws
                                                                                              :client-id "tracer"
                                                                                              :host "localhost"
                                                                                              :port 8080})]
    (reset! send-fn-a send-fn)))
