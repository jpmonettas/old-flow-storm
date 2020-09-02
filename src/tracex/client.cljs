(ns tracex.client
  (:require [reagent.core :as r]
            [clojure.core.async :refer [go-loop] :as async]
            [taoensso.sente  :as sente]))

(defn main-screen []
  [:div "It works now!!!"])

(defn ^:dev/after-load mount-component []
  (r/render [main-screen] (.getElementById js/document "app")))

(defn handle-ws-message [{:keys [event]}]
  (let [[_ evt] event]
    (println "Got event " evt)))

(defn  main []
  (mount-component)
  (let [?csrf-token (when-let [el (.getElementById js/document "sente-csrf-token")]
          (.getAttribute el "data-csrf-token"))
        {:keys [chsk ch-recv send-fn state]}
        (sente/make-channel-socket-client!
         "/chsk" ; Note the same path as before
         ?csrf-token
         {:type :auto
          :client-id "browser"
          :host "localhost"
          :port 8080})]
    (go-loop []
        (try
          (handle-ws-message (async/<! ch-recv))
          (catch js/Error e
            (js/console.error "Error handling ws message")))
      (recur))))
