(ns tracex.client
  (:require [reagent.core :as r]
            [clojure.core.async :refer [go-loop] :as async]
            [taoensso.sente  :as sente]
            [tracex.highlighter :refer [highlight-expr]]
            #_[tracex.tracer :as tracer]
            )
  #_(:require-macros [tracex.instrument :refer [t]]))

(defonce state (r/atom {:form-id nil
                        :form nil
                        :trace []
                        :trace-idx 0}))

(defn main-screen []
  (let [{:keys [form trace trace-idx]} @state
        coor (:coor (get trace trace-idx))]
    [:div
     [:div
      [:button {:on-click #(swap! state update :trace-idx dec)}"<"]
      [:button {:on-click #(swap! state update :trace-idx inc)}">"]]
     [:div (str ">>" coor)]
     [:div (str ">>" (:result (get trace trace-idx)))]
     [:pre (str form)]
     [:pre {:dangerouslySetInnerHTML {:__html (highlight-expr (str form) coor "<b>" "</b>")}} ]
     [:div (str trace)]]))

(defn ^:dev/after-load mount-component []
  (r/render [main-screen] (.getElementById js/document "app")))

(defn handle-ws-message [{:keys [event]}]
  (let [[_ evt] event]
    (let [[e-key e-data-map] evt]
      (case e-key
        :tracex/add-trace  (swap! state update :trace (fn [t] (conj t (select-keys e-data-map [:coor :result]))))
        :tracex/init-trace (swap! state assoc
                                  :form-id (:traced-form-id e-data-map)
                                  :form (:form e-data-map)))
     (println "Got event " evt))
    ))

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
