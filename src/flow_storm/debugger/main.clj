(ns flow-storm.debugger.main
  (:require [flow-storm.debugger.ui.main :as ui-main]
            [flow-storm.debugger.ui.state-vars :as ui-state-vars]
            [flow-storm.debugger.state :as dbg-state]))

(defn start-debugger []
  (dbg-state/init-state!)
  (ui-state-vars/reset-state!)
  (ui-main/start-ui))
