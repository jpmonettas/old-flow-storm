(ns flow-storm.debugger.main
  (:require [flow-storm.debugger.ui.main :as ui-main]
            [flow-storm.debugger.state :as dbg-state]))

(defn start-debugger []
  (dbg-state/init-state!)
  (ui-main/start-ui))
