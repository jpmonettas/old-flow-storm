(ns flow-storm.debugger.ui.state-vars)

(defonce main-pane nil)
(defonce ctx-menu nil)
(defonce stage nil)
(defonce scene nil)

(defn scene-lookup [selector]
  (.lookup scene selector))
