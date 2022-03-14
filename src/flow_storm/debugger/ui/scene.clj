(ns flow-storm.debugger.ui.scene)

(defonce scene nil)

(defn lookup [selector]
  (.lookup scene selector))
