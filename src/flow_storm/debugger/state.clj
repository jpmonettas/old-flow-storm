(ns flow-storm.debugger.state)

(def *state (atom nil))

(defn init-state! []
  (reset! *state {:trace-counter 0}))
