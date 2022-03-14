(ns flow-storm.debugger.state
  (:require [clojure.spec.alpha :as s]))

(def *state (atom nil))

(defn init-state! []
  (reset! *state {:trace-counter 0}))
