(ns user)

(in-ns 'dev)

(clojure.core/defn ui-refresh []
  ((clojure.core/resolve 'flow-storm.debugger.ui.main/refresh-ui)))
