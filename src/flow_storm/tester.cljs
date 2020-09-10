(ns flow-storm.tester
  (:require [clojure.pprint :as pp]
            [flow-storm.tracer :refer [connect]])
  (:require-macros [flow-storm.instrument :refer [trace]]))

;; Run with
;; clj -m cljs.main --target node --output-to main.js -c flow-storm.tester && node main.js

#_(pp/pprint (macroexpand-1 '(trace (let [a 5]
                                      (+ a 2)))))

(connect)

(trace (let [a 5]
         (+ a 2)))
