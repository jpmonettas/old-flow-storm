(ns flow-storm.tester
  (:require [clojure.pprint :as pp])
  (:require-macros [flow-storm.instrument :refer [trace]]))

;; Run with
;; clj -m cljs.main --target node --output-to main.js -c flow-storm.tester && node main.js

(pp/pprint (macroexpand-1 '(trace (let [a 5]
                                    (+ a 2)))))
