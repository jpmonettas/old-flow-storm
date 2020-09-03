(ns tracex.instrument
  (:require [cider.nrepl.middleware.util.instrument :as inst]))


(defmacro trace [form]
  `(binding [tracex.tracer/*form-id* ~(hash form)]
     (tracex.tracer/init-trace tracex.tracer/*form-id* (quote ~form))
     ~(-> form
          (inst/tag-form-recursively 'tracex.tracer/add-trace)
          (inst/instrument-tagged-code))))

(comment

  (require '[tracex.tracer :refer [connect]])
  (trace (let [a (+ 1 2)
               b (+ a a)]
           (map inc (range 10))))

  )
