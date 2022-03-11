(ns flow-storm.utils)

(defn colored-string [s c]
  (let [color {:red 31
               :yellow 33}]
    (format "\033[%d;1;1m%s\033[0m" (color c) s)))
