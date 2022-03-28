(ns dev-tester)

;;;;;;;;;;;;;;;;;;;;;;;
;; Some testing code ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defn factorial [n]
  (if (zero? n)
    1
    (* n (factorial (dec n)))))

(defn boo [xs]
  (reduce + (map factorial xs)))
